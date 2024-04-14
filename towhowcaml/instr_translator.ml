open! Core
module T_Util = Util
open Radatnet
open Mir
module B = Builder

type state = {
  mutable compare_args : (Instr.Ref.t * X86reg.gpr_type) list;
  mutable compare_instr : X86_instr.t;
  mutable input_condition_instr : X86_instr.t;
  mutable changed_flags : Status_flags.t;
  mutable fpu_status_word_args : Instr.Ref.t list;
  mutable fpu_status_word_instr : X86_instr.t;
  mutable float_compare_args : Instr.Ref.t list;
  mutable float_compare_instr : X86_instr.t;
  mutable backward_direction : bool;
  mutable fpu_stack_pointer : Instr.Ref.t;
  mutable fpu_stack_pointer_offset : int;
  mutable fpu_status_word : Instr.Ref.t;
}
[@@deriving sexp_of]

let initial_state () =
  {
    compare_args = [];
    compare_instr = INVALID;
    input_condition_instr = INVALID;
    changed_flags = Status_flags.none;
    fpu_status_word_args = [];
    fpu_status_word_instr = INVALID;
    float_compare_args = [];
    float_compare_instr = INVALID;
    backward_direction = false;
    fpu_stack_pointer = Instr.Ref.invalid;
    fpu_stack_pointer_offset = 0;
    fpu_status_word = Instr.Ref.invalid;
  }

type context = {
  opcode : Radatnet.Types.opcode;
  state : state;
  builder : Mir.Builder.t;
  intrinsics : (int, Util.intrinsic) Hashtbl.t;
}
[@@deriving sexp_of]

let flags_used_tbl =
  Hashtbl.of_alist_exn
    (module X86_instr)
    Status_flags.
      [
        (X86_instr.JG, zero %| sign %| overflow);
        (X86_instr.SETG, zero %| sign %| overflow);
        (X86_instr.JGE, sign %| overflow);
        (X86_instr.SETGE, sign %| overflow);
        (X86_instr.JL, sign %| overflow);
        (X86_instr.SETL, sign %| overflow);
        (X86_instr.JLE, zero %| sign %| overflow);
        (X86_instr.SETLE, zero %| sign %| overflow);
        (X86_instr.JE, zero);
        (X86_instr.SETE, zero);
        (X86_instr.JNE, zero);
        (X86_instr.SETNE, zero);
        (X86_instr.JA, carry %| overflow);
        (X86_instr.SETA, carry %| overflow);
        (X86_instr.JAE, zero %| carry %| overflow);
        (X86_instr.SETAE, zero %| carry %| overflow);
        (X86_instr.JB, carry);
        (X86_instr.SETB, carry);
        (X86_instr.JBE, zero %| carry);
        (X86_instr.SETBE, zero %| carry);
        (X86_instr.JP, parity);
        (X86_instr.JNP, parity);
        (X86_instr.SETNP, parity);
        (X86_instr.JNS, sign);
        (X86_instr.JS, sign);
        (X86_instr.SBB, carry);
        (X86_instr.ADC, carry);
        (X86_instr.JECXZ, none);
      ]

let flags_used_exn instr = Hashtbl.find_exn flags_used_tbl instr
let raise_c context msg = raise_s [%message msg (context : context)]

let raise_m c msg =
  let c_sexp = Sexp.[ List [ Atom "context"; sexp_of_context c ] ] in
  let msg =
    match msg with
    | Sexp.Atom _ -> Sexp.List (msg :: c_sexp)
    | Sexp.List ls -> Sexp.List (ls @ c_sexp)
  in
  raise_s msg

let raise_ops c = raise_c c "Invalid operands"
let assert_c c ?(msg = "Assert failed") b = if not b then raise_c c msg
let operands c = c.opcode.opex.operands
let newest_var c reg = X86reg.to_ident reg |> B.newest_var c.builder

let reg_type_of_size = function
  | 4 -> `Reg32Bit
  | 2 -> `Reg16Bit
  | 1 -> `RegLow8Bit
  | _ -> failwith "invalid size"

let loaded_reg_type operand = operand_size operand |> reg_type_of_size

let load_partial_address c mem_operand =
  let addr_var =
    Option.map mem_operand.index ~f:(fun i ->
        let i_reg = X86reg.to_ident i |> B.newest_var c.builder in
        if mem_operand.scale <> 1 then
          B.mul c.builder ~lhs:i_reg ~rhs:(B.const c.builder mem_operand.scale)
        else i_reg)
  in
  let addr_var =
    match mem_operand.base with
    | Some b ->
        let b_reg = X86reg.to_ident b |> B.newest_var c.builder in
        Some
          (Option.value_map addr_var ~default:b_reg ~f:(fun a ->
               B.add c.builder ~lhs:a ~rhs:b_reg))
    | _ -> addr_var
  in
  match addr_var with
  | Some addr -> (addr, mem_operand.displacement)
  | None -> (B.const c.builder mem_operand.displacement, 0)

let load_complete_address c mem_operand =
  let addr, offset = load_partial_address c mem_operand in
  if offset = 0 then B.add c.builder ~lhs:addr ~rhs:(B.const c.builder offset)
  else addr

let store_operand c src ~dest =
  match dest with
  | Immediate _ ->
      raise_m c
        [%message "Cannot store into immediate operand" (dest : operand)]
  | Register { reg = #X86reg.general_purpose as reg; _ } ->
      (let reg_ident = X86reg.to_32_bit reg |> X86reg.to_ident
       and b = c.builder in
       match reg with
       | #X86reg.reg_32bit -> B.dup_var b ~varName:reg_ident src Int
       | #X86reg.reg_16bit ->
           B.merge_trunc_16 b ~varName:reg_ident ~lhs:src
             ~rhs:(B.newest_var b reg_ident)
       | #X86reg.reg_high8bit ->
           B.merge_trunc_high8 b ~varName:reg_ident ~lhs:src
             ~rhs:(B.newest_var b reg_ident)
       | #X86reg.reg_low8bit ->
           B.merge_trunc_low8 b ~varName:reg_ident ~lhs:src
             ~rhs:(B.newest_var b reg_ident))
      |> ignore
  | Memory ({ segment = None | Some (Es | Fs); _ } as address) -> (
      let addr_var, offset = load_partial_address c address in
      let addr_var =
        if Option.value_map address.segment ~default:false ~f:(Poly.equal Fs)
        then raise_c c "fs segment"
        else addr_var
      in
      match address.size with
      | 4 -> B.store32 c.builder ~value:src ~addr:addr_var ~offset
      | 2 -> B.store16 c.builder ~value:src ~addr:addr_var ~offset
      | 1 -> B.store8 c.builder ~value:src ~addr:addr_var ~offset
      | _ -> raise_m c [%message "Invalid store size" (dest : operand)])
  | _ -> raise_m c [%message "Cannot store into operand" (dest : operand)]

(* Not guarenteed to be zero nor sign extended *)
let load_operand_typed c src : Instr.ref * X86reg.gpr_type =
  match src with
  | Immediate { value; size } ->
      assert_c c (match size with 1 | 2 | 4 -> true | _ -> false);
      (* Assembly only allows constants of the proper size *)
      (B.const c.builder value, `Reg32Bit)
  | Register { reg; _ } ->
      let r_type =
        match X86reg.reg_type reg with
        | `X87Float | `Mmx | `Sse ->
            raise_m c [%message "can't load operand" (src : operand)]
        | #X86reg.gpr_type as t -> t
      in
      (X86reg.to_ident reg |> B.newest_var c.builder, r_type)
  | Memory ({ segment = None; _ } as address) -> (
      let addr_var, offset = load_partial_address c address in
      match address.size with
      | 4 -> (B.load32 c.builder addr_var ~offset, `Reg32Bit)
      | 2 -> (B.load16 c.builder addr_var ~offset ~signed:false, `Reg16Bit)
      | 1 -> (B.load8 c.builder addr_var ~offset ~signed:false, `RegLow8Bit)
      | _ -> raise_m c [%message "can't load operand" (src : operand)])
  | _ -> raise_m c [%message "can't load operand" (src : operand)]

let extend_unsigned c (instr, typ) =
  match typ with
  | `Reg32Bit -> instr
  | `Reg16Bit -> B.zero_extend_16 c.builder ~operand:instr
  | `RegHigh8Bit -> B.zero_extend_high8 c.builder ~operand:instr
  | `RegLow8Bit -> B.zero_extend_low8 c.builder ~operand:instr

let extend_signed c (instr, typ) =
  match typ with
  | `Reg32Bit -> instr
  | `Reg16Bit -> B.sign_extend_16 c.builder ~operand:instr
  | `RegHigh8Bit -> B.sign_extend_high8 c.builder ~operand:instr
  | `RegLow8Bit -> B.sign_extend_low8 c.builder ~operand:instr

let load_operand_s c src = load_operand_typed c src |> extend_signed c
let load_operand_u c src = load_operand_typed c src |> extend_unsigned c

(* signed should be the default because it's one little WASM instruction *)
let load_operand = load_operand_s

let add_comparison c args =
  c.state.compare_instr <- c.opcode.id;
  c.state.compare_args <- args;
  c.state.changed_flags <- Status_flags.all

let add_float_comparison c args =
  c.state.float_compare_instr <- c.opcode.id;
  c.state.float_compare_args <- args

let get_fpu_stack_pointer c =
  (* only should be invalid if never previously used, so fpu_stack_pointer_offset should already be 0 *)
  if Instr.Ref.equal Instr.Ref.invalid c.state.fpu_stack_pointer then
    c.state.fpu_stack_pointer <-
      B.get_global c.builder T_Util.fpu_stack_pointer_global Int;
  c.state.fpu_stack_pointer

let set_fpu_stack c value index =
  let addr = get_fpu_stack_pointer c in
  let offset = (c.state.fpu_stack_pointer_offset - index) * float_size in
  match float_size with
  | 4 -> B.float_store32 c.builder ~addr ~value ~offset
  | 8 -> B.float_store64 c.builder ~addr ~value ~offset
  | _ -> failwith "invalid float size"

let get_fpu_stack c index =
  let addr = get_fpu_stack_pointer c in
  let offset = (c.state.fpu_stack_pointer_offset - index) * float_size in
  match float_size with
  | 4 -> B.float_load32 c.builder addr ~offset
  | 8 -> B.float_load64 c.builder addr ~offset
  | _ -> failwith "invalid float size"

let fpu_push c value =
  c.state.fpu_stack_pointer_offset <- c.state.fpu_stack_pointer_offset + 1;
  set_fpu_stack c value 0

let fpu_pop c =
  let value = get_fpu_stack c 0 in
  c.state.fpu_stack_pointer_offset <- c.state.fpu_stack_pointer_offset - 1;
  value

let load_operand_f c src =
  match src with
  | Immediate { value; size = 4 } ->
      B.float_const c.builder @@ Util.int32_to_float value
  | Register { reg = #X86reg.x87_float as reg; _ } ->
      get_fpu_stack c @@ X86reg.x87_float_reg_index reg
  | Memory ({ segment = None | Some Es; _ } as mem) -> (
      let addr, offset = load_partial_address c mem in
      match mem.size with
      | 4 -> B.float_load32 c.builder addr ~offset
      | 8 -> B.float_load64 c.builder addr ~offset
      | _ -> raise_c c "invalid size")
  | _ -> raise_c c "invalid operand"

let store_operand_f c value ~dest =
  match dest with
  | Register { reg = #X86reg.x87_float as reg; _ } ->
      set_fpu_stack c value @@ X86reg.x87_float_reg_index reg
  | Memory ({ segment = None | Some Es; size = 4 | 8; _ } as mem) -> (
      let addr, offset = load_partial_address c mem in
      match mem.size with
      | 4 -> B.float_store32 c.builder ~value ~addr ~offset
      | 8 -> B.float_store64 c.builder ~value ~addr ~offset
      | _ -> raise_c c "invalid operand")
  | _ -> raise_c c "invalid operand"

let translate_mov c =
  match operands c with
  | [ dest; src ] when operand_size dest = operand_size src ->
      store_operand c (load_operand c src) ~dest
  | _ -> raise_ops c

let translate_mov_zero_extend c =
  match operands c with
  | [ dest; src ] when operand_size dest > operand_size src ->
      store_operand c (load_operand_u c src) ~dest
  | _ -> raise_ops c

let translate_mov_sign_extend c =
  match operands c with
  | [ dest; src ] when operand_size dest > operand_size src ->
      store_operand c (load_operand_s c src) ~dest
  | _ -> raise_ops c

let stack_head_operand =
  {
    size = -1;
    base = Some `esp;
    index = None;
    scale = 1;
    displacement = 0;
    segment = None;
  }

let translate_push c =
  match operands c with
  | [ src ] ->
      let value = load_operand c src in
      let esp = newest_var c `esp in
      let size = operand_size src in
      B.sub c.builder ~varName:(X86reg.to_ident `esp) ~lhs:esp
        ~rhs:(B.const c.builder size)
      |> ignore;
      store_operand c value ~dest:(Memory { stack_head_operand with size })
  | _ -> raise_ops c

let translate_pop c =
  match operands c with
  | [ dest ] ->
      let esp = newest_var c `esp in
      let size = operand_size dest in
      let popped = load_operand c (Memory { stack_head_operand with size }) in
      (* what order should these be? *)
      B.add c.builder ~varName:(X86reg.to_ident `esp) ~lhs:esp
        ~rhs:(B.const c.builder size)
      |> ignore;
      store_operand c popped ~dest
  | _ -> raise_ops c

(* Fine to use as long as, for each bit, the op is determined at most by the bits to the left of it *)
let translate_bi_op c (op : B.bi_op_add) cond =
  match operands c with
  | [ dest; src ] when operand_size src = operand_size dest -> (
      (* originally these were load_typed. why? *)
      let lhs = load_operand c dest in
      let rhs = load_operand c src in
      let res = op c.builder ~lhs ~rhs in
      store_operand c res ~dest;
      let loaded_type = loaded_reg_type dest in
      match cond with
      | `All ->
          add_comparison c
            [ (lhs, loaded_type); (rhs, loaded_type); (res, loaded_type) ]
      | `Result -> add_comparison c [ (res, loaded_type) ]
      | `Args -> add_comparison c [ (lhs, loaded_type); (rhs, loaded_type) ]
      | `None -> add_comparison c [])
  | _ -> raise_ops c

let translate_xor c =
  match operands c with
  | [ dest; src ] when equal_operand dest src ->
      let value = B.const c.builder 0 in
      store_operand c value ~dest;
      add_comparison c []
  | _ -> translate_bi_op c B.xor `Result

let translate_shift_right c ~signed =
  match operands c with
  | [ dest; src ] ->
      let lhs =
        if signed then load_operand_s c dest else load_operand_u c dest
      in
      let rhs = if signed then load_operand_s c src else load_operand_u c src in
      let res = B.shift_right ~signed c.builder ~lhs ~rhs in
      store_operand c res ~dest;
      add_comparison c [ (res, loaded_reg_type dest) ]
  | _ -> raise_ops c

let translate_test_cmp c =
  match operands c with
  | [ lhs; rhs ] ->
      let lhs = load_operand_typed c lhs and rhs = load_operand_typed c rhs in
      add_comparison c [ lhs; rhs ]
  | _ -> raise_ops c

let translate_inc c =
  match operands c with
  | [ arg ] ->
      let lhs = load_operand c arg in
      let res = B.add c.builder ~lhs ~rhs:(B.const c.builder 1) in
      store_operand c res ~dest:arg;
      add_comparison c [ (res, loaded_reg_type arg) ]
  | _ -> raise_ops c

let translate_condition c =
  assert_c c
    (Status_flags.equal c.state.changed_flags @@ flags_used_exn c.opcode.id)
    ~msg:"Flag has been modified";
  B.set_check_var_is_latest c.builder false;
  let condition =
    match c.state.compare_instr with
    | _ -> raise_c c "invalid comparison instruction"
  in
  B.set_check_var_is_latest c.builder true;
  condition

let translate_set_cond c =
  match operands c with
  | [ dest ] -> store_operand c (translate_condition c) ~dest
  | _ -> raise_ops c

let translate_lea c =
  match operands c with
  | [ dest; Memory mem ] -> store_operand c ~dest @@ load_complete_address c mem
  | _ -> raise_ops c

let translate_float_load c =
  match operands c with
  | [ (Memory _ as mem) ] ->
      add_float_comparison c [];
      load_operand_f c mem |> fpu_push c
  | _ -> raise_ops c

let translate_float_comparison c ~after_pop =
  let rhs =
    match operands c with
    | [ operand ] -> load_operand_f c operand
    | [] -> get_fpu_stack c 1
    | _ -> raise_ops c
  in
  let lhs = get_fpu_stack c 0 in
  add_float_comparison c [ lhs; rhs ];
  match after_pop with
  | `None -> ()
  | `Once -> fpu_pop c |> ignore
  | `Twice ->
      fpu_pop c |> ignore;
      fpu_pop |> ignore

let translate_float_store c ~after_pop =
  match operands c with
  | [ dest ] ->
      (* assuming get_fpu_stack has nothing to do with float comparisons, order doesn't matter *)
      add_float_comparison c [];
      let value = get_fpu_stack c 0 in
      store_operand_f c value ~dest;
      if after_pop then fpu_pop c |> ignore
  | _ -> raise_ops c

let translate_float_store_status_word c =
  match operands c with
  | [ Register { reg = `ax; _ } ] ->
      assert_c c
        (not @@ Instr.Ref.equal c.state.fpu_status_word Instr.Ref.invalid)
        ~msg:"only store fpu status word once";
      let value = B.landmine c.builder Int ~varName:(X86reg.to_ident `eax) in
      c.state.fpu_status_word <- value;
      c.state.fpu_status_word_instr <- c.state.float_compare_instr;
      c.state.fpu_status_word_args <- c.state.float_compare_args
  | _ -> raise_ops c

let translate_call_start c =
  add_comparison c [];
  add_float_comparison c [];
  (* push address onto the stack *)
  let esp =
    B.sub c.builder ~rhs:(B.const c.builder 4) ~lhs:(newest_var c `esp)
      ~varName:(X86reg.to_ident `esp)
  in
  B.store32 c.builder ~addr:esp ~value:(B.const c.builder c.opcode.address)

let translate_call_end c =
  (* assert address is correct *)
  let esp = newest_var c `esp in
  B.mir_assert c.builder
  @@ B.equal c.builder ~lhs:(B.load32 c.builder esp)
       ~rhs:(B.const c.builder c.opcode.address);
  B.add c.builder ~rhs:(B.const c.builder 4) ~lhs:esp
    ~varName:(X86reg.to_ident `esp)
  |> ignore

let translate_direct_call c func_name func_sig =
  let args =
    List.map ~f:(fun a -> B.newest_var c.builder a.name) func_sig.args
  in
  translate_call_start c;
  B.call c.builder func_name args func_sig.return.typ
    ~varName:func_sig.return.name
  |> ignore;
  translate_call_end c

let translate_indirect_call c ~addr func_sig =
  let args =
    (* maybe check the types line up? *)
    List.map ~f:(fun a -> B.newest_var c.builder a.name) func_sig.args
  in
  translate_call_start c;
  B.call_indirect c.builder addr args ~varName:func_sig.return.name
    func_sig.return.typ
  |> ignore;
  translate_call_end c

let translate_call c =
  match operands c with
  (* intrinsics! *)
  | [
   ( Immediate { value = addr; _ }
   | Memory
       {
         size = 4;
         base = None;
         index = None;
         scale = 1;
         segment = None;
         displacement = addr;
       } );
  ]
    when Hashtbl.mem c.intrinsics addr ->
      let intrinisc = Hashtbl.find_exn c.intrinsics addr in
      translate_direct_call c intrinisc.mir_name intrinisc.signature
  | [ Immediate { value; _ } ] ->
      translate_direct_call c (Util.addr_to_func_name value) Util.fast_call
  | [ Memory ({ size = 4; _ } as mem) ] ->
      translate_indirect_call c Util.fast_call
        ~addr:(load_complete_address c mem)
  | [ Register { size = 4; reg } ] ->
      translate_indirect_call c Util.fast_call ~addr:(newest_var c reg)
  | _ -> raise_ops c

let translate_no_prefix c =
  assert_c c (c.opcode.prefix = opcode_prefix_none);
  match c.opcode.id with
  | MOV -> translate_mov c
  | MOVZX -> translate_mov_zero_extend c
  | MOVSX -> translate_mov_sign_extend c
  | PUSH -> translate_push c
  | POP -> translate_pop c
  | TEST | CMP -> translate_test_cmp c
  | AND -> translate_bi_op c B.int_and `Result
  | OR -> translate_bi_op c B.int_or `Result
  | XOR -> translate_xor c
  | ADD -> translate_bi_op c B.add `All
  | SUB -> translate_bi_op c B.sub `All
  | SHL -> translate_bi_op c B.shift_left `Result
  | SAR -> translate_shift_right c ~signed:false
  | SHR -> translate_shift_right c ~signed:true
  | INC -> translate_inc c
  | SETG | SETGE | SETL | SETLE | SETE | SETNE | SETA | SETAE | SETB | SETBE ->
      translate_set_cond c
  | LEA -> translate_lea c
  | FLD -> translate_float_load c
  | FCOM -> translate_float_comparison c ~after_pop:`None
  | FCOMP -> translate_float_comparison c ~after_pop:`Once
  | FCOMPP -> translate_float_comparison c ~after_pop:`Twice
  | FST -> translate_float_store c ~after_pop:false
  | FSTP -> translate_float_store c ~after_pop:true
  | FNSTSW -> translate_float_store_status_word c
  | CALL -> translate_call c
  | _ -> raise_c c "Invalid instruction"

let translate_rep_prefix c = raise_c c "rep prefix"
let translate_repne_prefix c = raise_c c "repne prefix"

let translate intrinsics builder state opcode =
  let c = { builder; opcode; state; intrinsics } in
  match opcode.prefix with
  | p when p = opcode_prefix_none -> translate_no_prefix c
  | p when p = opcode_prefix_rep -> translate_rep_prefix c
  | p when p = opcode_prefix_repne -> translate_repne_prefix c
  | _ -> raise_c c "Invalid opcode prefix"

type term_trans_result =
  | Nothing
  | Unconditional of { target : int }
  | Conditional of { target : int; condition : Mir.Instr.Ref.t }
  | Switch of { switch_on : Mir.Instr.Ref.t; table_addr : int }
  | Return
[@@deriving sexp_of]

let translate_terminator intrinsics builder state opcode =
  match opcode with
  | {
   id = JMP;
   prefix = 0;
   opex =
     {
       operands =
         [
           Memory
             {
               index = Some (#X86reg.reg_32bit as switch_reg);
               base = None;
               segment = None;
               scale = 4;
               displacement;
               size = 4;
             };
         ];
     };
   _;
  } ->
      Switch
        {
          table_addr = displacement;
          switch_on = B.newest_var builder (X86reg.to_ident switch_reg);
        }
  | {
   id = JMP;
   prefix = 0;
   opex = { operands = [ Immediate { size = 4; value } ] };
   _;
  } ->
      Unconditional { target = value }
  | { id = RET; prefix = 0; opex = { operands = [] }; _ } ->
      B.set_global builder Util.stack_pointer_global Int
      @@ B.newest_var builder @@ X86reg.to_ident `esp;
      Return
  | {
   id = RET;
   prefix = 0;
   opex = { operands = [ Immediate { value; _ } ] };
   _;
  } ->
      let esp = X86reg.to_ident `esp in
      let esp =
        B.add builder ~lhs:(B.newest_var builder esp)
          ~rhs:(B.const builder value) ~varName:esp
      in
      B.set_global builder Util.stack_pointer_global Int esp;
      Return
  | _ ->
      translate intrinsics builder state opcode;
      Nothing

let translate_output_condition builder state condition_instr = ()
