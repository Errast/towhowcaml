open! Core
module T_Util = Util
open Radatnet
open Mir
module B = Builder

let vcount = ref 0

type term_trans_result =
  | Nothing
  | Unconditional of { target : int }
  | Conditional of { target : int; condition : Mir.Instr.Ref.t }
  | Switch of { switch_on : Mir.Instr.Ref.t; table_addr : int }
  | Return
[@@deriving sexp_of]

type global_info = { value : Instr.Ref.t; modified : bool; global : variable }
[@@deriving sexp_of]

type state = {
  mutable compare_args : (Instr.Ref.t * [ X86reg.gpr_type | `ignore ]) list;
  mutable compare_instr : X86_instr.t;
  mutable input_condition_opcode : opcode option;
  mutable changed_flags : Status_flags.t;
  mutable fpu_status_word_args : Instr.Ref.t list;
  mutable fpu_status_word_instr : X86_instr.t;
  mutable float_compare_args : Instr.Ref.t list;
  mutable float_compare_instr : X86_instr.t;
  mutable backward_direction : bool;
  (* the height of the local fpu stack. can be negative if it pulls from the global stack  *)
  mutable local_fpu_stack_height : int;
  (* the keys here are offsets from the head of the global stack *)
  (* the stack grows up *)
  fpu_stack_changes : (int, Instr.Ref.t) Hashtbl.t;
  mutable fpu_status_word : Instr.Ref.t;
}
[@@deriving sexp_of]

let initial_state () =
  {
    compare_args = [];
    compare_instr = INVALID;
    input_condition_opcode = None;
    changed_flags = Status_flags.none;
    fpu_status_word_args = [];
    fpu_status_word_instr = INVALID;
    float_compare_args = [];
    float_compare_instr = INVALID;
    backward_direction = false;
    local_fpu_stack_height = 0;
    fpu_stack_changes = Hashtbl.create ~size:0 (module Int);
    fpu_status_word = Instr.Ref.invalid;
  }

let backward_direction s = s.backward_direction
let compare_instr s = s.compare_instr
let input_condition_opcode s = s.input_condition_opcode

type context = {
  opcode : Radatnet.Types.opcode;
  state : state;
  builder : Mir.Builder.t;
  intrinsics : ((int, Util.intrinsic) Hashtbl.t[@sexp.opaque]);
}
[@@deriving sexp_of]

let empty_hashtbl = Hashtbl.create ~growth_allowed:false ~size:0 (module Int)

let flags_used_tbl =
  let open X86_instr in
  Hashtbl.of_alist_exn
    (module X86_instr)
    Status_flags.
      [
        (JG, zero %| sign %| overflow);
        (SETG, zero %| sign %| overflow);
        (JGE, sign %| overflow);
        (SETGE, sign %| overflow);
        (JL, sign %| overflow);
        (SETL, sign %| overflow);
        (JLE, zero %| sign %| overflow);
        (SETLE, zero %| sign %| overflow);
        (JE, zero);
        (SETE, zero);
        (JNE, zero);
        (SETNE, zero);
        (JA, carry %| overflow);
        (SETA, carry %| overflow);
        (JAE, zero %| carry %| overflow);
        (SETAE, zero %| carry %| overflow);
        (JB, carry);
        (SETB, carry);
        (JBE, zero %| carry);
        (SETBE, zero %| carry);
        (JP, parity);
        (JNP, parity);
        (SETNP, parity);
        (JNS, sign);
        (JS, sign);
        (SBB, carry);
        (ADC, carry);
        (JECXZ, none);
        (JNO, overflow);
        (SETNO, overflow);
        (JO, overflow);
        (SETO, overflow);
        (RCR, carry);
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
let raise_comp_ops c = raise_c c "Invalid comparison or compariosn arguments"
let assert_c c ?(msg = "Assert failed") b = if not b then raise_c c msg
let operands c = c.opcode.opex.operands
let newest_var c reg = X86reg.to_ident reg |> B.newest_var c.builder

let rec instr_value c ref =
  match B.get_instr c.builder ref with
  | Const (_, i) -> Some i
  | DupVar { src; _ } -> instr_value c src
  | _ -> None

let instr_is_zeroed c ref =
  instr_value c ref |> Option.value_map ~default:false ~f:(equal_int 0)

let rec instr_is_vec_zeroed c ref =
  match B.get_instr c.builder ref with
  | VecConst { lower_bits = 0L; upper_bits = 0L; _ } -> true
  | DupVar { src; _ } -> instr_is_vec_zeroed c src
  | _ -> false

let reg_type_of_size = function
  | 4 -> `Reg32Bit
  | 2 -> `Reg16Bit
  | 1 -> `RegLow8Bit
  | _ -> failwith "invalid size"

let typed_ref_equal :
    Instr.ref * X86reg.gpr_type -> Instr.ref * X86reg.gpr_type -> bool =
 fun (ref1, t1) (ref2, t2) -> Instr.Ref.equal ref1 ref2 && Poly.(t1 = t2)

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
  if offset <> 0 then B.add c.builder ~lhs:addr ~rhs:(B.const c.builder offset)
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
       | #X86reg.reg_32bit -> B.try_change_var b reg_ident src
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
  | Memory ({ segment = None | Some Es; _ } as address) -> (
      let addr_var, offset = load_partial_address c address in
      let addr_var =
        if Option.value_map address.segment ~default:false ~f:(Poly.equal Fs)
        then raise_c c "fs segment"
        else addr_var
      in
      match address.size with
      | 4 -> B.store32 c.builder ~value:src ~addr:addr_var ~offset ~plane:0
      | 2 -> B.store16 c.builder ~value:src ~addr:addr_var ~offset ~plane:0
      | 1 -> B.store8 c.builder ~value:src ~addr:addr_var ~offset ~plane:0
      | _ -> raise_m c [%message "Invalid store size" (dest : operand)])
  | Memory
      {
        segment = Some Fs;
        index = None;
        base = None;
        scale = 1;
        displacement;
        size = 4;
      } ->
      B.set_global c.builder (Util.global_of_tib_offset displacement) src
  | _ -> raise_m c [%message "Cannot store into operand" (dest : operand)]

(* Not guarenteed to be zero nor sign extended *)
let load_operand_typed c src : Instr.ref * [> X86reg.gpr_type ] =
  match src with
  | Immediate { value; size } ->
      let r_typ =
        match size with
        | 1 -> `RegLow8Bit
        | 2 -> `Reg16Bit
        | 4 -> `Reg32Bit
        | _ -> raise_c c "const size"
      in
      (B.const c.builder value, r_typ)
  | Register { reg = #X86reg.general_purpose as reg; _ } ->
      let r_type =
        match X86reg.reg_type reg with
        | `X87Float | `Mmx | `Sse ->
            raise_m c [%message "can't load operand" (src : operand)]
        | #X86reg.gpr_type as t -> t
      in
      (X86reg.to_32_bit reg |> X86reg.to_ident |> B.newest_var c.builder, r_type)
  | Memory ({ segment = None | Some Es; _ } as address) -> (
      let addr_var, offset = load_partial_address c address in
      match address.size with
      | 4 -> (B.load32 c.builder addr_var ~offset ~plane:0, `Reg32Bit)
      | 2 ->
          (B.load16 c.builder addr_var ~offset ~plane:0 ~signed:false, `Reg16Bit)
      | 1 ->
          ( B.load8 c.builder addr_var ~offset ~plane:0 ~signed:false,
            `RegLow8Bit )
      | _ -> raise_m c [%message "can't load operand" (src : operand)])
  | Memory
      {
        segment = Some Fs;
        index = None;
        base = None;
        scale = 1;
        displacement;
        size = 4;
      } ->
      ( B.get_global c.builder (Util.global_of_tib_offset displacement),
        `Reg32Bit )
  | _ -> raise_m c [%message "can't load operand" (src : operand)]

let extend_unsigned c (instr, typ) =
  match typ with
  | `Reg32Bit -> instr
  | `Reg16Bit ->
      vcount := !vcount + 1;
      B.zero_extend_16 c.builder instr
  | `RegHigh8Bit ->
      vcount := !vcount + 1;
      B.zero_extend_high8 c.builder instr
  | `RegLow8Bit ->
      vcount := !vcount + 1;
      B.zero_extend_low8 c.builder instr

let extend_signed c (instr, typ) =
  match typ with
  | `Reg32Bit -> instr
  | `Reg16Bit ->
      vcount := !vcount + 1;
      B.sign_extend_16 c.builder instr
  | `RegHigh8Bit ->
      vcount := !vcount + 1;
      B.sign_extend_high8 c.builder instr
  | `RegLow8Bit ->
      vcount := !vcount + 1;
      B.sign_extend_low8 c.builder instr

let load_operand_s c src = load_operand_typed c src |> extend_signed c
let load_operand_u c src = load_operand_typed c src |> extend_unsigned c

(* signed should be the default because it's one little WASM instruction *)
let extend_either = extend_signed
let load_operand = load_operand_s

let add_comparison c args =
  c.state.compare_instr <- c.opcode.id;
  c.state.compare_args <- args;
  c.state.changed_flags <- Status_flags.none

let add_float_comparison c args =
  c.state.float_compare_instr <- c.opcode.id;
  c.state.float_compare_args <- args

let set_fpu_stack c value index =
  Hashtbl.set c.state.fpu_stack_changes
    ~key:(c.state.local_fpu_stack_height - index)
    ~data:value

(* this currently store things to the stack that were read but not modified *)
(* not a big deal *)
let get_fpu_stack c index =
  let offset = c.state.local_fpu_stack_height - index in
  Hashtbl.find_or_add c.state.fpu_stack_changes offset ~default:(fun () ->
      let addr = B.newest_var c.builder Util.fpu_stack_pointer in
      let offset = offset * int_of_float_size in
      match float_size with
      | `Single -> B.float_load32 c.builder addr ~offset ~plane:0
      | `Double -> B.float_load64 c.builder addr ~offset ~plane:0)

let fpu_push c value =
  c.state.local_fpu_stack_height <- c.state.local_fpu_stack_height + 1;
  set_fpu_stack c value 0

let fpu_pop c =
  c.state.local_fpu_stack_height <- c.state.local_fpu_stack_height - 1

let fpu_pop_off c =
  let value = get_fpu_stack c 0 in
  fpu_pop c;
  value

let load_operand_f c src =
  match src with
  | Immediate { value; size = 4 } ->
      B.float_const c.builder @@ Mir.Bits.int32_to_float value
  | Register { reg = #X86reg.x87_float as reg; _ } ->
      get_fpu_stack c @@ X86reg.x87_float_to_index reg
  | Memory ({ segment = None | Some Es; _ } as mem) -> (
      let addr, offset = load_partial_address c mem in
      match mem.size with
      | 4 -> B.float_load32 c.builder addr ~offset ~plane:1
      | 8 -> B.float_load64 c.builder addr ~offset ~plane:1
      | 10 ->
          let addr = load_complete_address c mem in
          B.call c.builder Util.load_big_float_func [ addr ];
          B.returned c.builder Float
      | _ -> raise_c c "invalid size")
  | _ -> raise_c c "invalid operand"

let store_operand_f c value ~dest =
  match dest with
  | Register { reg = #X86reg.x87_float as reg; _ } ->
      set_fpu_stack c value @@ X86reg.x87_float_to_index reg
  | Memory ({ segment = None | Some Es; size = 4 | 8 | 10; _ } as mem) -> (
      let addr, offset = load_partial_address c mem in
      match mem.size with
      | 4 -> B.float_store32 c.builder ~value ~addr ~offset
      | 8 -> B.float_store64 c.builder ~value ~addr ~offset
      | 10 ->
          let addr = load_complete_address c mem in
          B.call c.builder Util.store_big_float_func [ addr; value ]
      | _ -> raise_c c "invalid operand")
  | _ -> raise_c c "invalid operand"

(* merging mm and xmm here is probably fine *)
let load_operand_v c src =
  match src with
  | Register { size = 16; reg = #X86reg.sse as reg } ->
      B.newest_var c.builder @@ X86reg.to_ident reg
  | Memory ({ segment = None; size = 16; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.vec_load128 c.builder addr ~offset
  | _ -> raise_c c "invalid operand"

let store_operand_v c value ~dest =
  match dest with
  | Register { size = 16; reg = #X86reg.sse as reg } ->
      B.try_change_var c.builder (X86reg.to_ident reg) value |> ignore
  | Memory ({ segment = None; size = 16; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.vec_store128 c.builder ~addr ~offset ~value
  | _ -> raise_c c "invalid operand"

let load_operand_l c src =
  match src with
  | Memory ({ segment = None; size = 8; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.long_load64 c.builder addr ~offset
  | _ -> raise_c c "invalid operand"

let store_operand_l c value ~dest =
  match dest with
  | Memory ({ segment = None; size = 8; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.long_store64 c.builder ~addr ~offset ~value
  | _ -> raise_c c "invalid operand"

let load_operand_mmx c src =
  match src with
  | Register { size = 8; reg = #X86reg.mmx as reg } ->
      B.newest_var c.builder @@ X86reg.to_ident reg
  | Memory ({ segment = None; size = 8; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.vec_load64_zero_extend c.builder addr ~offset
  | _ -> raise_c c "invalid operand"

let store_operand_mmx c value ~dest =
  match dest with
  | Register { size = 8; reg = #X86reg.mmx as reg } ->
      B.try_change_var c.builder (X86reg.to_ident reg) value |> ignore
  | Memory ({ segment = None; size = 8; _ } as mem) ->
      let addr, offset = load_partial_address c mem in
      B.vec_store c.builder ~addr ~offset ~vec:value ~shape:`I64 ~lane:0
  | _ -> raise_c c "invalid operand"

let write_fpu_stack_changes ~state ~builder =
  (* Do this before resetting local_fpu_stack_height *)
  let stack_head = state.local_fpu_stack_height in
  if not @@ Hashtbl.is_empty state.fpu_stack_changes then (
    let addr = B.newest_var builder Util.fpu_stack_pointer in
    Hashtbl.iteri state.fpu_stack_changes ~f:(fun ~key:index ~data:value ->
        if index <= stack_head then
          match float_size with
          | `Single ->
              B.float_store32 builder ~addr ~value ~plane:1
                ~offset:(index * int_of_float_size)
          | `Double ->
              B.float_store64 builder ~addr ~value ~plane:1
                ~offset:(index * int_of_float_size));
    Hashtbl.clear state.fpu_stack_changes;

    if stack_head <> 0 then
      B.add builder ~varName:Util.fpu_stack_pointer
        ~lhs:(B.newest_var builder Util.fpu_stack_pointer)
        ~rhs:(B.const builder @@ (stack_head * int_of_float_size))
      |> ignore;
    state.local_fpu_stack_height <- 0)
  else assert (state.local_fpu_stack_height = 0)

let write_globals state builder =
  write_fpu_stack_changes ~state ~builder;
  B.store_globals builder

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

(* TODO do the mask *)
let translate_shift_left c =
  match operands c with
  | [ dest; Immediate { value; _ } ] when value < operand_size dest * 8 ->
      let lhs = load_operand c dest in
      let rhs = B.const c.builder value in
      let res = B.shift_left c.builder ~lhs ~rhs in
      store_operand c res ~dest;
      add_comparison c [ (res, loaded_reg_type dest) ]
  | [ dest; arg ] ->
      let res =
        B.shift_left c.builder ~rhs:(load_operand c arg)
          ~lhs:(load_operand c dest)
      in
      store_operand c res ~dest;
      add_comparison c []
  | _ -> raise_ops c

let translate_shift_right c ~signed =
  match operands c with
  | [ dest; src ] ->
      let lhs =
        if signed then load_operand_s c dest else load_operand_u c dest
      in
      let rhs = if signed then load_operand_s c src else load_operand_u c src in
      let res = B.shift_right ~signed c.builder ~lhs ~rhs in
      store_operand c res ~dest;
      (* technically if the shift is 0, the flags don't change. better safe *)
      add_comparison c
        [
          (lhs, loaded_reg_type dest);
          (rhs, loaded_reg_type src);
          (res, loaded_reg_type dest);
        ]
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
      let rhs = B.const c.builder 1 in
      let res = B.add c.builder ~lhs ~rhs in
      store_operand c res ~dest:arg;
      add_comparison c [ (res, loaded_reg_type arg) ]
  | _ -> raise_ops c

let translate_lea c =
  match operands c with
  | [ dest; Memory mem ] -> store_operand c ~dest @@ load_complete_address c mem
  | _ -> raise_ops c

let translate_float_load c =
  match operands c with
  | [ arg ] -> load_operand_f c arg |> fpu_push c
  (*add_float_comparison c []*)
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
  | `Once -> fpu_pop_off c |> ignore
  | `Twice ->
      fpu_pop_off c |> ignore;
      fpu_pop_off c |> ignore

let translate_float_store c ~after_pop =
  match operands c with
  | [ dest ] ->
      (* assuming get_fpu_stack has nothing to do with float comparisons, order doesn't matter *)
      add_float_comparison c [];
      let value = get_fpu_stack c 0 in
      store_operand_f c value ~dest;
      if after_pop then fpu_pop_off c |> ignore
  | _ -> raise_ops c

let translate_float_store_status_word c =
  match (operands c, c.opcode.address) with
  | [ Register { reg = `ax; _ } ], _ ->
      assert_c c
        (Instr.Ref.equal c.state.fpu_status_word Instr.Ref.invalid)
        ~msg:"only store fpu status word once";
      let value = B.landmine c.builder Int ~varName:(X86reg.to_ident `eax) in
      c.state.fpu_status_word <- value;
      c.state.fpu_status_word_instr <- c.state.float_compare_instr;
      c.state.fpu_status_word_args <- c.state.float_compare_args
  | [ dest ], (0x4893c2 | 0x483ab6) ->
      (* not sure what the status word is used for at 0x483ab6, but I hope it's fine *)
      B.const c.builder 0 |> store_operand c ~dest
  | _ -> raise_ops c

let translate_call_start c ~nontail =
  add_comparison c [];
  add_float_comparison c [];
  (* push address onto the stack *)
  (if nontail then
     let esp =
       B.sub c.builder ~rhs:(B.const c.builder 4) ~lhs:(newest_var c `esp)
         ~varName:(X86reg.to_ident `esp)
     in
     B.store32 c.builder ~addr:esp ~value:(B.const c.builder c.opcode.address));
  write_globals c.state c.builder

let translate_call_end c ~nontail =
  if nontail then
    B.mir_assert c.builder
    @@ B.equal c.builder
         ~lhs:(B.newest_var c.builder @@ X86reg.to_ident `eip)
         ~rhs:(B.const c.builder @@ c.opcode.address + c.opcode.size)

let translate_direct_call c func_name func_sig ~nontail =
  translate_call_start c ~nontail;
  let args =
    List.map ~f:(fun a -> B.newest_var c.builder a.name) func_sig.args
  in
  B.call c.builder func_name args |> ignore;
  List.iter func_sig.returns ~f:(fun { name; typ } ->
      B.returned c.builder ~varName:name typ |> ignore);
  translate_call_end c ~nontail

let translate_indirect_call c ~addr func_sig ~nontail =
  translate_call_start c ~nontail;
  let args =
    (* maybe check the types line up? *)
    List.map ~f:(fun a -> B.newest_var c.builder a.name) func_sig.args
  in
  B.call_indirect c.builder addr args;
  List.iter func_sig.returns ~f:(fun { name; typ } ->
      B.returned c.builder ~varName:name typ |> ignore);
  translate_call_end c ~nontail

let translate_call c ~nontail =
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
      translate_direct_call c intrinisc.name intrinisc.signature ~nontail
  | [ Immediate { value; _ } ] ->
      translate_direct_call c
        (Util.addr_to_func_name value)
        Util.fast_call ~nontail
  | [ Memory ({ size = 4; _ } as mem) ] ->
      translate_indirect_call c Util.fast_call ~nontail
        ~addr:(load_complete_address c mem)
  | [ Register { size = 4; reg } ] ->
      translate_indirect_call c Util.fast_call ~addr:(newest_var c reg) ~nontail
  | _ -> raise_ops c

let translate_input_cond c =
  if
    match c.state.input_condition_opcode with
    | None -> false
    | Some o -> Poly.(o.id <> c.opcode.id)
  then raise_c c "multiple input conditions";
  c.state.input_condition_opcode <- Some c.opcode;
  B.newest_var c.builder Util.input_compare_arg

let translate_float_test_comparison c constant =
  assert_c c
    (match c.state.fpu_status_word_instr with
    | FCOM | FCOMP | FCOMPP -> true
    | _ -> false)
    ~msg:"must use fcom";
  let lhs, rhs =
    match c.state.fpu_status_word_args with
    | [ lhs; rhs ] -> (lhs, rhs)
    | _ -> raise_ops c
  in
  match (c.opcode.id, constant) with
  | JP, 0x41 ->
      B.equals_zero c.builder (B.float_less_than_equal c.builder ~lhs ~rhs)
  | JP, 0x44 -> B.float_not_equal c.builder ~lhs ~rhs
  | JNE, 0x1 ->
      B.equals_zero c.builder (B.float_greater_than_equal c.builder ~lhs ~rhs)
  | JP, 0x5 -> B.equals_zero c.builder (B.float_less_than c.builder ~lhs ~rhs)
  | JNP, 0x5 -> B.float_less_than c.builder ~lhs ~rhs
  | JNP, 0x44 -> B.float_equal c.builder ~lhs ~rhs
  | JNE, 0x41 ->
      B.float_greater_than c.builder ~lhs ~rhs |> B.equals_zero c.builder
  | JE, 0x41 ->
      B.float_less_than_equal c.builder ~lhs ~rhs |> B.equals_zero c.builder
  | JNP, 0x41 -> B.float_less_than_equal c.builder ~lhs ~rhs
  | JE, 0x1 -> B.float_greater_than_equal c.builder ~lhs ~rhs
  | _ -> raise_comp_ops c

let translate_reflexive_test_comparison c arg =
  match c.opcode.id with
  | JE | SETE | JA ->
      let operand = extend_either c arg in
      B.equals_zero c.builder operand
  | JNE -> extend_either c arg
  | SETNE ->
      let operand = extend_either c arg in
      B.equals_zero c.builder (B.equals_zero c.builder operand)
  | JG | SETG ->
      let operand = extend_either c arg in
      B.greater_than c.builder ~lhs:operand ~rhs:(B.const c.builder 0)
        ~signed:true
  | JGE | SETGE ->
      let operand = extend_either c arg in
      B.greater_than_equal c.builder ~lhs:operand ~rhs:(B.const c.builder 0)
        ~signed:true
  | JL | SETL ->
      let operand = extend_either c arg in
      B.less_than c.builder ~lhs:operand ~rhs:(B.const c.builder 0) ~signed:true
  | JLE | SETLE ->
      let operand = extend_either c arg in
      B.less_than_equal c.builder ~lhs:operand ~rhs:(B.const c.builder 0)
        ~signed:true
  | JS | SETS ->
      B.less_than c.builder ~rhs:(B.const c.builder 0)
        ~lhs:(extend_signed c arg) ~signed:true
  | JNS | SETNS ->
      B.greater_than_equal c.builder ~rhs:(B.const c.builder 0)
        ~lhs:(extend_signed c arg) ~signed:true
  | JBE -> B.equals_zero c.builder (extend_either c arg)
  | JAE -> B.const c.builder 0
  | _ -> raise_comp_ops c

let translate_test_comparison c ~lhs ~rhs =
  match (c.opcode.id, lhs) with
  | _, (lhs, `RegHigh8Bit) when Instr.Ref.equal lhs c.state.fpu_status_word ->
      translate_float_test_comparison c
      @@ Option.value_or_thunk ~default:(fun () -> raise_ops c)
      @@ instr_value c @@ fst rhs
  | _ when typed_ref_equal lhs rhs -> translate_reflexive_test_comparison c lhs
  | (JE | SETE), _ ->
      let lhs = extend_unsigned c lhs in
      let rhs = extend_unsigned c rhs in
      let anded = B.int_and c.builder ~lhs ~rhs in
      B.equals_zero c.builder anded
  | JNE, _ ->
      let lhs = extend_unsigned c lhs in
      let rhs = extend_unsigned c rhs in
      B.int_and c.builder ~lhs ~rhs
  | SETNE, _ ->
      let lhs = extend_unsigned c lhs in
      let rhs = extend_unsigned c rhs in
      let anded = B.int_and c.builder ~lhs ~rhs in
      B.equals_zero c.builder (B.equals_zero c.builder anded)
  | _ -> raise_comp_ops c

let translate_cmp_comparison c ~lhs ~rhs =
  match c.opcode.id with
  | JE | SETE ->
      let lhs = extend_either c lhs in
      let rhs = extend_either c rhs in
      B.equal c.builder ~lhs ~rhs
  | JNE | SETNE ->
      let lhs = extend_either c lhs in
      let rhs = extend_either c rhs in
      B.not_equal c.builder ~lhs ~rhs
  | JB | SETB ->
      B.less_than c.builder ~rhs:(extend_unsigned c rhs)
        ~lhs:(extend_unsigned c lhs) ~signed:false
  | JBE | SETBE ->
      let lhs = extend_unsigned c lhs in
      let rhs = extend_unsigned c rhs in
      B.less_than_equal c.builder ~lhs ~rhs ~signed:false
  | JLE | SETLE ->
      let lhs = extend_signed c lhs in
      let rhs = extend_signed c rhs in
      B.less_than_equal c.builder ~lhs ~rhs ~signed:true
  | JL | SETL ->
      let lhs = extend_signed c lhs in
      let rhs = extend_signed c rhs in
      B.less_than c.builder ~lhs ~rhs ~signed:true
  | JGE | SETGE ->
      let lhs = extend_signed c lhs in
      let rhs = extend_signed c rhs in
      B.greater_than_equal c.builder ~lhs ~rhs ~signed:true
  | JG | SETG ->
      let lhs = extend_signed c lhs in
      let rhs = extend_signed c rhs in
      B.greater_than c.builder ~lhs ~rhs ~signed:true
  | JA | SETA ->
      let lhs = extend_unsigned c lhs in
      let rhs = extend_unsigned c rhs in
      B.greater_than c.builder ~lhs ~rhs ~signed:false
  | JAE | SETAE ->
      B.greater_than_equal c.builder ~signed:false ~rhs:(extend_unsigned c rhs)
        ~lhs:(extend_unsigned c lhs)
  | ADC | SBB ->
      B.less_than c.builder ~rhs:(extend_unsigned c rhs)
        ~lhs:(extend_unsigned c lhs) ~signed:false
  | _ -> raise_comp_ops c

let translate_dec_comparison c ~result =
  (* slightly different from cmp *)
  match c.opcode.id with
  | JS | SETS ->
      B.less_than ~rhs:(B.const c.builder 0) ~lhs:(extend_signed c result)
        ~signed:true c.builder
  | JNS ->
      B.greater_than_equal ~rhs:(B.const c.builder 0)
        ~lhs:(extend_signed c result) ~signed:true c.builder
  | JE -> B.equals_zero c.builder (extend_either c result)
  | JNE -> extend_either c result
  | _ -> raise_comp_ops c

let translate_inc_comparison c ~result =
  (* slightly different from add *)
  match c.opcode.id with
  | JNE -> extend_either c result
  | _ -> raise_comp_ops c

let translate_result_comparison c ~result ~cf_zero ~of_zero =
  match c.opcode.id with
  | JE -> B.equals_zero c.builder (extend_either c result)
  | JNE -> extend_either c result
  | JNS ->
      B.greater_than_equal c.builder ~rhs:(B.const c.builder 0)
        ~lhs:(extend_signed c result) ~signed:true
  | JS ->
      B.less_than c.builder ~rhs:(B.const c.builder 0)
        ~lhs:(extend_signed c result) ~signed:true
  | JBE when cf_zero -> B.equals_zero c.builder (extend_either c result)
  | JLE when of_zero ->
      B.less_than c.builder ~lhs:(extend_signed c result)
        ~rhs:(B.const c.builder 0) ~signed:true
  | _ -> raise_comp_ops c

let translate_sub_comparison c ~lhs ~rhs ~result =
  match c.opcode.id with
  (* lhs unsigned< rhs -> lhs unsigned< lhs - rhs -> result unsigned> lhs *)
  | ADC | SBB | JB ->
      B.greater_than c.builder ~rhs:(extend_unsigned c lhs)
        ~lhs:(extend_unsigned c result) ~signed:false
  | i
    when let used = flags_used_exn i in
         Status_flags.(contains overflow used || contains carry used) ->
      translate_cmp_comparison c ~lhs ~rhs
  | _ -> translate_result_comparison c ~result ~cf_zero:false ~of_zero:false

let translate_add_comparison c ~lhs ~result =
  match c.opcode.id with
  (* CF=1 -> result unsigned< lhs *)
  | ADC | SBB | JB ->
      B.less_than c.builder ~rhs:(extend_unsigned c lhs)
        ~lhs:(extend_unsigned c result) ~signed:false
  (* CF=0 *)
  | JAE ->
      B.greater_than_equal c.builder ~rhs:(extend_unsigned c lhs)
        ~lhs:(extend_unsigned c result) ~signed:false
  | _ -> translate_result_comparison c ~result ~cf_zero:false ~of_zero:false

let translate_shr_comparison c ~lhs ~rhs ~result =
  match c.opcode.id with
  | RCR ->
      let lhs = extend_unsigned c lhs in
      (*RCR wants CF in the MSB & is always 32 bits *)
      let thirty_two = B.const c.builder 32 in
      let rhs = extend_either c rhs in
      B.mir_assert c.builder rhs;
      B.shift_left c.builder ~rhs:(B.sub c.builder ~lhs:thirty_two ~rhs) ~lhs
  | _ -> translate_result_comparison c ~result ~cf_zero:true ~of_zero:false

let translate_sahf_comparison c =
  match
    (c.state.fpu_status_word_instr, c.opcode.id, c.state.fpu_status_word_args)
  with
  | (FCOM | FCOMP | FCOMPP), JAE, [ lhs; rhs ] ->
      B.float_greater_than_equal c.builder ~lhs ~rhs
  | (FCOM | FCOMP | FCOMPP), JBE, [ lhs; rhs ] ->
      B.equals_zero c.builder (B.float_greater_than c.builder ~lhs ~rhs)
  | (FCOM | FCOMP | FCOMPP), JP, [ lhs; rhs ] ->
      B.int_or c.builder
        ~rhs:(B.float_not_equal c.builder ~rhs ~lhs:rhs)
        ~lhs:(B.float_not_equal c.builder ~lhs ~rhs:lhs)
  | (FCOM | FCOMP | FCOMPP), JNE, [ lhs; rhs ] ->
      B.int_or c.builder
        ~rhs:(B.float_greater_than c.builder ~lhs ~rhs)
        ~lhs:(B.float_less_than c.builder ~lhs ~rhs)
  | (FCOM | FCOMP | FCOMPP), JE, [ lhs; rhs ] ->
      B.equal c.builder
        ~rhs:(B.float_less_than_equal c.builder ~lhs ~rhs)
        ~lhs:(B.float_greater_than_equal c.builder ~lhs ~rhs)
  | (FPTAN | FSIN | FCOS), JP, [ arg ] ->
      let norm = B.float_abs c.builder arg in
      let const = B.float_const c.builder @@ Float.int_pow 2. 63 in
      B.float_less_than c.builder ~lhs:norm ~rhs:const
  | FPREM1, JP, [] -> B.const c.builder 0
  | _ -> raise_comp_ops c

let translate_neg_comparison c ~result =
  match c.opcode.id with
  | SBB | ADC ->
      B.equals_zero c.builder (B.equals_zero c.builder (extend_either c result))
  | _ -> raise_comp_ops c

let translate_ucomisd_comparison c ~lhs ~rhs =
  match c.opcode.id with
  | JNP ->
      B.vec_and c.builder
        ~rhs:(B.vec_equal c.builder ~shape:`F64 ~lhs:rhs ~rhs)
        ~lhs:(B.vec_equal c.builder ~shape:`F64 ~rhs:lhs ~lhs)
      |> B.vec_extract c.builder ~shape:`I32 ~lane:0
  | JP ->
      B.vec_or c.builder
        ~rhs:(B.vec_not_equal c.builder ~shape:`F64 ~lhs:rhs ~rhs)
        ~lhs:(B.vec_not_equal c.builder ~shape:`F64 ~rhs:lhs ~lhs)
      |> B.vec_extract c.builder ~shape:`I32 ~lane:0
  | JAE ->
      B.vec_greater_than_equal c.builder ~lhs ~rhs ~shape:F64
      |> B.vec_extract c.builder ~shape:`I32 ~lane:0
  | JB ->
      B.vec_greater_than_equal c.builder ~lhs ~rhs ~shape:F64
      |> B.vec_extract c.builder ~shape:`I32 ~lane:0
      |> B.equals_zero c.builder
  | JNE ->
      B.vec_or c.builder
        ~rhs:(B.vec_less_than c.builder ~lhs ~rhs ~shape:F64)
        ~lhs:(B.vec_greater_than c.builder ~lhs ~rhs ~shape:F64)
      |> B.vec_extract c.builder ~shape:`I32 ~lane:0
  | _ -> raise_comp_ops c

let translate_bit_test_comparison c arg =
  match c.opcode.id with
  | JAE -> B.equals_zero c.builder arg
  | _ -> raise_comp_ops c

let translate_condition c =
  assert_c c Poly.(c.opcode.prefix = opcode_prefix_none);
  assert_c c
    Status_flags.(
      equal none @@ (c.state.changed_flags %& flags_used_exn c.opcode.id))
    ~msg:"Flag has been modified";
  B.set_check_var_is_latest c.builder false;
  let result =
    let open X86reg in
    match (c.state.compare_instr, c.state.compare_args) with
    | TEST, [ ((_, #gpr_type) as lhs); ((_, #gpr_type) as rhs) ] ->
        translate_test_comparison c ~lhs ~rhs
    | ( (CMP | CMPSD | CMPSB),
        [ ((_, #gpr_type) as lhs); ((_, #gpr_type) as rhs) ] ) ->
        translate_cmp_comparison c ~lhs ~rhs
    | INVALID, [] -> translate_input_cond c
    | DEC, [ ((_, #gpr_type) as result) ] -> translate_dec_comparison c ~result
    | (ADD | ADC), [ ((_, #gpr_type) as lhs); _; ((_, #gpr_type) as result) ] ->
        translate_add_comparison c ~lhs ~result
    | ( (SUB | SBB),
        [
          ((_, #gpr_type) as lhs);
          ((_, #gpr_type) as rhs);
          ((_, #gpr_type) as result);
        ] ) ->
        translate_sub_comparison c ~lhs ~rhs ~result
    | INC, [ ((_, #gpr_type) as result) ] -> translate_inc_comparison c ~result
    | (OR | AND), [ ((_, #gpr_type) as result) ] ->
        translate_result_comparison c ~result ~cf_zero:true ~of_zero:true
    | ( SHR,
        [
          ((_, #gpr_type) as lhs);
          ((_, #gpr_type) as rhs);
          ((_, #gpr_type) as result);
        ] ) ->
        translate_shr_comparison c ~lhs ~rhs ~result
    | SAHF, [] -> translate_sahf_comparison c
    | NEG, [ ((_, #gpr_type) as result) ] -> translate_neg_comparison c ~result
    | (UCOMISD | COMISD), [ (lhs, `ignore); (rhs, `ignore) ] ->
        translate_ucomisd_comparison c ~lhs ~rhs
    | XOR, [ ((_, #gpr_type) as result) ] ->
        translate_result_comparison c ~result ~cf_zero:true ~of_zero:true
    | BT, [ (arg, `Reg32Bit) ] -> translate_bit_test_comparison c arg
    | _ -> raise_c c "Invalid comparison"
  in
  B.set_check_var_is_latest c.builder true;
  result

let translate_set_stuff c =
  match operands c with
  | [ dest ] -> store_operand c (translate_condition c) ~dest
  | _ -> raise_ops c

let translate_leave c =
  match operands c with
  | [] ->
      let esp =
        B.dup_var c.builder ~varName:(X86reg.to_ident `esp) Int
          (newest_var c `ebp)
      in
      B.load32 c.builder ~varName:(X86reg.to_ident `ebp) esp |> ignore;
      B.add c.builder ~varName:(X86reg.to_ident `esp) ~lhs:esp
        ~rhs:(B.const c.builder 4)
      |> ignore
  | _ -> raise_ops c

let translate_float_bi_op c (op : B.bi_op_add)
    (cond : [ `PopAfter | `IntArg | `None ]) =
  (match (cond, operands c) with
  | `None, [ lhs; rhs ] ->
      let lhs_arg = load_operand_f c lhs in
      let rhs_arg = load_operand_f c rhs in
      store_operand_f c ~dest:lhs @@ op c.builder ~lhs:lhs_arg ~rhs:rhs_arg
  | `IntArg, [ (Memory _ as arg) ] ->
      (* tecnically supposed to work with long too *)
      set_fpu_stack c
        (op c.builder
           ~rhs:(B.int32_to_float c.builder ~signed:true (load_operand_s c arg))
           ~lhs:(get_fpu_stack c 0))
        0
  | `PopAfter, [ arg ] ->
      store_operand_f c ~dest:arg
      @@ op c.builder ~rhs:(get_fpu_stack c 0) ~lhs:(load_operand_f c arg);
      fpu_pop c
  | `None, [ arg ] ->
      set_fpu_stack c
        (op c.builder ~rhs:(load_operand_f c arg) ~lhs:(get_fpu_stack c 0))
        0
  | _ -> raise_ops c);
  add_float_comparison c []

let translate_float_bi_op_r c (op : B.bi_op_add)
    (cond : [ `PopAfter | `IntArg | `None ]) =
  (match (cond, operands c) with
  | `None, [ dest; src ] ->
      let lhs = load_operand_f c src in
      let rhs = load_operand_f c dest in
      store_operand_f c ~dest @@ op c.builder ~lhs ~rhs
  | `IntArg, [ arg ] ->
      (* tecnically supposed to work with long too *)
      set_fpu_stack c
        (op c.builder ~rhs:(get_fpu_stack c 0)
           ~lhs:(B.int32_to_float c.builder ~signed:true (load_operand_s c arg)))
        0
  | `PopAfter, [ arg ] ->
      store_operand_f c ~dest:arg
      @@ op c.builder ~rhs:(load_operand_f c arg) ~lhs:(get_fpu_stack c 0);
      fpu_pop c
  | `None, [ arg ] ->
      set_fpu_stack c
        (op c.builder ~rhs:(get_fpu_stack c 0) ~lhs:(load_operand_f c arg))
        0
  | _ -> raise_ops c);
  add_float_comparison c []

let translate_imul c =
  match operands c with
  | [ rhs ] when operand_size rhs = 1 ->
      let lhs = Register { reg = `al; size = 1 } in
      let dest = Register { reg = `ax; size = 2 } in
      B.mul c.builder ~rhs:(load_operand_s c rhs) ~lhs:(load_operand_s c lhs)
      |> store_operand c ~dest
  | [ dest; lhs; rhs ] | [ (lhs as dest); rhs ] ->
      let lhs = load_operand_s c lhs in
      let rhs = load_operand_s c rhs in
      store_operand c ~dest @@ B.mul c.builder ~lhs ~rhs;
      add_comparison c []
  | _ -> raise_ops c

let translate_cdq c =
  match operands c with
  | [] ->
      B.shift_right c.builder ~varName:(X86reg.to_ident `edx)
        ~lhs:(newest_var c `eax) ~rhs:(B.const c.builder 31) ~signed:true
      |> ignore
  | _ -> raise_ops c

let translate_idiv c =
  match operands c with
  | [ divisor ]
    when operand_size divisor = 4
         &&
         (* this is a bit much just to detect cdq.
              maybe have an instruction to merge to ints into longs,
            and deal with this stuff during optimization *)
         match B.get_instr c.builder @@ newest_var c `edx with
         | SignedBiOp { op = ShiftRight; signed = true; rhs; _ } -> (
             match B.get_instr c.builder rhs with
             | Const (_, 31) -> true
             | _ -> false)
         | _ -> false ->
      let lhs = newest_var c `eax in
      let rhs = load_operand_s c divisor in
      B.set_check_var_is_latest c.builder false;
      B.div c.builder ~varName:(X86reg.to_ident `eax) ~lhs ~rhs ~signed:true
      |> ignore;
      B.remainder c.builder ~varName:(X86reg.to_ident `edx) ~lhs ~rhs
        ~signed:true
      |> ignore;
      B.set_check_var_is_latest c.builder true;
      add_comparison c []
  | _ -> raise_ops c

let translate_div c =
  match operands c with
  | [ divisor ] when operand_size divisor = 4 ->
      (* yeah this should be an optimization *)
      if instr_is_zeroed c @@ newest_var c `edx then (
        let lhs = newest_var c `eax in
        let rhs = load_operand_u c divisor in
        B.set_check_var_is_latest c.builder false;
        B.div c.builder ~varName:(X86reg.to_ident `eax) ~lhs ~rhs ~signed:false
        |> ignore;
        B.remainder c.builder ~varName:(X86reg.to_ident `edx) ~lhs ~rhs
          ~signed:false
        |> ignore;
        B.set_check_var_is_latest c.builder true;
        add_comparison c [])
      else
        let low_lhs =
          B.int32_to_long c.builder (newest_var c `eax) ~signed:false
        in
        let high_lhs =
          B.int32_to_long c.builder (newest_var c `edx) ~signed:false
        in
        let lhs =
          B.long_or c.builder ~lhs:low_lhs
            ~rhs:
              (B.long_shift_left c.builder ~lhs:high_lhs
                 ~rhs:(B.long_const c.builder 32L))
        in
        let rhs =
          B.int32_to_long c.builder (load_operand_u c divisor) ~signed:false
        in
        B.long_to_int32 c.builder ~varName:(X86reg.to_ident `eax)
          (B.long_div c.builder ~lhs ~rhs ~signed:false)
        |> ignore;
        B.long_to_int32 c.builder ~varName:(X86reg.to_ident `edx)
          (B.long_remainder c.builder ~lhs ~rhs ~signed:false)
        |> ignore;
        add_comparison c []
  | _ -> raise_ops c

let translate_float_int_load c =
  match operands c with
  | [ arg ] ->
      (fpu_push c
      @@
      match arg with
      | Memory ({ size = 8; _ } as src) ->
          let addr, offset = load_partial_address c src in
          (* why is this named this? *)
          B.int64_to_float c.builder
            (B.long_load64 c.builder addr ~offset)
            ~signed:true
      | _ -> B.int32_to_float c.builder (load_operand_s c arg) ~signed:true);
      add_float_comparison c []
  | _ -> raise_ops c

let translate_float_int_store c ~pop_after =
  (match operands c with
  | [ (Memory { size = 8; _ } as dest) ] ->
      B.float_to_long c.builder (get_fpu_stack c 0) |> store_operand_l c ~dest
  | [ (Memory { size = 4; _ } as dest) ] ->
      B.float_to_int32 c.builder (get_fpu_stack c 0) |> store_operand c ~dest
  | _ -> raise_ops c);

  if pop_after then fpu_pop c

let translate_rep_stos c =
  assert_c c (not c.state.backward_direction) ~msg:"must go forward";
  match operands c with
  | [
   Memory
     {
       size = size1;
       segment = Some Es;
       base = Some `edi;
       scale = 1;
       displacement = 0;
       index = None;
     };
   Register { size = size2; reg = `eax | `al };
  ]
    when size1 = size2 ->
      (match size1 with
      | 4 ->
          if instr_is_zeroed c @@ newest_var c `eax then
            (* we just have to fill zeroes *)
            let count =
              B.shift_left c.builder ~rhs:(B.const c.builder 2)
                ~lhs:(newest_var c `ecx)
            in
            B.memset c.builder ~count ~dest:(newest_var c `edi)
              ~value:(B.const c.builder 0)
          else
            B.call c.builder Util.int_memset_func
              [ newest_var c `edi; newest_var c `eax; newest_var c `ecx ]
      | 1 ->
          B.memset c.builder ~dest:(newest_var c `edi)
            ~value:(newest_var c `eax) ~count:(newest_var c `ecx)
      | _ -> raise_ops c);
      B.const c.builder ~varName:(X86reg.to_ident `ecx) 0 |> ignore
  | _ -> raise_ops c

let translate_rep_movs c =
  match operands c with
  | [
   Memory
     {
       size = (4 | 2 | 1) as size1;
       segment = Some Es;
       base = Some `edi;
       scale = 1;
       displacement = 0;
       index = None;
     };
   Memory
     {
       size = (4 | 2 | 1) as size2;
       segment = None;
       base = Some `esi;
       scale = 1;
       displacement = 0;
       index = None;
     };
  ]
    when size1 = size2 ->
      let count = newest_var c `ecx in
      let[@warning "-8"] count =
        match size1 with
        | 4 -> B.shift_left c.builder ~rhs:(B.const c.builder 2) ~lhs:count
        | 2 -> B.shift_left c.builder ~rhs:(B.const c.builder 1) ~lhs:count
        | 1 -> count
      in
      if c.state.backward_direction then (
        let amount = B.sub c.builder ~rhs:(B.const c.builder 1) ~lhs:count in
        B.sub c.builder ~varName:(X86reg.to_ident `esi) ~rhs:amount
          ~lhs:(newest_var c `esi)
        |> ignore;
        B.sub c.builder ~varName:(X86reg.to_ident `edi) ~rhs:amount
          ~lhs:(newest_var c `edi)
        |> ignore);

      B.memcopy c.builder ~dest:(newest_var c `edi) ~src:(newest_var c `esi)
        ~count;

      if c.state.backward_direction then (
        B.sub c.builder ~varName:(X86reg.to_ident `esi)
          ~rhs:(B.const c.builder 1) ~lhs:(newest_var c `esi)
        |> ignore;
        B.sub c.builder ~varName:(X86reg.to_ident `edi)
          ~rhs:(B.const c.builder 1) ~lhs:(newest_var c `edi)
        |> ignore)
      else (
        B.add c.builder ~varName:(X86reg.to_ident `esi) ~rhs:count
          ~lhs:(newest_var c `esi)
        |> ignore;
        B.add c.builder ~varName:(X86reg.to_ident `edi) ~rhs:count
          ~lhs:(newest_var c `edi)
        |> ignore);
      B.const c.builder ~varName:(X86reg.to_ident `ecx) 0 |> ignore
  | _ -> raise_ops c

let translate_repe_cmpsd c =
  assert_c c (not c.state.backward_direction) ~msg:"must go forward";
  match operands c with
  | [
   (Memory
      {
        base = Some `esi;
        displacement = 0;
        scale = 1;
        index = None;
        segment = None;
        size = size1;
      } as arg1);
   (Memory
      {
        base = Some `edi;
        displacement = 0;
        scale = 1;
        index = None;
        segment = Some Es;
        size = size2;
      } as arg2);
  ]
    when size1 = size2 ->
      let esi = newest_var c `esi in
      let edi = newest_var c `edi in
      let ecx = newest_var c `ecx in
      let func =
        match size1 with
        | 1 -> Util.byte_diff_func
        | 4 -> Util.int_diff_func
        | _ -> raise_ops c
      in
      B.call c.builder func [ esi; edi; ecx ];
      B.returned c.builder ~varName:(X86reg.to_ident `esi) Int |> ignore;
      B.returned c.builder ~varName:(X86reg.to_ident `edi) Int |> ignore;
      B.returned c.builder ~varName:(X86reg.to_ident `ecx) Int |> ignore;
      (* make the comparison values the last value*)
      (* will get deleted by opts anyway *)
      let arg1 = load_operand_typed c arg1 in
      let arg2 = load_operand_typed c arg2 in
      add_comparison c [ arg1; arg2 ]
  | _ -> raise_ops c

let translate_float_abs c =
  assert_c c (List.is_empty @@ operands c);
  set_fpu_stack c (B.float_abs c.builder (get_fpu_stack c 0)) 0;
  add_float_comparison c []

let translate_dec c =
  match operands c with
  | [ arg ] ->
      let result =
        B.sub c.builder ~rhs:(B.const c.builder 1) ~lhs:(load_operand c arg)
      in
      store_operand c ~dest:arg result;
      add_comparison c [ (result, loaded_reg_type arg) ]
  | _ -> raise_ops c

let translate_add_carry c =
  match operands c with
  | [ dest; src ] ->
      (* the fact that one load_operand is signed is intentional *)
      let lhs =
        B.add c.builder ~rhs:(load_operand_s c src) ~lhs:(translate_condition c)
      in
      let rhs = load_operand c dest in
      let result = B.add c.builder ~lhs ~rhs in
      store_operand c ~dest @@ result;
      let loaded_type = loaded_reg_type dest in
      add_comparison c
        [ (lhs, loaded_type); (rhs, loaded_type); (result, loaded_type) ]
  | _ -> raise_ops c

let translate_subtract_borrow c =
  match operands c with
  | [ dest; src ] ->
      (* TODO maybe changing associativity to help in the reflexive case *)
      let rhs =
        B.add c.builder ~rhs:(load_operand_s c src) ~lhs:(translate_condition c)
      in
      let lhs = load_operand c dest in
      let result = B.sub c.builder ~lhs ~rhs in
      store_operand c ~dest result;
      let loaded_type = loaded_reg_type dest in
      add_comparison c
        [ (lhs, loaded_type); (rhs, loaded_type); (result, loaded_type) ]
  | _ -> raise_ops c

let translate_move_data c =
  match operands c with
  | [
   (Memory
      {
        size = (4 | 2 | 1) as size1;
        segment = Some Es;
        base = Some `edi;
        scale = 1;
        displacement = 0;
        index = None;
      } as dest);
   (Memory
      {
        size = (4 | 2 | 1) as size2;
        segment = None;
        base = Some `esi;
        scale = 1;
        displacement = 0;
        index = None;
      } as src);
  ]
    when size1 = size2 ->
      store_operand c ~dest @@ load_operand c src;
      let change = if c.state.backward_direction then B.sub else B.add in
      change c.builder ~varName:(X86reg.to_ident `edi) ~lhs:(newest_var c `edi)
        ~rhs:(B.const c.builder size1)
      |> ignore;
      change c.builder ~varName:(X86reg.to_ident `esi) ~lhs:(newest_var c `esi)
        ~rhs:(B.const c.builder size1)
      |> ignore
  | [ (Register { size = 16; _ } as dest); (Register { size = 16; _ } as src) ]
    ->
      let high = load_operand_v c dest in
      let low =
        load_operand_v c src |> B.vec_extract c.builder ~shape:`I64 ~lane:0
      in
      B.vec_replace c.builder ~dest:high ~value:low ~shape:`I64 ~lane:0
      (* B.vec_shuffle c.builder ~vec2:(load_operand_v c src) *)
      (* ~vec1:(load_operand_v c dest) ~ctrl_l:0x07_06_05_04_03_02_01_00L *)
      (* ~ctrl_h:0x1F_1E_1D_1C_1B_1A_19_18L *)
      |> store_operand_v c ~dest
  | _ -> raise_ops c

let translate_float_sqrt c =
  assert_c c (List.is_empty @@ operands c);
  let result =
    B.call c.builder Util.float_sqrt_func [ get_fpu_stack c 0 ];
    B.returned c.builder Float
  in
  set_fpu_stack c result 0;
  add_float_comparison c []

let translate_float_load_constant c const =
  assert_c c (List.is_empty @@ operands c);
  fpu_push c @@ B.float_const c.builder const;
  add_float_comparison c []

let translate_float_sin_cos c =
  let angle = fpu_pop_off c in
  let sin =
    B.call c.builder Util.float_sine_func [ angle ];
    B.returned c.builder Float
  in
  let cos =
    B.call c.builder Util.float_cosine_func [ angle ];
    B.returned c.builder Float
  in
  fpu_push c sin;
  fpu_push c cos;
  add_float_comparison c []

let translate_float_negate c =
  set_fpu_stack c (B.float_neg c.builder (get_fpu_stack c 0)) 0;
  add_float_comparison c []

let translate_negate c =
  match operands c with
  | [ arg ] ->
      let result =
        B.sub c.builder ~lhs:(B.const c.builder 0) ~rhs:(load_operand_s c arg)
      in
      store_operand c ~dest:arg result;
      add_comparison c [ (result, loaded_reg_type arg) ]
  | _ -> raise_ops c

let translate_exchange c =
  match operands c with
  | [ lhs; rhs ] when operand_size lhs = operand_size rhs ->
      let lhs_value = load_operand c lhs in
      let rhs_value = load_operand c rhs in
      store_operand c ~dest:lhs @@ rhs_value;
      B.set_check_var_is_latest c.builder false;
      store_operand c ~dest:rhs @@ lhs_value;
      B.set_check_var_is_latest c.builder true
  | _ -> raise_ops c

let translate_float_exchange c =
  match operands c with
  | [ lhs; rhs ] ->
      let lhs_value = load_operand_f c lhs in
      let rhs_value = load_operand_f c rhs in
      store_operand_f c ~dest:lhs rhs_value;
      store_operand_f c ~dest:rhs lhs_value;
      add_float_comparison c []
  | _ -> raise_ops c

let translate_float_scale c =
  assert_c c (List.is_empty @@ operands c);
  let result =
    B.call c.builder Util.float_scale_func
      [ get_fpu_stack c 0; get_fpu_stack c 1 ];
    B.returned c.builder Float
  in
  set_fpu_stack c result 0;
  add_float_comparison c []

let translate_sahf c =
  assert_c c (List.is_empty @@ operands c);
  assert_c c
    (Instr.Ref.equal c.state.fpu_status_word @@ newest_var c `eax)
    ~msg:"fpu status word must be in eax";
  (* this only works because we only allow doing fnstsw once in a block*)
  add_comparison c []

let translate_store_data c =
  assert_c c (not c.state.backward_direction) ~msg:"must go forward";
  match operands c with
  | [
   (Memory
      {
        size = (4 | 2 | 1) as size1;
        base = Some `edi;
        index = None;
        segment = Some Es;
        scale = 1;
        displacement = 0;
      } as dest);
   (Register { size = (4 | 2 | 1) as size2; reg = `eax | `ax | `al } as src);
  ]
    when size1 = size2 ->
      store_operand c ~dest @@ load_operand c src;
      B.add c.builder ~varName:(X86reg.to_ident `edi)
        ~rhs:(B.const c.builder size1) ~lhs:(newest_var c `edi)
      |> ignore
  | _ -> raise_ops c

let translate_mul c =
  match operands c with
  | [ rhs ] when operand_size rhs = 4 ->
      let lhs = newest_var c `eax in
      let lhs_l = B.int32_to_long c.builder lhs ~signed:false in
      let rhs = load_operand_u c rhs in
      let rhs_l = B.int32_to_long c.builder rhs ~signed:false in
      let res = B.long_mul c.builder ~lhs:lhs_l ~rhs:rhs_l in
      B.long_to_int32 c.builder ~varName:(X86reg.to_ident `eax) res |> ignore;
      B.long_to_int32 c.builder ~varName:(X86reg.to_ident `edx)
        (B.long_shift_right c.builder ~lhs:res
           ~rhs:(B.long_const c.builder 32L)
           ~signed:false)
      |> ignore;
      add_comparison c []
  | _ -> raise_ops c

let translate_rotate_cl_right c =
  match operands c with
  | [ op; Immediate { value = 1; _ } ] when operand_size op = 4 ->
      (* expecting only the MSB to be set/unset *)
      let new_bit = translate_condition c in
      (* size is 4, doens't matter *)
      let op_value = load_operand c op in
      let shifted =
        B.shift_right c.builder ~signed:false ~rhs:(B.const c.builder 1)
          ~lhs:op_value
      in
      store_operand c ~dest:op @@ B.int_or c.builder ~lhs:new_bit ~rhs:shifted;
      add_comparison c [ (op_value, `Reg32Bit) ]
  | _ -> raise_ops c

let translate_not c =
  match operands c with
  | [ arg ] ->
      store_operand c ~dest:arg
      @@ B.xor c.builder
           ~rhs:(B.const c.builder 0xFFFFFF)
           ~lhs:(load_operand c arg)
  | _ -> raise_ops c

let translate_repne_scas c =
  assert_c c (not c.state.backward_direction) ~msg:"must go forward";
  match operands c with
  | [
   (Register { reg = `al; _ } as needle);
   Memory
     {
       base = Some `edi;
       segment = Some Es;
       index = None;
       scale = 1;
       displacement = 0;
       size = 1;
     };
  ] ->
      let needle = load_operand_u c needle in
      let haystack = newest_var c `edi in
      let length = newest_var c `ecx in
      B.call c.builder Util.find_byte_func [ needle; haystack; length ];
      B.returned c.builder ~varName:(X86reg.to_ident `ecx) Int |> ignore;
      add_comparison c []
  | _ -> raise_ops c

let translate_shift_left_two c =
  match operands c with
  | [
   (Register { size = 4; _ } as dest);
   (Register { size = 4; _ } as src);
   Immediate { value = count; _ };
  ]
    when count < 32 ->
      let low_part =
        B.shift_left c.builder ~rhs:(B.const c.builder count)
          ~lhs:(load_operand_u c dest)
      in
      let high_part =
        B.shift_right c.builder
          ~rhs:(B.const c.builder @@ (32 - count))
          ~lhs:(load_operand_u c src) ~signed:false
      in
      store_operand c ~dest @@ B.int_or c.builder ~rhs:high_part ~lhs:low_part;
      add_comparison c []
  | _ -> raise_ops c

let translate_float_round c =
  assert_c c (List.is_empty @@ operands c);
  set_fpu_stack c (B.float_round c.builder (get_fpu_stack c 0)) 0;
  add_float_comparison c []

let translate_store_fpu_cw c =
  match operands c with
  | [ dest ] when operand_size dest = 2 ->
      store_operand c ~dest @@ B.const c.builder 0x27f
  | _ -> raise_ops c

let translate_store_mxcsr c =
  match operands c with
  | [ dest ] when operand_size dest = 4 ->
      store_operand c ~dest @@ B.const c.builder 0x00001F80
  | _ -> raise_ops c

let translate_partial_tan c =
  assert_c c (List.is_empty @@ operands c);
  let st0 = get_fpu_stack c 0 in
  B.call c.builder Util.float_tan_func [ st0 ];
  set_fpu_stack c (B.returned c.builder Float) 0;
  translate_float_load_constant c 1.0;
  add_float_comparison c [ st0 ]

let translate_float_remainder c kind =
  assert_c c (List.is_empty @@ operands c);
  let st0 = get_fpu_stack c 0 in
  let st1 = get_fpu_stack c 1 in
  B.call c.builder
    (match kind with
    | `Mod -> Util.float_mod_func
    | `Remainder -> Util.float_rem_func)
    [ st0; st1 ];
  let result = B.returned c.builder Float in
  set_fpu_stack c result 0;
  add_float_comparison c []

let translate_float_sin c =
  assert_c c (List.is_empty @@ operands c);
  let arg = get_fpu_stack c 0 in
  B.call c.builder Util.float_sine_func [ arg ];
  set_fpu_stack c (B.returned c.builder Float) 0;
  add_float_comparison c [ arg ]

let translate_float_cos c =
  assert_c c (List.is_empty @@ operands c);
  let arg = get_fpu_stack c 0 in
  B.call c.builder Util.float_cosine_func [ arg ];
  set_fpu_stack c (B.returned c.builder Float) 0;
  add_float_comparison c [ arg ]

let translate_movq c =
  match operands c with
  | [ dest; src ] -> (
      match (operand_size dest, operand_size src) with
      | 8, 16 ->
          store_operand_l c ~dest
          @@ B.vec_extract c.builder ~shape:`I64 ~lane:0
          @@ load_operand_v c src
      | 16, 8 ->
          let zero = B.vec_const c.builder ~l:0L ~h:0L in
          let src_value = load_operand_l c src in
          store_operand_v c ~dest
          @@ B.vec_replace c.builder ~dest:zero ~value:src_value ~shape:`I64
               ~lane:0
      | 8, 8 -> load_operand_mmx c src |> store_operand_mmx c ~dest
      | 16, 16 ->
          B.vec_replace c.builder
            ~value:
              (B.vec_extract c.builder (load_operand_v c src) ~shape:`F64
                 ~lane:0)
            ~dest:(load_operand_v c dest) ~shape:`F64 ~lane:0
          |> store_operand_v c ~dest
      | _ -> raise_ops c)
  | _ -> raise_ops c

let translate_movapd c =
  match operands c with
  | [ dest; src ] -> store_operand_v c ~dest @@ load_operand_v c src
  | _ -> raise_ops c

let translate_vec_shift_64 c (f : B.bi_op_add) =
  match operands c with
  | [ dest; src ] when operand_size src = 16 ->
      let lhs = load_operand_v c dest in
      f c.builder ~lhs
        ~rhs:
          (B.vec_extract c.builder ~shape:`I32 ~lane:0 @@ load_operand_v c src)
      |> store_operand_v c ~dest
  | [ dest; Immediate { size = 1; value } ] when operand_size dest = 16 ->
      let lhs = load_operand_v c dest in
      f c.builder ~rhs:(B.const c.builder value) ~lhs |> store_operand_v c ~dest
  | [ dest; Immediate { size = 1; value } ] when operand_size dest = 8 ->
      let lhs = load_operand_mmx c dest in
      f c.builder ~rhs:(B.const c.builder value) ~lhs
      |> store_operand_mmx c ~dest
  | _ -> raise_ops c

let translate_movd c =
  match operands c with
  | [ dest; src ] -> (
      match (operand_size dest, operand_size src) with
      | 4, 16 ->
          store_operand c ~dest
          @@ B.vec_extract c.builder ~shape:`I32 ~lane:0
          @@ load_operand_v c src
      | 8, 4 ->
          B.int32_to_long c.builder (load_operand c src) ~signed:false
          |> B.vec_splat c.builder ~shape:`I64
          |> store_operand_mmx c ~dest
      | 4, 8 ->
          load_operand_mmx c src
          |> B.vec_extract c.builder ~lane:0 ~shape:`I32
          |> store_operand c ~dest
      | 16, 4 ->
          B.vec_replace c.builder ~value:(load_operand c src)
            ~dest:(load_operand_v c dest) ~shape:`I32 ~lane:0
          |> store_operand_v c ~dest
      | _ -> raise_ops c)
  | _ -> raise_ops c

let translate_vec_bi_op c (f : B.bi_op_add) =
  match operands c with
  | [ dest; src ] -> (
      match (operand_size dest, operand_size src) with
      | 16, 16 ->
          store_operand_v c ~dest
          @@ f c.builder ~rhs:(load_operand_v c src)
               ~lhs:(load_operand_v c dest)
      | 8, 8 ->
          f c.builder ~rhs:(load_operand_mmx c src)
            ~lhs:(load_operand_mmx c dest)
          |> store_operand_mmx c ~dest
      | _ -> raise_ops c)
  | _ -> raise_ops c

let translate_ucomisd c =
  match operands c with
  | [ lhs; rhs ] ->
      let lhs = load_operand_v c lhs in
      let rhs = load_operand_v c rhs in
      add_comparison c [ (lhs, `ignore); (rhs, `ignore) ]
  | _ -> raise_ops c

let translate_pxor c =
  match operands c with
  | [ lhs; rhs ] when equal_operand lhs rhs -> (
      match operand_size lhs with
      | 16 -> B.vec_const c.builder ~l:0L ~h:0L |> store_operand_v c ~dest:lhs
      | 8 -> B.vec_const c.builder ~l:0L ~h:0L |> store_operand_mmx c ~dest:lhs
      | _ -> raise_ops c)
  | _ -> translate_vec_bi_op c B.vec_xor

(* works for mm stuff as long as ctrl_l doesn't reference bytes >7 *)
let translate_punpack c ?mm ?vec2_zeroed ?(allow_mm = true) ctrl =
  match operands c with
  | [ lhs; rhs ] when allow_mm && operand_size lhs = 8 -> (
      let vec1 = load_operand_mmx c lhs in
      let vec2 =
        match rhs with
        | Memory { size = 4; _ } ->
            (* gotta hit both the low and high order parts *)
            load_operand c rhs |> B.vec_splat c.builder ~shape:`I32
        | _ -> load_operand_mmx c rhs
      in
      match vec2_zeroed with
      | Some f when instr_is_vec_zeroed c vec2 ->
          f c.builder vec1 |> store_operand_mmx c ~dest:lhs
      | _ ->
          let ctrl_l = Option.value mm ~default:(snd ctrl) in
          B.vec_shuffle c.builder ~vec1 ~vec2 ~ctrl_l ~ctrl_h:0L
          |> store_operand_mmx c ~dest:lhs)
  | [ lhs; rhs ] when operand_size lhs = 16 -> (
      let vec1 = load_operand_v c lhs in
      let vec2 = load_operand_v c rhs in
      match vec2_zeroed with
      | Some f when instr_is_vec_zeroed c vec2 ->
          f c.builder vec1 |> store_operand_v c ~dest:lhs
      | _ ->
          let ctrl_h, ctrl_l = ctrl in
          B.vec_shuffle c.builder ~vec1 ~vec2 ~ctrl_l ~ctrl_h
          |> store_operand_v c ~dest:lhs)
  | _ -> raise_ops c

let translate_f2xm1 c =
  assert_c c (List.is_empty @@ operands c);
  B.call c.builder Util.float_pow_func [ get_fpu_stack c 0 ];
  set_fpu_stack c (B.returned c.builder Float) 0

let translate_move_low c =
  match operands c with
  | [ lhs; Memory ({ size = 8; _ } as mem) ] ->
      let vec = load_operand_v c lhs in
      let addr, offset = load_partial_address c mem in
      B.vec_load c.builder ~dest:vec ~addr ~offset ~shape:`I64 ~lane:0
      |> store_operand_v c ~dest:lhs
  | [ Memory ({ size = 8; _ } as mem); src ] ->
      let vec = load_operand_v c src in
      let addr, offset = load_partial_address c mem in
      B.vec_store c.builder ~vec ~addr ~offset ~shape:`I64 ~lane:0
  | _ -> raise_ops c

let translate_vec_extract c shape =
  match operands c with
  | [ dest; src; Immediate { size = 1; value = lane } ] ->
      load_operand_v c src
      |> B.vec_extract c.builder ~lane ~shape
      |> store_operand c ~dest
  | _ -> raise_ops c

let translate_vec_insert c shape =
  match operands c with
  | [ dest; src; Immediate { size = 1; value = lane } ] ->
      B.vec_replace c.builder ~value:(load_operand c src)
        ~dest:(load_operand_v c dest) ~lane ~shape
      |> store_operand_v c ~dest
  | _ -> raise_ops c

let translate_vec_bottom_64 c f =
  let dest, lhs, rhs =
    match operands c with
    | [ dest; Memory ({ size = 8; _ } as mem) ] ->
        let lhs = load_operand_v c dest in
        let addr, offset = load_partial_address c mem in
        let rhs = B.vec_load64_zero_extend c.builder addr ~offset in
        (dest, lhs, rhs)
    | [ dest; rhs ] ->
        let lhs = load_operand_v c dest in
        let rhs = load_operand_v c rhs in
        (dest, lhs, rhs)
    | _ -> raise_ops c
  in
  let result = f ?varName:None c.builder ~lhs ~rhs in
  (* B.vec_shuffle c.builder ~vec1:result ~vec2:lhs *)
  (* ~ctrl_l:0x07_06_05_04_03_02_01_00L ~ctrl_h:0x1F_1E_1D_1C_1B_1A_19_18L *)
  let top_part = B.vec_extract c.builder lhs ~shape:`I64 ~lane:1 in
  B.vec_replace c.builder ~dest:result ~value:top_part ~shape:`I64 ~lane:1
  |> store_operand_v c ~dest

let translate_cvtdq2pd c =
  match operands c with
  | [ dest; src ] ->
      B.vec_convert_low_32bits_to_float_signed c.builder (load_operand_v c src)
      |> store_operand_v c ~dest
  | _ -> raise_ops c

let translate_vec_convert_low_f64_to_i32 c =
  match operands c with
  | [ dest; src ] when operand_size dest = 4 ->
      load_operand_v c src
      |> B.vec_extract c.builder ~shape:`F64 ~lane:0
      |> B.float_to_int32 c.builder |> store_operand c ~dest
  | _ -> raise_ops c

let translate_vec_shuffle_dwords c =
  match operands c with
  | [ dest; src; Immediate { value = order; size = 1 } ] ->
      let[@warning "-8"] order_to_ctrl o =
        match o land 3 with
        | 0 -> 0x03_02_01_00L
        | 1 -> 0x07_06_05_04L
        | 2 -> 0x0B_0A_09_08L
        | 3 -> 0x0F_0E_0D_0CL
      and combine l h = Int64.bit_or l (Int64.shift_left h 32) in
      let ctrl_l = combine (order_to_ctrl order) (order_to_ctrl @@ (order lsr 2))
      and ctrl_h =
        combine (order_to_ctrl @@ (order lsr 4)) (order_to_ctrl @@ (order lsr 6))
      in
      B.vec_shuffle c.builder
        ~vec2:(B.vec_const c.builder ~l:0L ~h:0L)
        ~vec1:(load_operand_v c src) ~ctrl_l ~ctrl_h
      |> store_operand_v c ~dest
  | _ -> raise_ops c

let translate_vec_shuffle_doubles c =
  match operands c with
  | [ dest; src; Immediate { value = order; size = 1 } ] ->
      let ctrl_l =
        if order land 1 = 0 then 0x07_06_05_04_03_02_01_00L
        else 0x0F_0E_0D_0C_0B_0A_09_08L
      in
      let ctrl_h =
        if order land 2 = 0 then 0x17_16_15_14_13_12_11_10L
        else 0x1F_1E_1D_1C_1B_1A_19_18L
      in
      B.vec_shuffle c.builder ~vec2:(load_operand_v c src)
        ~vec1:(load_operand_v c dest) ~ctrl_l ~ctrl_h
      |> store_operand_v c ~dest
  | _ -> raise_ops c

let translate_vec_bitmask c =
  match operands c with
  | [ dest; src ] ->
      load_operand_v c src
      |> B.vec_int8_sign_bitmask c.builder
      |> store_operand c ~dest
  | _ -> raise_ops c

let translate_fxam c =
  assert_c c (List.is_empty @@ operands c);
  add_float_comparison c [ get_fpu_stack c 0 ]

let translate_rotate_left c =
  match operands c with
  | [ op; Immediate { value; size = 1 } ] ->
      B.rotate_left c.builder ~rhs:(B.const c.builder value)
        ~lhs:(load_operand c op)
      |> store_operand c ~dest:op
  | _ -> raise_ops c

let translate_xlatb c =
  assert_c c (List.is_empty @@ operands c);
  let ebx = load_operand c @@ Register { reg = `ebx; size = 4 } in
  let al = load_operand c @@ Register { reg = `al; size = 1 } in
  B.add c.builder ~lhs:ebx ~rhs:al
  |> B.load8 c.builder ~signed:false
  |> store_operand c ~dest:(Register { reg = `al; size = 1 })

let translate_bit_test c kind =
  match operands c with
  | [ dest; src ] when operand_size dest = 4 -> (
      let prev = load_operand c dest in
      let one = B.const c.builder 1 in
      let count = load_operand c src in
      let masked =
        B.int_and c.builder ~rhs:(B.const c.builder 0x1F) ~lhs:count
      in
      let bit = B.shift_left c.builder ~lhs:one ~rhs:masked in
      match kind with
      | `Set ->
          B.int_or c.builder ~lhs:prev ~rhs:bit |> store_operand c ~dest;
          add_comparison c []
      | `Test ->
          add_comparison c
            [ (B.int_and c.builder ~lhs:prev ~rhs:bit, `Reg32Bit) ])
  | _ -> raise_ops c

let translate_vec_pack c (f : Builder.bi_op_add) =
  match operands c with
  | [ dest; src ] -> (
      match (operand_size dest, operand_size src) with
      | 8, 8 ->
          let lhs = load_operand_mmx c dest in
          let rhs = load_operand_mmx c src in
          let packed = f c.builder ~rhs ~lhs in
          let fixed =
            B.vec_shuffle c.builder ~vec1:packed ~vec2:rhs
              ~ctrl_h:0x0404040404040404L ~ctrl_l:0x0B0A090803020100L
          in
          store_operand_mmx c fixed ~dest
      | 16, 16 ->
          store_operand_mmx c ~dest
          @@ f c.builder ~rhs:(load_operand_v c src)
               ~lhs:(load_operand_v c dest)
      | _ -> raise_ops c)
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
  | SHL -> translate_shift_left c
  | SAR -> translate_shift_right c ~signed:true
  | SHR -> translate_shift_right c ~signed:false
  | INC -> translate_inc c
  | SETG | SETGE | SETL | SETLE | SETE | SETNE | SETA | SETAE | SETB | SETBE ->
      translate_set_stuff c
  | LEA -> translate_lea c
  | FLD -> translate_float_load c
  | FCOM -> translate_float_comparison c ~after_pop:`None
  | FCOMP -> translate_float_comparison c ~after_pop:`Once
  | FCOMPP -> translate_float_comparison c ~after_pop:`Twice
  | FST -> translate_float_store c ~after_pop:false
  | FSTP -> translate_float_store c ~after_pop:true
  | FNSTSW -> translate_float_store_status_word c
  | CALL -> translate_call c ~nontail:true
  (*TODO: there is currently no way to tell FADD from FADDP
    https://github.com/capstone-engine/capstone/issues/1456 *)
  | FADD -> translate_float_bi_op c B.float_add `None
  | FADDP -> translate_float_bi_op c B.float_add `PopAfter
  | FSUB -> translate_float_bi_op c B.float_sub `None
  | FSUBR -> translate_float_bi_op_r c B.float_sub `None
  | FSUBP -> translate_float_bi_op c B.float_sub `PopAfter
  | FSUBRP -> translate_float_bi_op_r c B.float_sub `PopAfter
  | FISUB -> translate_float_bi_op c B.float_sub `IntArg
  | FMUL -> translate_float_bi_op c B.float_mul `None
  | FMULP -> translate_float_bi_op c B.float_mul `PopAfter
  | FIMUL -> translate_float_bi_op c B.float_mul `IntArg
  | LEAVE -> translate_leave c
  | IMUL -> translate_imul c
  | CDQ -> translate_cdq c
  | IDIV -> translate_idiv c
  | FDIV -> translate_float_bi_op c B.float_div `None
  | FDIVP -> translate_float_bi_op c B.float_div `PopAfter
  | FDIVR -> translate_float_bi_op_r c B.float_div `None
  | FDIVRP -> translate_float_bi_op_r c B.float_div `PopAfter
  | FIDIV -> translate_float_bi_op c B.float_div `IntArg
  | FIDIVR -> translate_float_bi_op_r c B.float_div `IntArg
  | FILD -> translate_float_int_load c
  | FIADD -> translate_float_bi_op c B.float_add `IntArg
  | DIV -> translate_div c
  | FABS -> translate_float_abs c
  | CLC ->
      c.state.changed_flags <- Status_flags.(c.state.changed_flags %| carry)
  | DEC -> translate_dec c
  | FISTP -> translate_float_int_store c ~pop_after:true
  | ADC -> translate_add_carry c
  | SBB -> translate_subtract_borrow c
  | MOVSD | MOVSW | MOVSB -> translate_move_data c
  | FSQRT -> translate_float_sqrt c
  | FLDZ -> translate_float_load_constant c 0.0
  | FSINCOS -> translate_float_sin_cos c
  | FSIN -> translate_float_sin c
  | FCOS -> translate_float_cos c
  | FCHS -> translate_float_negate c
  | FLD1 -> translate_float_load_constant c 1.0
  | NEG -> translate_negate c
  | XCHG -> translate_exchange c
  | FXCH -> translate_float_exchange c
  | WAIT ->
      ()
      (* technically this should add a comparison, but the executible seems to think it doesn't *)
  | FFREE -> (* i don't care? *) add_float_comparison c []
  | FLDCW -> assert_c c (List.length (operands c) = 1)
  | FSCALE -> translate_float_scale c
  | SAHF -> translate_sahf c
  | CLD -> c.state.backward_direction <- false
  | STD -> c.state.backward_direction <- true
  | STOSD | STOSW | STOSB -> translate_store_data c
  | MUL -> translate_mul c
  | RCR -> translate_rotate_cl_right c
  | NOT -> translate_not c
  | SHLD -> translate_shift_left_two c
  | FRNDINT -> translate_float_round c
  | FNSTCW -> translate_store_fpu_cw c
  | FPTAN -> translate_partial_tan c
  | FPREM1 -> translate_float_remainder c `Remainder
  | STMXCSR -> translate_store_mxcsr c
  | MOVQ -> translate_movq c
  | MOVAPD -> translate_movapd c
  | PSLLQ -> translate_vec_shift_64 c (B.vec_shift_left ~shape:`I64)
  | PSRLQ ->
      translate_vec_shift_64 c (B.vec_shift_right ~shape:`I64 ~signed:false)
  | MOVD -> translate_movd c
  | PAND | ANDPD -> translate_vec_bi_op c B.vec_and
  | PSUBD -> translate_vec_bi_op c (B.vec_sub ~shape:`I32)
  | UCOMISD -> translate_ucomisd c
  (* the signed doesn't do anything cause the lane is a float. gadts would fix this *)
  | CMPLTPD -> translate_vec_bi_op c (B.vec_less_than ~shape:F64)
  | POR | ORPD -> translate_vec_bi_op c B.vec_or
  | XORPD | PXOR -> translate_pxor c
  | PUNPCKLBW ->
      translate_punpack c
        (0x13_03_12_02_11_01_10_00L, 0x17_07_16_06_15_05_14_04L)
        ~vec2_zeroed:
          (B.vec_extend ~shape:`I8 ~signed:false ~half_used:`LowOrder)
  | PUNPCKLWD ->
      translate_punpack c
        (0x17_16_07_06_15_14_05_04L, 0x13_12_03_02_11_10_01_00L)
        ~vec2_zeroed:
          (B.vec_extend ~shape:`I16 ~signed:false ~half_used:`LowOrder)
  | PSUBSW -> translate_vec_bi_op c (B.vec_sub_sat ~shape:I16 ~signed:true)
  | PMADDWD -> translate_vec_bi_op c B.vec_mul_add_16bit
  | PSLLW -> translate_vec_shift_64 c (B.vec_shift_left ~shape:`I16)
  | PSLLD -> translate_vec_shift_64 c (B.vec_shift_left ~shape:`I32)
  | PSRLW ->
      translate_vec_shift_64 c (B.vec_shift_right ~shape:`I16 ~signed:false)
  | PUNPCKHWD ->
      translate_punpack c
        (0x1F_1E_0F_0E_1D_1C_0D_0CL, 0x1B_1A_0B_0A_19_18_09_08L)
        ~mm:0x17_16_07_06_15_14_05_04L
  | PADDD -> translate_vec_bi_op c (B.vec_add ~shape:`I32)
  | PSRAD ->
      translate_vec_shift_64 c (B.vec_shift_right ~signed:true ~shape:`I64)
  | PUNPCKLDQ ->
      translate_punpack c
        (0x17_16_15_14_07_06_05_04L, 0x13_12_11_10_03_02_01_00L)
        ~vec2_zeroed:
          (B.vec_extend ~shape:`I32 ~signed:false ~half_used:`LowOrder)
  | PUNPCKHDQ ->
      translate_punpack c
        (0x1F_1E_1D_1C_0F_0E_0D_0CL, 0x1B_1A_19_18_0B_0A_09_08L)
        ~mm:0x17_16_15_14_07_06_05_04L
  | PACKSSDW -> translate_vec_pack c (B.vec_narrow_32bit ~signed:true)
  | PACKUSWB -> translate_vec_pack c (B.vec_narrow_16bit ~signed:false)
  | PMULLW -> translate_vec_bi_op c (B.vec_mul ~shape:`I16)
  | PADDW -> translate_vec_bi_op c (B.vec_add ~shape:`I16)
  | PSUBW -> translate_vec_bi_op c (B.vec_sub ~shape:`I16)
  | NOP -> ()
  (* is it worth it keeping track of this stuff? *)
  | EMMS -> ()
  | F2XM1 -> translate_f2xm1 c
  | MOVLPD -> translate_move_low c
  | PEXTRW -> translate_vec_extract c `I16
  | MULSD -> translate_vec_bottom_64 c (B.vec_mul ~shape:`F64)
  | PSUBQ -> translate_vec_bi_op c (B.vec_add ~shape:`I64)
  | CVTDQ2PD -> translate_cvtdq2pd c
  | ADDPD -> translate_vec_bi_op c (B.vec_add ~shape:`F64)
  | ADDSD -> translate_vec_bottom_64 c (B.vec_add ~shape:`F64)
  | UNPCKLPD ->
      translate_punpack c
        (0x17_16_15_14_13_12_11_10L, 0x07_06_05_04_03_02_01_00L)
        ~allow_mm:false
  | PINSRW -> translate_vec_insert c `I16
  | CVTSD2SI -> translate_vec_convert_low_f64_to_i32 c
  | PSHUFD -> translate_vec_shuffle_dwords c
  | MULPD -> translate_vec_bi_op c (B.vec_mul ~shape:`F64)
  | SUBSD -> translate_vec_bottom_64 c (B.vec_sub ~shape:`F64)
  | PMAXSW -> translate_vec_bi_op c (B.vec_max ~shape:I16 ~signed:true)
  | PCMPEQD -> translate_vec_bi_op c (B.vec_equal ~shape:`I32)
  | PMOVMSKB -> translate_vec_bitmask c
  | DIVSD -> translate_vec_bottom_64 c (B.vec_div ~shape:F64)
  | FPREM -> translate_float_remainder c `Mod
  | COMISD -> translate_ucomisd c
  | FNSTENV when c.opcode.address = 0x489403 -> ()
  | FLDENV when c.opcode.address = 0x489412 -> ()
  | SHUFPD -> translate_vec_shuffle_doubles c
  | FXAM -> translate_fxam c
  | ROL -> translate_rotate_left c
  | XLATB -> translate_xlatb c
  | BTS -> translate_bit_test c `Set
  | BT -> translate_bit_test c `Test
  | _ -> raise_c c "Invalid instruction"

let translate_rep_prefix c =
  match c.opcode.id with
  | MOVSD | MOVSW | MOVSB -> translate_rep_movs c
  | STOSD | STOSB -> translate_rep_stos c
  | CMPSD | CMPSB -> translate_repe_cmpsd c
  | _ -> raise_c c "Invalid instruction"

let translate_repne_prefix c =
  match c.opcode.id with
  | SCASB -> translate_repne_scas c
  | _ -> raise_c c "Invalid instruction"

let translate intrinsics builder state opcode =
  let c = { builder; opcode; state; intrinsics } in
  match opcode.prefix with
  | p when p = opcode_prefix_none -> translate_no_prefix c
  | p when p = opcode_prefix_rep -> translate_rep_prefix c
  | p when p = opcode_prefix_repne -> translate_repne_prefix c
  | _ -> raise_c c "Invalid opcode prefix"

let translate_terminator intrinsics builder state opcode ~tail_position =
  let result =
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
    }
      when not tail_position ->
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
        if tail_position then (
          (* tail call*)
          translate_call { intrinsics; builder; state; opcode } ~nontail:false;
          Return)
        else Unconditional { target = value }
    | {
     id = JMP;
     prefix = 0;
     opex =
       {
         operands =
           [
             Memory
               {
                 size = 4;
                 base = None;
                 index = None;
                 scale = 1;
                 segment = None;
                 displacement;
               };
           ];
       };
     _;
    }
      when tail_position && Hashtbl.mem intrinsics displacement ->
        let intrinisc = Hashtbl.find_exn intrinsics displacement in
        translate_direct_call
          { intrinsics; builder; state; opcode }
          intrinisc.name intrinisc.signature ~nontail:false;
        Return
    | { id = JMP; prefix = 0; opex = { operands = [ addr ] }; _ }
      when tail_position ->
        let context = { intrinsics; builder; state; opcode } in
        translate_indirect_call context
          ~addr:(load_operand context addr)
          Util.fast_call ~nontail:false;
        Return
    | { id = RET; prefix = 0; opex = { operands }; _ } when tail_position ->
        let esp = X86reg.to_ident `esp in
        B.load32 ~varName:(X86reg.to_ident `eip) builder
          (B.newest_var builder esp)
        |> ignore;
        let pop_size =
          match operands with
          | [] -> 4
          | [ Immediate { value; _ } ] -> value + 4
          | _ -> raise_ops { intrinsics; builder; state; opcode }
        in
        B.add builder ~rhs:(B.const builder pop_size)
          ~lhs:(B.newest_var builder esp) ~varName:esp
        |> ignore;
        Return
    | {
     id =
       ( JP | JNP | JE | JNE | JB | JBE | JGE | JG | JL | JLE | JA | JAE | JO
       | JNO | JS | JNS );
     prefix = 0;
     opex = { operands = [ Immediate { value; _ } ] };
     _;
    }
      when not tail_position ->
        let condition =
          translate_condition { intrinsics; builder; state; opcode }
        in

        (* would it be better to write_fpu_stack_changes first here? *)
        Conditional { target = value; condition }
    | {
     id = JECXZ;
     prefix = 0;
     opex = { operands = [ Immediate { value; _ } ] };
     _;
    }
      when not tail_position ->
        Conditional
          {
            target = value;
            condition =
              B.equals_zero builder
                (B.newest_var builder @@ X86reg.to_ident `ecx);
          }
    | { id = INT3; prefix = 0; opex = { operands = [] }; _ } when tail_position
      ->
        Return
    (* call to RaiseException *)
    | {
     id = CALL;
     prefix = 0;
     opex =
       {
         operands =
           [
             Memory
               {
                 size = 4;
                 scale = 1;
                 displacement = 4772052;
                 index = None;
                 base = None;
                 segment = None;
               };
           ];
       };
     _;
    } ->
        translate intrinsics builder state opcode;
        Return
    | _ ->
        translate intrinsics builder state opcode;
        Nothing
  in

  write_fpu_stack_changes ~state ~builder;
  B.store_globals builder;
  (* doesn't work *)
  (* (match result with *)
  (* | Return -> B.store_globals builder *)
  (* | Nothing | Conditional _ | Unconditional _ | Switch _ -> ()); *)
  result

let translate_output_condition builder state condition_opcode =
  translate_condition
    { builder; state; opcode = condition_opcode; intrinsics = empty_hashtbl }
  |> B.dup_var builder ~varName:Util.input_compare_arg Int
  |> ignore

let print () =
  let c = !vcount in
  vcount := 0;
  c
