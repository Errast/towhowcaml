open! Core
open Radatnet.Types
open Radatnet
open Mir
module B = Builder

type state = {
  mutable compare_args : (Instr.Ref.t * X86reg.gpr_type) list;
  mutable compare_instr : X86_instr.t;
  mutable input_condition_instr : X86_instr.t;
  mutable output_condition_instr : X86_instr.t;
  mutable changed_flags : Status_flags.t;
  mutable fpu_status_word_args : Instr.Ref.t list;
  mutable fpu_status_word_instr : X86_instr.t;
  mutable float_compare_args : Instr.Ref.t list;
  mutable float_compare_instr : X86_instr.t;
  mutable backward_direction : bool;
  mutable fpu_stack_pointer : Instr.Ref.t;
}
[@@deriving sexp_of]

(* why not *)
let invalid_instr = X86_instr.VAESKEYGENASSIST

let initial_state () =
  {
    compare_args = [];
    compare_instr = invalid_instr;
    input_condition_instr = invalid_instr;
    output_condition_instr = invalid_instr;
    changed_flags = Status_flags.none;
    fpu_status_word_args = [];
    fpu_status_word_instr = invalid_instr;
    float_compare_args = [];
    float_compare_instr = invalid_instr;
    backward_direction = false;
    fpu_stack_pointer = Instr.Ref.invalid;
  }

type context = {
  opcode : Radatnet.Types.opcode;
  state : state;
  builder : Mir.Builder.t;
}
[@@deriving sexp_of]

let flags_used =
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
    (Status_flags.equal c.state.changed_flags
    @@ Hashtbl.find_exn flags_used c.opcode.id)
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
  | [ dest; Memory mem ] ->
      let addr, offset = load_partial_address c mem in
      let res =
        if offset = 0 then
          B.add c.builder ~lhs:addr ~rhs:(B.const c.builder offset)
        else addr
      in
      store_operand c res ~dest
  | _ -> raise_ops c

let translate_no_prefix c =
  assert_c c (c.opcode.prefix = opcode_prefix_none);
  let open X86_instr in
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
  | _ -> raise_c c "Invalid instruction"

let translate_rep_prefix c = raise_c c "rep prefix"
let translate_repne_prefix c = raise_c c "repne prefix"

let translate builder state opcode =
  let c = { builder; opcode; state } in
  match opcode.prefix with
  | p when p = opcode_prefix_none -> translate_no_prefix c
  | p when p = opcode_prefix_rep -> translate_rep_prefix c
  | p when p = opcode_prefix_repne -> translate_repne_prefix c
  | _ -> raise_c c "Invalid opcode prefix"
