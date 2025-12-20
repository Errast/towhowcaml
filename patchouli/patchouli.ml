open! Core
include Types
open Smt_util
module Types = Types
open Radatnet
module Parser = Parser
module Interpret = Interpret

type ctx = Interpret.ctx = {
  env : value String.Map.t;
  reg_map : Reg_map.t;
  (* If this was a function from a value instead of an Smt thing,
  it could be partially evaluated *)
  memory : Array.t;
  fp_offset : int;
  min_fp_offset : int;
}

let empty_context () : ctx =
  {
    env = String.Map.empty;
    reg_map = Reg_map.empty ();
    memory = Array.uninterpreted "memory" (Bitvec.dword ()) (Bitvec.byte ());
    fp_offset = 0;
    min_fp_offset = 0;
  }

let load_address ctx size addr = load_address ctx.memory size addr

let store_address ctx addr data =
  { ctx with memory = store_address ctx.memory addr data }

let[@warning "-8"] sort_of_size =
  let open Bitvec in
  function 1 -> byte | 2 -> word | 4 -> dword

let calc_address ctx =
  let add_displacement displacement addr =
    if displacement = 0 then addr
    else Bitvec.(addr + of_int (dword ()) displacement)
  in
  let get_reg ctx reg = Interpret.load_reg ctx reg |> Interpret.lift_value in
  function
  | {
      index = Some reg;
      scale = 1;
      displacement;
      base = None;
      segment = None;
      _;
    }
  | { index = None; displacement; base = Some reg; segment = None; _ } ->
      get_reg ctx reg |> add_displacement displacement
  | { index = Some index_reg; scale; displacement; base; segment = None; _ } ->
      let addr =
        get_reg ctx index_reg |> Bitvec.(( * ) @@ of_int (dword ()) scale)
      in
      let addr =
        match base with
        | Some base_reg -> Bitvec.(addr + get_reg ctx base_reg)
        | None -> addr
      in
      add_displacement displacement addr
  | { index = None; base = None; segment = None; displacement; _ } ->
      Bitvec.(of_int (dword ()) displacement)
  | _ -> failwith "invalid memory operand"

let load_operand ctx = function
  | Register { reg; _ } -> Interpret.load_reg ctx reg
  | Immediate { value; _ } -> Int value
  | Memory mem_op ->
      let addr = calc_address ctx mem_op in
      SymbolicBV (load_address ctx mem_op.size addr)

let store_operand ctx op data =
  match op with
  | Immediate _ -> failwith "can't store to immediate"
  | Register { reg; _ } -> Interpret.store_reg ctx reg data
  | Memory mem_op ->
      assert (mem_op.size * 8 = (get_sort data |> Bitvec.sort_size));
      let addr = calc_address ctx mem_op in
      store_address ctx addr data

let eval_instruction : ctx -> Radatnet.Types.opcode -> ctx =
  let flush_dest (ctx : ctx) dest_op =
    Map.find_exn ctx.env "DEST"
    |> Interpret.lift_value |> store_operand ctx dest_op
  in
  fun ctx instr ->
    let pseudocode = Pseudocode.get_pseudocode instr.id in
    let[@warning "-8"] (Sexp.Atom name) =
      Radatnet.X86_instr.sexp_of_t instr.id
    in
    let env =
      String.Map.empty |> Map.set ~key:"Instruction" ~data:(Atom name)
    in
    let reg_map =
      Reg_map.set ctx.reg_map ~key:`eip
        ~data:
          Bitvec.(Reg_map.get ctx.reg_map `eip + of_int (dword ()) instr.size)
    in
    match (instr.id, instr.opex.operands) with
    | _, [] -> Interpret.eval_statements { ctx with env; reg_map } pseudocode
    | (ADD | FADD | FADDP | FIADD), [ dest_op; src_op ] ->
        let env =
          Map.set env ~key:"SRC" ~data:(load_operand ctx src_op)
          |> Map.set ~key:"DEST" ~data:(load_operand ctx dest_op)
          |> Map.set ~key:"Operands" ~data:(Int 2)
        in
        let ctx =
          Interpret.eval_statements { ctx with env; reg_map } pseudocode
        in
        flush_dest ctx dest_op
    | instr, operands ->
        raise_s
          [%message
            "unsupported instruction"
              ~instr:(instr : X86_instr.t)
              ~operands:(operands : Types.operand list)]
