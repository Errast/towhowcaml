open! Core
open Types
open Smt_util
module Reg = Radatnet.X86reg

let val_bool b = if b then True else False

(*
Gonna make the assumption that we can load all SRC beforehand
and write DEST afterwards and everything will be fine.
*)

type ctx = {
  env : value String.Map.t;
  reg_map : Reg_map.t;
  (* If this was a function from a value instead of an Smt thing,
  it could be partially evaluated *)
  memory : Array.t;
  fp_offset : int;
}

let lift_value = function
  | SymbolicBV expr | SymbolicBool expr | SymbolicFloat expr -> expr
  | _ -> failwith "cannot lift value"

let eval_assign ctx lhs rhs =
  { ctx with env = Map.set ctx.env ~key:lhs ~data:rhs }

let fp_index_to_phys_reg = function
  | -7 -> `st__7
  | -6 -> `st__6
  | -5 -> `st__5
  | -4 -> `st__4
  | -3 -> `st__3
  | -2 -> `st__2
  | -1 -> `st__1
  | 0 -> `st0
  | 1 -> `st1
  | 2 -> `st2
  | 3 -> `st3
  | 4 -> `st4
  | 5 -> `st5
  | 6 -> `st6
  | _ -> failwith "invalid fp_index"

let x87_reg_to_phys_reg ctx reg =
  ctx.fp_offset - Reg.x87_float_to_index reg |> fp_index_to_phys_reg

let load_reg ctx reg =
  match reg with
  | #Reg.reg_32bit as reg -> SymbolicBV (Reg_map.get ctx.reg_map reg)
  | #Reg.x87_float as reg ->
      SymbolicFloat (x87_reg_to_phys_reg ctx reg |> Reg_map.get ctx.reg_map)
  | _ -> failwith "cannot load reg"

let store_reg ctx reg data =
  let reg_map =
    match reg with
    | #Reg.reg_32bit as reg -> Reg_map.set ctx.reg_map ~key:reg ~data
    | #Reg.x87_float as reg ->
        Reg_map.set ctx.reg_map ~key:(x87_reg_to_phys_reg ctx reg) ~data
    | _ -> failwith "cannot store reg"
  in
  if phys_equal reg_map ctx.reg_map then ctx else { ctx with reg_map }

let lift_int symb int =
  let sort = get_sort symb in
  assert (((1 lsl Bitvec.sort_size sort) - 1) land int = int);
  of_int sort int

let eval_eq lhs rhs =
  match (lhs, rhs) with
  | lhs, rhs when equal_value lhs rhs -> True
  | SymbolicBV symb, value
  | SymbolicBool symb, value
  | SymbolicFloat symb, value
  | value, SymbolicBV symb
  | value, SymbolicBool symb
  | value, SymbolicFloat symb ->
      SymbolicBool Smt.(symb = lift_value value)
  | _ -> failwith "cannot eval eq"

let eval_mul lhs rhs =
  match (lhs, rhs) with
  | SymbolicBV bv, Int i | Int i, SymbolicBV bv ->
      (* should the size be stored in the SymbolicBV? *)
      let sort = Z3.Expr.get_sort bv in
      let size = Bitvec.sort_size sort in
      let extended = Bitvec.sign_ext size bv in
      SymbolicBV Bitvec.(extended * sign_ext size (Bitvec.of_int sort i))
  | SymbolicBV lhs, SymbolicBV rhs ->
      let size = Z3.Expr.get_sort lhs |> Bitvec.sort_size in
      SymbolicBV Bitvec.(zero_ext size lhs * zero_ext size rhs)
  | SymbolicFloat lhs, SymbolicFloat rhs -> SymbolicFloat FP.(lhs * rhs)
  | _ -> failwith "cannot eval mul"

let eval_funcall_expr =
 fun ctx name args ->
  match (name, args) with
  | "ConvertToDoubleExtendedPrecisionFP", [ SymbolicBV bv ] ->
      SymbolicFloat (FP.of_bv bv)
  | _ -> failwith "no such funcs"

let eval_funcall_stmt ctx name args =
  match (name, args) with
  | "PopRegisterStack", [] ->
      (* This is just blatantly not true? *)
      assert (ctx.fp_offset > 0);
      { ctx with fp_offset = ctx.fp_offset - 1 }
  | ""
  | _ -> failwith "no funcs"

let rec eval_assign_reg ({ reg_map; _ } as ctx) lhs rhs =
  let[@warning "-8"] get_bv_exn (SymbolicBV bv) = bv in
  let subword_reg ctx reg high_bit low_bit =
    let big_reg = Reg.to_32_bit reg in
    let prev = Reg_map.get reg_map big_reg in
    Reg_map.set reg_map ~key:big_reg
      ~data:
        (eval_expr ctx rhs |> get_bv_exn |> Bitvec.replace high_bit low_bit prev)
  in
  let reg_map =
    match lhs with
    | #Reg.reg_32bit as reg ->
        Reg_map.set reg_map ~key:reg ~data:(eval_expr ctx rhs |> get_bv_exn)
    | #Reg.reg_16bit as reg -> subword_reg ctx reg 15 0
    | #Reg.reg_high8bit as reg -> subword_reg ctx reg 15 8
    | #Reg.reg_low8bit as reg -> subword_reg ctx reg 7 0
    | #Reg.flags as reg ->
        Reg_map.set_lazy reg_map ~key:reg
          ~data:(lazy (eval_expr ctx rhs |> get_bv_exn))
    | #Reg.x87_float as reg ->
        let[@warning "-8"] (SymbolicFloat rhs) = eval_expr ctx rhs in
        let phys_reg = x87_reg_to_phys_reg ctx reg in
        Reg_map.set reg_map ~key:phys_reg ~data:rhs
    | _ -> failwith "unimplemented reg"
  in
  { ctx with reg_map }

and eval_expr ctx = function
  | Reg reg -> load_reg ctx reg
  | Ident id -> begin
      try Map.find_exn ctx.env id with Not_found_s _ -> Atom id
    end
  | Eq (lhs, rhs) -> eval_eq (eval_expr ctx lhs) (eval_expr ctx rhs)
  | Mul (lhs, rhs) -> eval_mul (eval_expr ctx lhs) (eval_expr ctx rhs)
  | FunCall (name, args) ->
      eval_funcall_expr ctx name @@ List.map ~f:(eval_expr ctx) args

let merge_env cond true_env false_env =
  Map.fold_symmetric_diff ~data_equal:equal_value true_env false_env
    ~init:true_env ~f:(fun acc (key, v) ->
      match v with
      (* You can't remove bindings, so if anything uses these bindings
            it will fail instead of giving a wrong value *)
      | `Left _ -> Map.remove acc key
      | `Right _ -> acc
      | `Unequal (true_val, false_val) ->
          let if_expr =
            if_ cond ~then_:(lift_value true_val) ~else_:(lift_value false_val)
          in
          let data =
            match true_val with
            | SymbolicBV _ -> SymbolicBV if_expr
            | SymbolicBool _ -> SymbolicBool if_expr
            | SymbolicFloat _ -> SymbolicFloat if_expr
            | _ -> failwith "bad_stuff"
          in
          Map.set acc ~key ~data)

(* Abstraction <<< copy-paste *)
let merge_phys_regs cond true_regs false_regs =
  Reg_map.fold_symmetric_diff true_regs false_regs ~init:true_regs
    ~f:(fun acc reg ~left ~right ->
      let left_forced = Lazy.force left in
      let right_forced = Lazy.force right in
      let if_expr = if_ cond ~then_:left_forced ~else_:right_forced in
      Reg_map.set acc ~key:reg ~data:if_expr)

let rec eval_if ctx cond then_ else_ =
  match cond with
  | True -> List.fold_left ~init:ctx ~f:eval_statement then_
  | False -> List.fold_left ~init:ctx ~f:eval_statement else_
  | SymbolicBool cond ->
      let true_ctx = List.fold_left ~init:ctx ~f:eval_statement then_ in
      let false_ctx = List.fold_left ~init:ctx ~f:eval_statement else_ in
      (* So we're saying any branches must be partially-evaluated away
      where not all do the same thing to fp_offset *)
      assert (Int.(true_ctx.fp_offset = false_ctx.fp_offset));
      let merged_env = merge_env cond true_ctx.env false_ctx.env in
      let merged_regs =
        merge_phys_regs cond true_ctx.reg_map false_ctx.reg_map
      in
      let merged_mem =
        if phys_equal true_ctx.memory false_ctx.memory then true_ctx.memory
        else fun ptr ->
          if_ cond ~then_:(true_ctx.memory ptr) ~else_:(false_ctx.memory ptr)
      in
      {
        env = merged_env;
        memory = merged_mem;
        reg_map = merged_regs;
        fp_offset = true_ctx.fp_offset;
      }
  | _ -> failwith "cannot eval if"

and eval_statement ctx = function
  | Assign (lhs, rhs) -> eval_assign ctx lhs (eval_expr ctx rhs)
  | AssignReg (lhs, rhs) -> eval_assign_reg ctx lhs rhs
  | FunCall (name, args) ->
      eval_funcall_stmt ctx name @@ List.map ~f:(eval_expr ctx) args
  | If { cond; then_; else_ } -> eval_if ctx (eval_expr ctx cond) then_ else_

let eval_statements ctx stmts = List.fold ~f:eval_statement ~init:ctx stmts
