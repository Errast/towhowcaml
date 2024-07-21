open! Core
open Syntax
open Mir
module B = Builder
module StrMap = Map.Make (String)

type unbound_branch_target = Block of int | Label of string [@@deriving sexp]

type unbound_terminator =
  | Goto of unbound_branch_target
  | If of { cond : expr; t : unbound_branch_target; f : unbound_branch_target }
  | IfReturn of { cond : expr; f : unbound_branch_target }
  | Return
[@@deriving sexp]

type unbound_block = {
  id : int;
  body : statement list;
  terminator : unbound_terminator;
}
[@@deriving sexp]

let separate_blocks statements =
  let id_ref = ref 0 in
  let new_id () =
    let id = !id_ref in
    id_ref := id + 1;
    id
  in
  let block_map = Hashtbl.create (module String) in
  let blocks = ref [] in
  let rec go : int -> statement list -> int =
   fun next_block stmts ->
    match stmts with
    | last :: stmts ->
        let terminator, stmts =
          match last with
          | If { cond; t = [ Return ]; f = [ Goto l ] } ->
              (IfReturn { cond; f = Label l }, stmts)
          | If { cond; t = [ Return ]; f = [] } ->
              (IfReturn { cond; f = Block next_block }, stmts)
          | If { cond; t; f } ->
              let f =
                match f with
                | [ Goto l ] -> Label l
                | _ -> Block (go next_block @@ f)
              in
              let t =
                match t with
                | [ Goto l ] -> Label l
                | _ -> Block (go next_block @@ t)
              in
              (If { cond; t; f }, stmts)
          | GotoId i -> (Goto (Block i), stmts)
          | Goto l -> (Goto (Label l), stmts)
          | Return -> (Return, stmts)
          | i -> (Goto (Block next_block), i :: stmts)
        in
        let rec take_block :
            statement list ->
            statement list ->
            statement list * string option * statement list =
         fun stmts acc ->
          match stmts with
          | (If _ | Goto _ | GotoId _ | Return) :: _ -> (acc, None, stmts)
          | Label label :: stmts -> (acc, Some label, stmts)
          | [] -> (acc, None, [])
          | stmt :: stmts -> take_block stmts (stmt :: acc)
        in
        let body, label, stmts = take_block stmts [] in
        let id = new_id () in
        Option.iter label ~f:(fun key ->
            Hashtbl.add_exn block_map ~key ~data:id);
        blocks := { id; body; terminator } :: !blocks;
        go id stmts
    | _ -> next_block
  in
  let count = go (-1) @@ statements in
  ( List.sort ~compare:(fun x y -> compare_int y.id x.id) !blocks,
    count,
    block_map )

let lower_block max_block block_map local_vars block : Block.t =
  let b = B.create local_vars in
  let bind_block : unbound_branch_target -> branch_target = function
    | Block i -> Block (max_block - i)
    | Label l -> Block (max_block - Hashtbl.find_exn block_map l)
  in
  let rec lower_expr aliases = function
    | Const i ->
        let i = int_of_string i in
        if i <= 0xFFFFFFFF then B.const b i else failwith "big num"
    | LongConst i -> B.long_const b @@ Int64.of_string i
    | Var name -> B.newest_var b name
    | Use name -> Map.find_exn aliases name
    | UniOp (op, expr) -> (
        let operand = lower_expr aliases expr in
        match op with
        | Not -> B.equals_zero b operand
        | TruncLongToInt -> B.long_to_int32 b operand
        | SextIntToLong -> B.int32_to_long b operand ~signed:true
        | ZextIntToLong -> B.int32_to_long b operand ~signed:false)
    | BiOp (op, lhs, rhs) -> (
        let lhs = lower_expr aliases lhs in
        let rhs = lower_expr aliases rhs in
        match op with
        | Add -> B.add b ~lhs ~rhs
        | Sub -> B.sub b ~lhs ~rhs
        | Mul -> B.mul b ~lhs ~rhs
        | Eq -> B.equal b ~lhs ~rhs
        | NotEq -> B.not_equal b ~lhs ~rhs
        | BitAnd -> B.int_and b ~lhs ~rhs
        | BitOr -> B.int_or b ~lhs ~rhs
        | ShiftLeft -> B.shift_left b ~lhs ~rhs
        | LongAdd -> B.long_add b ~lhs ~rhs
        | LongSub -> B.long_sub b ~lhs ~rhs
        | LongMul -> B.long_mul b ~lhs ~rhs
        | LongEq -> B.long_equal b ~lhs ~rhs
        | LongNotEq -> B.long_not_equal b ~lhs ~rhs
        | LongAnd -> B.long_and b ~lhs ~rhs
        | LongOr -> B.long_or b ~lhs ~rhs
        | LongShiftLeft -> B.long_shift_left b ~lhs ~rhs)
    | Deref { addr; offset; size = Int } ->
        B.load32 b ~offset @@ lower_expr aliases addr
    | Deref { addr; offset; size = Float } ->
        B.float_load64 b ~offset @@ lower_expr aliases addr
    | Deref { addr; offset; size = Long } ->
        B.long_load64 b ~offset @@ lower_expr aliases addr
    | Deref { addr; offset; size = Vec } ->
        B.vec_load128 b ~offset @@ lower_expr aliases addr
    | Splat (shape, expr) -> B.vec_splat b ~shape @@ lower_expr aliases expr
    | Deref8 (addr, signed, offset) ->
        B.load8 b ~signed ~offset @@ lower_expr aliases addr
    | Deref16 (addr, signed, offset) ->
        B.load16 b ~signed ~offset @@ lower_expr aliases addr
    | SignBiOp { lhs; rhs; signed; op } -> (
        let lhs = lower_expr aliases lhs in
        let rhs = lower_expr aliases rhs in
        match op with
        | IntGT -> B.greater_than b ~lhs ~rhs ~signed
        | IntGTE -> B.greater_than_equal b ~lhs ~rhs ~signed
        | IntLT -> B.less_than b ~lhs ~rhs ~signed
        | IntLTE -> B.less_than_equal b ~lhs ~rhs ~signed
        | LongGT -> B.long_greater_than b ~lhs ~rhs ~signed
        | LongGTE -> B.long_greater_than_equal b ~lhs ~rhs ~signed
        | LongLT -> B.long_less_than b ~lhs ~rhs ~signed
        | LongLTE -> B.long_less_than_equal b ~lhs ~rhs ~signed
        | ShiftRight -> B.shift_right b ~lhs ~rhs ~signed
        | LongShiftRight -> B.long_shift_right b ~lhs ~rhs ~signed)
  in
  let aliases =
    List.fold block.body ~init:StrMap.empty ~f:(fun aliases stmt ->
        match stmt with
        | Let { lhs; rhs } ->
            B.try_change_var b lhs @@ lower_expr aliases rhs |> ignore;
            aliases
        | Alias { lhs; rhs } ->
            Map.add_exn aliases ~key:lhs ~data:(lower_expr aliases rhs)
        | Store { addr; offset; value; size = Int } ->
            let addr = lower_expr aliases addr in
            B.store32 b ~offset ~value:(lower_expr aliases value) ~addr;
            aliases
        | Store { addr; offset; value; size = Float } ->
            let addr = lower_expr aliases addr in
            B.float_store64 b ~offset ~value:(lower_expr aliases value) ~addr;
            aliases
        | Store { addr; offset; value; size = Long } ->
            let addr = lower_expr aliases addr in
            B.long_store64 b ~offset ~value:(lower_expr aliases value) ~addr;
            aliases
        | Store { addr; offset; value; size = Vec } ->
            let addr = lower_expr aliases addr in
            B.vec_store128 b ~offset ~value:(lower_expr aliases value) ~addr;
            aliases
        | Store8 { addr; offset; value } ->
            let addr = lower_expr aliases addr in
            B.store8 b ~offset ~value:(lower_expr aliases value) ~addr;
            aliases
        | If _ | Label _ | Goto _ | GotoId _ | Return ->
            print_s @@ sexp_of_statement stmt;
            failwith "bad")
  in
  let terminator : Block.terminator =
    match block.terminator with
    | Return | Goto (Block -1) -> Return
    | Goto b -> Goto (bind_block b)
    | If { cond; t; f } ->
        Branch
          {
            condition = lower_expr aliases cond;
            succeed = bind_block t;
            fail = bind_block f;
          }
    | IfReturn { cond; f } ->
        BranchReturn
          { condition = lower_expr aliases cond; fail = bind_block f }
  in
  if Poly.(terminator = Return) then B.store_globals b;
  let instrs, _, _, roots = B.deconstruct b in
  { id = max_block - block.id; instrs; roots; terminator }

let lower : func_def -> Mir.Func.t =
 fun func_def ->
  let blocks, max_block, block_map = separate_blocks func_def.body in
  let local_vars =
    Map.of_alist_exn (module String)
    @@ List.map
         ~f:(fun v ->
           (v.name, { B.name = v.name; scope = `Local; typ = v.typ }))
         (func_def.signature.returns @ func_def.signature.args @ func_def.locals)
  in
  let blocks =
    Array.Permissioned.of_list_map
      ~f:(lower_block max_block block_map local_vars)
      blocks
  in
  {
    name = func_def.name;
    blocks;
    signature = func_def.signature;
    locals =
      Map.of_list_with_key_exn
        (module String)
        ~get_key:(fun v -> v.name)
        func_def.locals;
  }
