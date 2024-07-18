open! Core
open Syntax
open Mir
module B = Builder
module StrMap = Map.Make (String)

type unbound_branch_target = Block of int | Label of string [@@deriving sexp]

type unbound_terminator =
  | Goto of unbound_branch_target
  | If of { cond : expr; t : unbound_branch_target; f : unbound_branch_target }
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
          | If { cond; t; f } ->
              let f =
                match f with
                | [ Goto l ] -> Label l
                | _ -> Block (go next_block @@ List.rev f)
              in
              let t =
                match t with
                | [ Goto l ] -> Label l
                | _ -> Block (go next_block @@ List.rev t)
              in
              (If { cond; t; f }, stmts)
          | i -> (Goto (Block next_block), i :: stmts)
        in
        let rec take_block :
            statement list ->
            statement list ->
            statement list * string option * statement list =
         fun stmts acc ->
          match stmts with
          | If _ :: _ -> (acc, None, stmts)
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
  let count = go (-1) @@ List.rev statements in
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
    | Const i -> B.const b i
    | Var name -> B.newest_var b name
    | Use name -> Map.find_exn aliases name
    | UniOp (op, expr) -> (
        let operand = lower_expr aliases expr in
        match op with Not -> B.equals_zero b operand)
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
        | BitOr -> B.int_or b ~lhs ~rhs)
    | Deref (addr, offset) -> B.load32 b ~offset @@ lower_expr aliases addr
  in
  let aliases =
    List.fold block.body ~init:StrMap.empty ~f:(fun aliases stmt ->
        match[@warning "-8"] stmt with
        | Let { lhs; rhs } ->
            B.try_change_var b lhs @@ lower_expr aliases rhs |> ignore;
            aliases
        | Alias { lhs; rhs } ->
            Map.add_exn aliases ~key:lhs ~data:(lower_expr aliases rhs)
        | Store { addr; offset; value } ->
            B.store32 b ~offset ~value:(lower_expr aliases value)
              ~addr:(lower_expr aliases addr);
            aliases)
  in
  let terminator : Block.terminator =
    match block.terminator with
    | Goto (Block -1) -> Return
    | Goto b -> Goto (bind_block b)
    | If { cond; t; f } ->
        Branch
          {
            condition = lower_expr aliases cond;
            succeed = bind_block t;
            fail = bind_block f;
          }
  in
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
