open! Core
open Radatnet.Types
module T_Util = Util
open Mir
module AP = Array.Permissioned
module Vec = Mir.Vec

type intrinsic = { addr : int; name : string; mir_name : string }
[@@deriving sexp]

type jump_case = { num : int; target : int } [@@deriving sexp]

type terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of jump_case list
[@@deriving sexp]

type block = {
  offset : int;
  terminator : terminator;
  ops : (Radatnet.Types.opcode, immutable) Array.Permissioned.t;
}
[@@deriving sexp]

type block_data = { builder : Mir.Builder.t; state : Instr_translator.state }

type context = {
  intrinsics : (int, intrinsic) Hashtbl.t;
  inverted_cfg : (int list, read_write) AP.t;
  raw_blocks : (block, immutable) AP.t;
}

let offset_to_block_id c offset =
  AP.binary_search c.raw_blocks
    ~compare:(fun b o -> compare_int b.offset o)
    `First_equal_to offset
  |> Option.value_exn

let translate_block c id block =
  let add_branch b =
    let b_id = offset_to_block_id c b in
    AP.set c.inverted_cfg b_id (id :: AP.get c.inverted_cfg b_id)
  in
  (match block.terminator with
  | Goto b -> add_branch b
  | Branch { succeed; fail } ->
      add_branch succeed;
      add_branch fail
  | Switch bs ->
      List.fold ~f:(fun () { target; _ } -> add_branch target) ~init:() bs);
  let builder = Builder.create T_Util.used_locals in
  if id = 0 then
    Builder.get_global builder T_Util.stack_pointer_global Int
      ~varName:(Radatnet.X86reg.to_ident `esp)
    |> ignore

let translate ~intrinsics ~name ~blocks =
  let num_blocks = AP.length blocks in
  let inverted_cfg = AP.create ~len:num_blocks [] in
  assert (
    AP.is_sorted_strictly
      ~compare:(fun l r -> compare_int l.offset r.offset)
      blocks);
  let block_data =
    AP.mapi ~f:(translate_block intrinsics inverted_cfg) c.raw_blocks
  in
  failwith "todo"

let translate_block ~intrinsics ~block = failwith "todo"
