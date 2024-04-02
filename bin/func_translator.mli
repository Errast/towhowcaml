open! Core

type intrinsic = { addr : int; name : string; mir_name : string }

type block_terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of Radatnet.Types.jump_table_case list

type block = {
  offset : int;
  terminator : block_terminator;
  ops : (Radatnet.Types.opcode, [ `Immutable ]) Array.Permissioned.t;
}

val translate :
  intrinsics:(int, intrinsic) Hashtbl.t ->
  name:string ->
  blocks:block list ->
  'a
