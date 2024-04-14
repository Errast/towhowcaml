open! Core

type terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of { offset : int; cases : Radatnet.Types.jump_table_case list }
  | Return
  | Trap
[@@deriving sexp]

type block = {
  offset : int;
  terminator : terminator;
  ops : Radatnet.Types.opcode array;
}
[@@deriving sexp]

val translate :
  intrinsics:(int, Util.intrinsic) Hashtbl.t ->
  name:string ->
  blocks:block array ->
  Mir.Func.t
