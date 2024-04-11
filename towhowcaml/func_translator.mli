open! Core

type intrinsic = { addr : int; name : string; mir_name : string }
[@@deriving sexp]

type jump_case = { num : int; target : int } [@@deriving sexp]

type terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of { offset : int; cases : jump_case list }
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
  intrinsics:(int, intrinsic) Hashtbl.t ->
  name:string ->
  blocks:block array ->
  Mir.Func.t
