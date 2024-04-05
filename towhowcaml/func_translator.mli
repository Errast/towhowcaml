open! Core

type intrinsic = { addr : int; name : string; mir_name : string }
[@@deriving sexp]

type block = {
  offset : int;
  terminator : Mir.Block.terminator;
  ops : (Radatnet.Types.opcode, immutable) Array.Permissioned.t;
}
[@@deriving sexp]

val translate :
  intrinsics:(int, intrinsic) Hashtbl.t ->
  name:string ->
  blocks:block list ->
  Mir.Func.t
