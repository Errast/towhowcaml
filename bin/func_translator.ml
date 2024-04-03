open! Core
open Radatnet.Types

type intrinsic = { addr : int; name : string; mir_name : string }
[@@deriving sexp]

type block_terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of Radatnet.Types.jump_table_case list
[@@deriving sexp]

type block = {
  offset : int;
  terminator : block_terminator;
  ops : (Radatnet.Types.opcode, immutable) Array.Permissioned.t;
}
[@@deriving sexp]

let translate c addr =
  (c, addr) |> ignore;
  failwith "not implemented"
