open Core
open Types

type terminator =
  | Goto of branch_target
  | Branch of { succeed : branch_target; fail : branch_target }
  | Switch of (int * branch_target) list
[@@deriving sexp]

type t = {
  id : int;
  terminator : terminator;
  instrs : (Instr.t, Perms.Export.immutable) Array.Permissioned.t;
  roots : Local_info.t Map.M(String).t;
}
[@@deriving sexp]
