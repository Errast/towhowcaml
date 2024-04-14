open Core
open Types

type terminator =
  | Goto of branch_target
  | Branch of {
      succeed : branch_target;
      fail : branch_target;
      condition : Instr.Ref.t;
    }
  | Switch of {
      cases : branch_target list;
      default : branch_target;
      switch_on : Instr.Ref.t;
    }
  | Return
[@@deriving sexp]

type t = {
  id : int;
  instrs : Instr_list.t;
  terminator : terminator;
  roots : Set.M(Instr.Ref).t;
}
[@@deriving sexp]
