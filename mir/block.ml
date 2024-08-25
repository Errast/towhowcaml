open Core
open Types

type terminator =
  | Goto of branch_target
  | Branch of {
      succeed : branch_target;
      fail : branch_target;
      condition : Instr.Ref.t;
    }
  | BranchReturn of { fail : branch_target; condition : Instr.Ref.t }
  | Switch of {
      cases : branch_target list;
      default : branch_target;
      switch_on : Instr.Ref.t;
    }
  | Return
[@@deriving sexp]

module Terminator = struct
  include Container.Make0 (struct
    type t = terminator

    module Elt = struct
      type t = branch_target

      let equal (Block l) (Block r) = l = r
    end

    let fold t ~init ~f =
      match t with
      | Goto t | BranchReturn { fail = t; _ } -> f init t
      | Branch { succeed; fail; _ } -> f (f init succeed) fail
      | Switch { cases; default; _ } -> f (List.fold ~f ~init cases) default
      | Return -> init

    let iter = `Define_using_fold

    let length =
      `Custom
        (function
        | Return -> 0
        | Goto _ | BranchReturn _ -> 1
        | Branch _ -> 2
        | Switch { cases; _ } -> List.length cases + 1)
  end)

  let map f = function
    | Goto t -> Goto (f t)
    | Branch t -> Branch { t with succeed = f t.succeed; fail = f t.fail }
    | BranchReturn t -> BranchReturn { t with fail = f t.fail }
    | Switch t ->
        Switch { t with cases = List.map ~f t.cases; default = f t.default }
    | Return -> Return
end

type t = {
  id : int;
  instrs : Instr_list.t;
  terminator : terminator;
  roots : Set.M(Instr.Ref).t;
}
[@@deriving sexp]
