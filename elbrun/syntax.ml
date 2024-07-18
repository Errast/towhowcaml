open! Core

type bi_op = Add | Sub | Mul | BitAnd | BitOr | Eq | NotEq [@@deriving sexp]
type uni_op = Not [@@deriving sexp]

type expr =
  (* 32bit int *)
  | Const of int
  | Var of string
  | Use of string
  | UniOp of uni_op * expr
  | BiOp of bi_op * expr * expr
  | Deref of expr * int
[@@deriving sexp]

type statement =
  | Let of { lhs : string; rhs : expr }
  | Alias of { lhs : string; rhs : expr }
  | Store of { addr : expr; offset : int; value : expr }
  | If of { cond : expr; t : statement list; f : statement list }
  | Label of string
  | Goto of string
  | Return
[@@deriving sexp]

type func_def = {
  name : string;
  signature : Mir.func_sig;
  locals : Mir.variable list;
  body : statement list;
}
[@@deriving sexp]
