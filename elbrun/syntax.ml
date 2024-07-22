open! Core

type sign_bi_op =
  | IntGT
  | IntLT
  | IntGTE
  | IntLTE
  | LongGT
  | LongLT
  | LongGTE
  | LongLTE
  | ShiftRight
  | LongShiftRight
[@@deriving sexp]

type bi_op =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor
  | Eq
  | NotEq
  | ShiftLeft
  | LongAdd
  | LongSub
  | LongMul
  | LongAnd
  | LongOr
  | LongXor
  | LongEq
  | LongNotEq
  | LongShiftLeft
[@@deriving sexp]

type uni_op =
  | Not
  | TruncLongToInt
  | SextIntToLong
  | ZextIntToLong
  | BitcastLongToFloat
  | BitcastFloatToLong
  | CountLeadingZeros
  | LongCountLeadingZeros
[@@deriving sexp]

type expr =
  (* 32bit int *)
  | Const of string
  | LongConst of string
  | Var of string
  | Use of string
  | UniOp of uni_op * expr
  | BiOp of bi_op * expr * expr
  | Deref of { addr : expr; offset : int; size : Mir.local_type }
  | Deref8 of expr * bool * int
  | Deref16 of expr * bool * int
  | Splat of Mir.vec_lane_shape * expr
  | SignBiOp of { op : sign_bi_op; lhs : expr; rhs : expr; signed : bool }
[@@deriving sexp]

type statement =
  | Let of { lhs : string; rhs : expr }
  | Alias of { lhs : string; rhs : expr }
  | Store of { addr : expr; offset : int; value : expr; size : Mir.local_type }
  | Store8 of { addr : expr; offset : int; value : expr }
  | Store16 of { addr : expr; offset : int; value : expr }
  | If of { cond : expr; t : statement list; f : statement list }
  | Label of string
  | Goto of string
  | GotoId of int
  | Return
[@@deriving sexp]

type func_def = {
  name : string;
  signature : Mir.func_sig;
  locals : Mir.variable list;
  body : statement list;
}
[@@deriving sexp]
