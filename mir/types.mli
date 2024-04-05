open! Core

type ident = string [@@deriving sexp]
type opt_ident = string option [@@deriving sexp]
type local_type = Int | Float | Long | Vec [@@deriving sexp]

type vec_lane_shape = [ `I8 | `I16 | `I32 | `I64 | `F32 | `F64 ]
[@@deriving sexp]

type branch_target = Block of int | Return [@@deriving sexp]
type local = { name : ident; typ : local_type } [@@deriving sexp]
type func_sig = { args : local list; return : local_type } [@@deriving sexp]
