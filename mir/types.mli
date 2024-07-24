open! Core

type ident = string [@@deriving sexp, compare]
type opt_ident = string option [@@deriving sexp]
type local_type = Int | Float | Long | Vec [@@deriving sexp, compare]

type int_vec_lane_shape = [`I8 | `I16 | `I32 | `I64] [@@deriving sexp]
type vec_lane_shape = [ int_vec_lane_shape | `F32 | `F64 ]
[@@deriving sexp]

type branch_target = Block of int [@@unboxed ][@@deriving sexp]
type variable = { name : ident; typ : local_type } [@@deriving sexp, compare]
type func_sig = { args : variable list; returns : variable list } [@@deriving sexp]
