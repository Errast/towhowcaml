open! Core

type t [@@deriving sexp, compare, hash]

val none : t
val sign : t
val overflow : t
val carry : t
val zero : t
val parity : t
val ( lor ) : t -> t -> t
val all : t
