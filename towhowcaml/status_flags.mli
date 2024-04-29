open! Core

type t [@@deriving sexp, compare, hash, equal]

val none : t
val sign : t
val overflow : t
val carry : t
val zero : t
val parity : t
val ( %| ) : t -> t -> t
val ( %& ) : t -> t -> t
val all : t
