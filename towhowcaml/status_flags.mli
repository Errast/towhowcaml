open! Core

type t [@@immediate]

include Comparable.S with type t := t

val none : t
val sign : t
val overflow : t
val carry : t
val zero : t
val parity : t
val ( lor ) : t -> t -> t
val all : t
