open! Core


type t = { seed : int; buckets : int; pilot_table : string; slots : int }
[@@deriving sexp_of]

val make_perfect_hash :
  ?alpha:float ->
  ?bucket_size:float ->
  ?seed:int ->
  (int, [> read ]) Array.Permissioned.t ->
  t option

val hash : t -> int -> int

module Elbrun : sig
  open Elbrun.DSL

  val hash : t -> int -> int_ expr -> int_ expr
end
