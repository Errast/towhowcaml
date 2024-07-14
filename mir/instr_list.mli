open Core
type builder = Instr.t Vec.t [@@deriving sexp_of]
type t = (Instr.t,immutable) Array.Permissioned.t [@@deriving sexp]
val t_of_builder : builder -> t
