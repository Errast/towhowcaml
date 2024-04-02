open Core

type t = { name : Types.ident; subscript : int } [@@deriving sexp]
