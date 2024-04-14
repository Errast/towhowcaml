open Core

type t = { name : Types.ident } [@@unboxed] [@@deriving sexp]

include
  Sexpable.Of_sexpable
    (String)
    (struct
      type nonrec t = t

      let to_sexpable t = t.name
      let of_sexpable s = { name = s }
    end)
