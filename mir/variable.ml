open Core

type t = { name : Types.ident; subscript : int } [@@deriving sexp]

let sexp_of_t t : Sexp.t = List [ Atom t.name; sexp_of_int t.subscript ]

let t_of_sexp = function
  | Sexp.List [ Atom name; subscript ] ->
      { name; subscript = int_of_sexp subscript }
  | sexp ->
      Sexplib.Conv.of_sexp_error_exn (Failure "can't parse Mir.Variable.t") sexp
