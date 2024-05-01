open! Core

type builder = Instr.t Vec.t
type t = Instr.t array

let t_of_builder = Vec.to_array

let sexp_of_builder vec =
  Sexp.List
    (Vec.to_array vec
    |> Array.mapi ~f:(fun i instr ->
           Sexp.List [ sexp_of_int i; Instr.sexp_of_t instr ])
    |> List.of_array)

let sexp_of_t vec =
  Sexp.List
    (Array.mapi vec ~f:(fun i instr ->
         Sexp.List [ sexp_of_int i; Instr.sexp_of_t instr ])
    |> Array.to_list)

let t_of_sexp sexp =
  [%of_sexp: (Sexp.t * Instr.t) array] sexp |> Array.map ~f:snd
