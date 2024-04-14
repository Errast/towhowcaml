open! Core

type t = (Instr.t, immutable) Array.Permissioned.t

let sexp_of_t vec =
  Sexp.List
    (Array.Permissioned.mapi vec ~f:(fun i instr ->
         Sexp.List [ sexp_of_int i; Instr.sexp_of_t instr ])
    |> Array.Permissioned.to_list)

let t_of_sexp sexp =
  [%of_sexp: (Sexp.t * Instr.t, immutable) Array.Permissioned.t] sexp
  |> Array.Permissioned.map ~f:snd
