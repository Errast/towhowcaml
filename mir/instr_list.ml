open! Core
module AP = Array.Permissioned

type builder = Instr.t Vec.t
type t = (Instr.t, immutable) AP.t

let t_of_builder = Vec.to_perm_array

let sexp_of_builder vec =
  Sexp.List
    (Vec.to_array vec
    |> Array.mapi ~f:(fun i instr ->
           Sexp.List [ sexp_of_int i; Instr.sexp_of_t instr ])
    |> List.of_array)

let sexp_of_t vec =
  Sexp.List
    (AP.mapi vec ~f:(fun i instr ->
         Sexp.List [ sexp_of_int i; Instr.sexp_of_t instr ])
    |> AP.to_list)

let t_of_sexp sexp =
  [%of_sexp: (Sexp.t * Instr.t) array] sexp
  |> AP.of_array_id
  |> AP.map ~f:snd
