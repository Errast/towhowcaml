open! Core

let sexp_of_iarray sexp_of_t arr =
  Array.mapi ~f:(fun i a -> (i, a)) arr
  |> sexp_of_array (sexp_of_pair sexp_of_int sexp_of_t)

let sexp_of_iparray sexp_of_t sexp_of_perm arr =
  Array.Permissioned.mapi ~f:(fun i a -> (i, a)) arr
  |> Array.Permissioned.sexp_of_t
       (sexp_of_pair sexp_of_int sexp_of_t)
       sexp_of_perm
