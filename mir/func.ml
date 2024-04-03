open! Core
open Types

type t = {
  name : ident;
  signature : func_sig;
  blocks : Block.t;
  locals : local Map.M(String).t;
}
[@@deriving sexp]
