open! Core
open Types

type t = {
  name : ident;
  signature : func_sig;
  blocks : Block.t array;
  locals : variable Map.M(String).t;
}
[@@deriving sexp]
