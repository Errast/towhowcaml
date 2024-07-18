open! Core
open Types

type t = {
  name : ident;
  signature : func_sig;
  blocks : (Block.t, immutable) Array.Permissioned.t;
  locals : variable Map.M(String).t; 
}
[@@deriving sexp]
