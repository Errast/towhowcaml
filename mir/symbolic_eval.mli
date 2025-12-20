open Core
open Smt_util

type ctx = {
  vals : expr Option_array.t;
  memory : Array.t Map.M(Int).t;
  vars : expr Map.M(String).t;
}

val interpret_instr : ctx -> int -> Instr.t -> ctx
