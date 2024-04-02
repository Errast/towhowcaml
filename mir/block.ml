open Core
open Types

type block = {
  id : block_ref;
  next_block : block_ref;
  conditional : block_ref;
  instrs : (Instr.t, Perms.Export.immutable) Array.Permissioned.t;
  roots : Local_info.t Map.M(String).t;
}
[@@deriving sexp]
