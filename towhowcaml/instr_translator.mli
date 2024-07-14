open! Core
open Radatnet

type state

val initial_state : unit -> state
val backward_direction : state -> bool
val input_condition_opcode : state -> opcode option
val compare_instr : state -> X86_instr.t

val translate :
  (int, Util.intrinsic) Hashtbl.t ->
  Mir.Builder.t ->
  state ->
  Radatnet.Types.opcode ->
  unit

val translate_output_condition : Mir.Builder.t -> state -> opcode -> unit

type term_trans_result =
  | Nothing
  | Unconditional of { target : int }
  | Conditional of { target : int; condition : Mir.Instr.ref }
  | Switch of { switch_on : Mir.Instr.ref; table_addr : int }
  | Return
[@@deriving sexp_of]

val translate_terminator :
  (int, Util.intrinsic) Hashtbl.t ->
  Mir.Builder.t ->
  state ->
  Radatnet.Types.opcode ->
  tail_position:bool ->
  term_trans_result

val print : unit -> int
