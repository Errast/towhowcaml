open! Core
open Mir
open Radatnet

type state = private {
  mutable compare_args : (Instr.Ref.t * X86reg.gpr_type) list;
  mutable compare_instr : X86_instr.t;
  mutable input_condition_instr : X86_instr.t;
  mutable changed_flags : Status_flags.t;
  mutable fpu_status_word_args : Instr.Ref.t list;
  mutable fpu_status_word_instr : X86_instr.t;
  mutable float_compare_args : Instr.Ref.t list;
  mutable float_compare_instr : X86_instr.t;
  mutable backward_direction : bool;
  mutable fpu_stack_pointer : Instr.Ref.t;
  mutable fpu_stack_pointer_offset : int;
  mutable fpu_status_word : Instr.Ref.t;
}

val initial_state : unit -> state
val translate : Mir.Builder.t -> state -> Radatnet.Types.opcode -> unit
val translate_output_condition : Mir.Builder.t -> state -> X86_instr.t -> unit

type term_trans_result =
  | Nothing
  | Unconditional of { target : int }
  | Conditional of { target : int; condition : Mir.Instr.ref }

val translate_terminator :
  Mir.Builder.t -> state -> Radatnet.Types.opcode -> term_trans_result
