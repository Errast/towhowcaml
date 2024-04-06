type state

val initial_state : unit -> state
val translate : Mir.Builder.t -> state -> Radatnet.Types.opcode -> unit
