open! Core

val test : Block.t -> unit
val run : Out_channel.t -> Func.t -> unit
val run_block : Out_channel.t -> Block.t -> unit
