type t
type basic_opcode = { address : int; prefix : int; id : int; opex_str : string }

val create : unit -> t
val run : t -> string -> string
val run_at : t -> int -> string -> string
val analyze_opcode : t -> int -> basic_opcode
