type t
type basic_opcode = { address : int; prefix : int; id : int; opex_str : string }

external create : unit -> t = "radatnet_core_new"
external run : t -> string -> string = "radatnet_core_cmd_str"
external run_at : t -> int -> string -> string = "radatnet_core_cmd_str_at"
external analyze_opcode : t -> int -> basic_opcode = "radatnet_core_anal_op"
