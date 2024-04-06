type t 

external create: unit -> t = "radatnet_core_new"

external run: t -> string -> string = "radatnet_core_cmd_str"

