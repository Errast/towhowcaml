type t
exception Radatnet_disposed
val create : unit -> t
val run: t -> string -> string
