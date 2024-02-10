type t
exception Radatnet_disposed
val create : unit -> t
val destroy : t -> unit
val run: t -> string -> string option
