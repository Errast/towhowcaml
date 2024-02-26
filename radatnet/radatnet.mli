type t = Wrapper.t

exception Radatnet_disposed

val create : unit -> t
val run : t -> string -> string

module Commands = Commands

module Types = Types
