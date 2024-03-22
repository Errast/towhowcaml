open Core

type 'a t
val empty : unit -> 'a t
val create : ?cap:int -> unit -> 'a t
val valid_index : 'a t -> int -> unit
val get : 'a t -> int -> 'a
val unsafe_get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val add : 'a t -> 'a -> unit
val copy : 'a t -> 'a t
include Indexed_container.Generic with type ('a, _) t := 'a t and type 'a elt := 'a
include Sexpable.S1 with type 'a t := 'a t
