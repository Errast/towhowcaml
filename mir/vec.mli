open Core

type 'a t
val empty : unit -> 'a t
val create : ?cap:int -> unit -> 'a t
val length : 'a t -> int
val valid_index : 'a t -> int -> unit
val get : 'a t -> int -> 'b
val unsafe_get : 'a t -> 'b -> 'c
val set : 'a t -> int -> 'b -> 'c
val add : 'a t -> 'b -> unit
val of_array : 'a array -> 'b t
val copy : 'a t -> 'b t
include Indexed_container.Generic with type ('a, _) t := 'a t and type 'a elt = 'a
include Sexpable.S1 with type 'a t := 'a t
