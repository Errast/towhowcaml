open Core

type !'a t

val empty : unit -> 'a t
val create : ?cap:int -> unit -> 'a t
val init : int -> ?cap:int -> (int -> 'a) -> 'a t
val clear : 'a t -> unit
val get : 'a t -> int -> 'a
val get_opt : 'a t -> int -> 'a option
val unsafe_get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val add : 'a t -> 'a -> unit
val pop_exn : 'a t -> 'a
val insert : 'a t -> int -> 'a -> unit
val append : 'a t -> 'a t -> unit
val singleton : 'a -> 'a t
val reserve_for : 'a t -> int -> unit
val copy : 'a t -> 'a t
val of_array : 'a array -> 'a t
val of_array_perm : ('a, [> read ]) Array.Permissioned.t -> 'a t
val to_perm_array : 'a t -> ('a, [< 'b perms ]) Array.Permissioned.t

include
  Indexed_container.Generic with type ('a, _, _) t := 'a t and type 'a elt := 'a

include Sexpable.S1 with type 'a t := 'a t
