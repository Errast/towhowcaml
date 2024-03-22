open Core

type 'a t = { mutable array: 'a Option_array.t; mutable length: int }

let empty () = { array = Option_array.empty; length = 0 }
let create ?(cap=4) () = { array = Option_array.create ~len:(if cap = 0 then 0 else Int.ceil_pow2 cap); length=0 }
let length vec = vec.length
let valid_index vec i = if length vec <= i then raise @@ Invalid_argument "Index out of range" else ()
let get vec i = valid_index vec i; (* Option_array.unsafe_get_some_assuming_some vec.array i *)
                Option_array.get_some_exn vec.array i
let unsafe_get vec i = (* Option_array.unsafe_get_some_assuming_some *) Option_array.get_some_exn vec.array i

let set vec i value = valid_index vec i; (* Option_array.unsafe_set_some vec.array i value *)
                      Option_array.set_some vec.array i value

let add vec value = let len = length vec in
                    if len >= Option_array.length vec.array
                    then let new_arr = Option_array.create ~len:(Int.min 4 @@ Int.ceil_pow2 @@ len + 1) in
                         Option_array.blito ~src:vec.array ~dst:new_arr ();
                         vec.array <- new_arr;
                    else (* Option_array.unsafe_set_some vec.array len value *) Option_array.set_some vec.array len value;
                    vec.length <- len + 1

let copy vec = { vec with array = Option_array.copy vec.array; }

let iteri vec ~f =
  for i = 0 to length vec - 1 do
    f i (unsafe_get vec i)
  done

let foldi vec ~init ~f =
  let acc = ref init in
  iteri vec ~f:(fun i v -> acc := f i !acc v);
  !acc

let fold vec ~init ~f = foldi vec ~init ~f:(fun _ acc x -> f acc x)

include Indexed_container.Make_gen(struct
    type nonrec ('a, _) t = 'a t
    type 'a elt = 'a
    let length = `Custom length
    let fold = fold
    let foldi = `Custom foldi
    let iter = `Custom (fun vec ~f -> iteri vec ~f:(fun _ v -> f v))
    let iteri = `Custom iteri
  end)


(* Shadow the indexe-container stuff *)
let of_array arr = { array=Option_array.of_array_some arr; length = Array.length arr }
let to_array vec = Array.init vec.length ~f:((* Option_array.unsafe_get_some_assuming_some *) Option_array.get_some_exn vec.array)

include Sexpable.Of_sexpable1(Array)(struct type nonrec 'a t = 'a t let to_sexpable = to_array let of_sexpable = of_array end)
