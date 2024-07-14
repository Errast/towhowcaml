open Core

type 'a t = { mutable array : 'a Option_array.t; mutable length : int }

let empty () = { array = Option_array.empty; length = 0 }

let create ?(cap = 4) () =
  {
    array = Option_array.create ~len:(if cap = 0 then 0 else Int.ceil_pow2 cap);
    length = 0;
  }

let init length ?(cap = length) f =
  assert (cap >= length);
  assert (length >= 0);
  let array = Option_array.create ~len:cap in
  for i = 0 to length - 1 do
    Option_array.set_some array i @@ f i
    (* Option_array.unsafe_set_some array i @@ f i *)
  done;
  { array; length }

let length vec = vec.length
let valid_index vec i = i < length vec

let valid_index_exn vec i =
  if not @@ valid_index vec i then
    raise @@ Invalid_argument "Index out of range"
  else ()

let get vec i =
  valid_index_exn vec i;
  (* Option_array.unsafe_get_some_assuming_some vec.array i *)
  Option_array.get_some_exn vec.array i

let unsafe_get vec i =
  (* Option_array.unsafe_get_some_assuming_some *)
  Option_array.get_some_exn vec.array i

let get_opt vec i = if valid_index vec i then Some (unsafe_get vec i) else None

let set vec i value =
  valid_index_exn vec i;
  (* Option_array.unsafe_set_some vec.array i value *)
  Option_array.set_some vec.array i value

let add vec value =
  let len = length vec in
  if len >= Option_array.length vec.array then (
    let new_arr =
      Option_array.create ~len:(Int.max 4 @@ (Int.floor_pow2 2 * len))
    in
    Option_array.blito ~src:vec.array ~dst:new_arr ();
    vec.array <- new_arr);
  (* Option_array.unsafe_set_some vec.array len value *)
  Option_array.set_some vec.array len value;
  vec.length <- len + 1

let pop vec =
  if vec.length < 1 then raise @@ Invalid_argument "Vec is empty";
  vec.length <- vec.length - 1;
  (* Option_array.unsafe_get_some_assuming_some vec.array vec.length *)
  let res = Option_array.get_some_exn vec.array vec.length in
  (* Option_array.unsafe_set_none vec.array vec.length *)
  Option_array.set_none vec.array vec.length;
  res

let insert vec i value =
  let len = vec.length in
  vec.length <- len + 1;
  valid_index_exn vec i;
  if len = Option_array.length vec.array then (
    let new_arr =
      Option_array.create ~len:(Int.max 4 @@ (Int.floor_pow2 2 * len))
    in
    if i > 0 then
      Option_array.blit ~src:vec.array ~dst:new_arr ~src_pos:0 ~dst_pos:0 ~len:i;
    if len <> i then
      Option_array.blit ~src:vec.array ~dst:new_arr ~src_pos:i ~dst_pos:(i + 1)
        ~len:(len - i);
    vec.array <- new_arr)
  else if i < len then
    Option_array.blit ~src:vec.array ~dst:vec.array ~src_pos:i ~dst_pos:(i + 1)
      ~len:(len - 1);
  (* Option_array.unsafe_set_some vec.array i value *)
  Option_array.set_some vec.array i value

let copy vec = { vec with array = Option_array.copy vec.array }

let iteri vec ~f =
  for i = 0 to length vec - 1 do
    f i (unsafe_get vec i)
  done

let foldi vec ~init ~f =
  let acc = ref init in
  iteri vec ~f:(fun i v -> acc := f i !acc v);
  !acc

let fold vec ~init ~f = foldi vec ~init ~f:(fun _ acc x -> f acc x)

include Indexed_container.Make_gen (struct
  type nonrec ('a, _, _) t = 'a t
  type 'a elt = 'a

  let length = `Custom length
  let fold = fold
  let foldi = `Custom foldi
  let iter = `Custom (fun vec ~f -> iteri vec ~f:(fun _ v -> f v))
  let iteri = `Custom iteri
end)

(* Shadow the indexe-container stuff *)
let of_array arr =
  { array = Option_array.of_array_some arr; length = Array.length arr }

let to_array vec =
  Array.init vec.length
    ~f:
      ((* Option_array.unsafe_get_some_assuming_some *)
       Option_array.get_some_exn vec.array)

let to_perm_array vec =
  Array.Permissioned.init vec.length
    ~f:
      (Option_array.unsafe_get_some_assuming_some
         (*Option_array.get_some_exn*) vec.array)

include
  Sexpable.Of_sexpable1
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_sexpable = to_array
      let of_sexpable = of_array
    end)
