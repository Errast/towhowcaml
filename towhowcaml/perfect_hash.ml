(*
Very stripped-down version of PtrHash. Not minimal, no parts
*)
open! Core

(* open Util *)
module AP = Array.Permissioned

type bucket = |

let sexp_of_bucket _ = failwith ""

type slot = |
type key = |
type pilot = |

module Idx = struct
  type 'a t = { i : int } [@@unboxed]

  let bucket i : bucket t = { i }
  let slot i : slot t = { i }
  let key i : key t = { i }
  let pilot i : pilot t = { i }
  let succ : 'a t -> 'a t = fun i -> { i = i.i + 1 }
  let equal : 'a t -> 'a t -> bool = fun l r -> l.i = r.i
  let compare : 'a t -> 'a t -> int = fun l r -> compare l.i r.i
  let sexp_of_t _ { i } = sexp_of_int i
end

type 'a idx = 'a Idx.t = { i : int } [@@unboxed]

let sexp_of_idx = Idx.sexp_of_t

module type Array = sig
  type ('idx, 'elem) t
  type 'a elem

  val init : int -> f:('a idx -> 'b elem) -> ('a, 'b) t
  val create : int -> 'b elem -> ('a, 'b) t
  val get : ('a, 'b) t -> 'a idx -> 'b elem
  val set : ('a, 'b) t -> 'a idx -> 'b elem -> unit
  val get_set : ('a, 'b) t -> 'a idx -> 'b elem -> 'b elem
  val length : ('a, _) t -> int
  val fill : ('a, 'b) t -> 'b elem -> unit
  val sexp_of_t : ('b elem -> Sexplib.Sexp.t) -> ('a, 'b) t -> Sexplib.Sexp.t
end

module Bool_array : sig
  type 'a t

  include Array with type _ elem := bool with type ('a, _) t := 'a t

  val or_ : dest:'a t -> src:'a t -> unit
  val intersections : 'a t -> 'a t -> int
end = struct
  type _ t = Bytes.t
  type exn += BadBoolArray : 'a t * int -> exn

  let get t { i } =
    let byte_i = i / 8 in
    let bit = 1 lsl (i mod 8) in
    if i >= Bytes.length t * 8 then raise @@ BadBoolArray (t, i);
    (Bytes.get t byte_i |> Char.to_int) land bit <> 0

  let get_set t { i } b =
    let byte_i = i / 8 in
    let bit = 1 lsl (i mod 8) in
    let byte = Bytes.get t byte_i |> Char.to_int in
    let new_byte =
      Char.unsafe_of_int @@ if b then byte lor bit else byte land Int.lnot bit
    in
    Bytes.set t byte_i new_byte;
    byte land bit <> 0

  let set t i b = get_set t i b |> ignore

  let create len b =
    assert (len >= 0);
    let byte_len = if len mod 8 = 0 then len / 8 else (len / 8) + 1 in
    Bytes.make byte_len (if b then '\255' else '\000')

  let init len ~f =
    let t = create len false in
    for i = 0 to len - 1 do
      let i = { i } in
      set t i (f i)
    done;
    t

  let length t = Bytes.length t * 8

  let fill t b =
    Bytes.fill t ~pos:0 ~len:(Bytes.length t) @@ if b then '\255' else '\000'

  let intersections s t =
    assert (length s = length t);
    let count = ref 0 in
    let len = Bytes.length s in
    for i = 0 to (len / 8) - 1 do
      let byte = i * 8 in
      let si = Bytes.unsafe_get_int64 s byte in
      let ti = Bytes.unsafe_get_int64 t byte in
      let ones = Int64.(popcount @@ (si land ti)) in
      if ones <> 0 then count := !count + ones
    done;
    for i = len - (len mod 8) to len - 1 do
      let ones =
        (Bytes.unsafe_get s i |> Char.to_int)
        land (Bytes.unsafe_get t i |> Char.to_int)
        |> Int.popcount
      in
      if ones <> 0 then count := !count + ones
    done;
    !count

  let or_ ~dest:s ~src:t =
    assert (length s = length t);
    let len = Bytes.length s in
    for i = 0 to (len / 8) - 1 do
      let byte = i * 8 in
      let si = Bytes.unsafe_get_int64 s byte in
      let ti = Bytes.unsafe_get_int64 t byte in
      Bytes.unsafe_set_int64 s byte Int64.(si lor ti)
    done;
    for i = len - (len mod 8) to len - 1 do
      let si = Bytes.unsafe_get s i |> Char.to_int in
      let ti = Bytes.unsafe_get t i |> Char.to_int in
      Bytes.unsafe_set s i (si lor ti |> Char.of_int_exn)
    done

  let sexp_of_t _ t = Bytes.sexp_of_t t
end

module Byte_array : sig
  type 'a t

  include Array with type _ elem := char with type ('a, _) t := 'a t

  val to_string : 'a t -> string
end = struct
  type 'a t = Bytes.t

  let get t { i } = Bytes.get t i
  let set t { i } e = Bytes.set t i e

  let get_set t { i } e =
    let e_old = Bytes.get t i in
    Bytes.set t i e;
    e_old

  let create len e = Bytes.make len e
  let length = Bytes.length

  let init len ~f =
    let t = Bytes.create len in
    for i = 0 to len - 1 do
      let i = { i } in
      set t i (f i)
    done;
    t

  let fill t b = Bytes.fill t ~pos:0 ~len:(Bytes.length t) b
  let sexp_of_t _ = Bytes.sexp_of_t
  let to_string = Bytes.to_string
end

module Std_array : sig
  include Array with type 'a elem := 'a

  val of_array : ('a, read_write) AP.t -> (_, 'a) t
end = struct
  type (_, 'elem) t = ('elem, read_write) AP.t

  let get t { i } = AP.get t i
  let set t { i } e = AP.set t i e

  let get_set t i e =
    let e_old = get t i in
    set t i e;
    e_old

  let create len e = AP.create ~len e
  let init len ~f = AP.init len ~f:(fun i -> f { i })
  let length = AP.length
  let fill t e = AP.fill t ~pos:0 ~len:(length t) e
  let of_array x = x
  let sexp_of_t sexp_of_elem t = [%sexp_of: (elem, _) AP.t] t
end

let hash32_constant = 0x9e3779b9

let hash32 : seed:int -> int -> int =
 fun ~seed i ->
  assert (seed <= 0xFFFFFFFF && seed <= 0xFFFFFFFF);
  let mult = i * hash32_constant in
  mult lxor seed land 0xFFFFFFFF

(* hopefully inlining unboxes it*)
let[@inline] mul_high : Int64.t -> Int64.t -> Int64.t =
 fun lhs rhs -> Int64.((lhs * rhs) lsr 32)

(* from https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction  *)
let[@inline] reduce32 x n = mul_high x n |> Int64.to_int_trunc

let[@inline] get_bucket : int -> buckets:int -> bucket idx =
 fun h ~buckets ->
  let hash_i64 = Int64.of_int h in
  reduce32 hash_i64 (Int64.of_int buckets) |> Idx.bucket

type t = { seed : int; buckets : int; pilot_table : string; slots : int }
[@@deriving sexp_of]

let sort_buckets :
    (key, int) Std_array.t ->
    int ->
    (bucket, key idx) Std_array.t * (bucket idx, immutable) AP.t =
 fun hashes buckets ->
  let start = ref @@ Idx.key 0 in
  let rec find_bucket_end bucket cursor =
    if
      cursor.i >= Std_array.length hashes
      || not @@ Idx.equal bucket
         @@ get_bucket (Std_array.get hashes cursor) ~buckets
    then cursor
    else find_bucket_end bucket (Idx.succ cursor)
  in
  let bucket_starts =
    Std_array.init (buckets + 1) ~f:(function
      | { i = 0 } -> Idx.key 0
      | { i } ->
          let bucket_end = find_bucket_end (Idx.bucket @@ (i - 1)) !start in
          start := bucket_end;
          bucket_end)
  in
  let bucket_order = AP.init buckets ~f:Idx.bucket in
  let bucket_order =
    AP.sorted_copy bucket_order ~compare:(fun l r ->
        compare
          ((Std_array.get bucket_starts (Idx.succ r)).i
         - (Std_array.get bucket_starts r).i)
          ((Std_array.get bucket_starts (Idx.succ l)).i
         - (Std_array.get bucket_starts l).i))
  in
  (bucket_starts, bucket_order)

module A = Base.Queue

(* TODO: replace mod with multiply-and-shift thing *)
(* Because our hash is a |-> (a*c) xor seed mod 2^32, xoring two of these together like we do here makes no difference,
   so we could make do with onl (a*c) mod 2^32, but xoring is basically free *)
let[@inline] position ~hpilot ~hkey ~slots =
  Idx.slot @@ (hpilot lxor hkey mod slots)

exception NoPilotFoundException

let find_pilots ~slots ~(hashes : (key, int) Std_array.t)
    ~(bucket_starts : (bucket, key idx) Std_array.t)
    ~(bucket_order : (bucket idx, _) AP.t) ~seed =
  let open struct end in
  let module Weighted_bucket = struct
    type t = { weight : int; bucket : bucket idx } [@@deriving sexp_of]

    let compare l r =
      match compare l.weight r.weight with
      | 0 -> Idx.compare l.bucket r.bucket
      | c -> c

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end in
  let pilot_table : bucket Byte_array.t =
    Byte_array.create (AP.length bucket_order) '\000'
  in
  let slot_owners : (slot, bucket idx) Std_array.t =
    Std_array.create slots (Idx.bucket @@ -1)
  in
  let bucket_size b =
    (Std_array.get bucket_starts @@ Idx.bucket (b.i + 1)).i
    - (Std_array.get bucket_starts b).i
  in
  let set_bucket_slots bucket value =
    let hpilot =
      Byte_array.get pilot_table bucket |> Char.to_int |> hash32 ~seed
    in
    for
      i = (Std_array.get bucket_starts bucket).i
      to (Std_array.get bucket_starts @@ Idx.succ bucket).i - 1
    do
      let pos =
        position ~hpilot ~hkey:(Std_array.get hashes @@ Idx.key i) ~slots
      in
      Std_array.set slot_owners pos value
    done
  in
  let check_pilot : hpilot:int -> istart:key idx -> iend:key idx -> bool =
    let bucket_taken : slot Bool_array.t = Bool_array.create slots false in
    fun ~hpilot ~istart ~iend ->
      let rec go i ~hpilot ~iend =
        if i.i >= iend.i then true
        else
          let pos = position ~hpilot ~hkey:(Std_array.get hashes i) ~slots in
          if
            (Std_array.get slot_owners pos).i >= 0
            || Bool_array.get_set bucket_taken pos true
          then false
          else go (Idx.succ i) ~hpilot ~iend
      in

      Bool_array.fill bucket_taken false;
      go istart ~hpilot ~iend
  in
  let find_best_eviction ~istart ~iend ~seed =
    let offset = Random.int 256 in
    let rec go_pilot =
      let bucket_taken = Bool_array.create slots false in
      fun i ~best_pilot ~best_score ->
        if i > 255 then best_pilot
        else
          let pilot = Idx.pilot @@ ((offset + i) mod 256) in
          Bool_array.fill bucket_taken false;
          let hpilot = hash32 ~seed pilot.i in
          let rec go_entry i score =
            if i.i >= iend.i || score >= best_score then score
            else
              let pos =
                position ~hpilot ~hkey:(Std_array.get hashes i) ~slots
              in
              if Bool_array.get_set bucket_taken pos true then Int.max_value
              else
                match Std_array.get slot_owners pos with
                | b when b.i < 0 -> go_entry (Idx.succ i) score
                | b -> go_entry (Idx.succ i) (score + Int.pow (bucket_size b) 2)
          in
          let pilot_score = go_entry istart 0 in
          if pilot_score < best_score then
            go_pilot (i + 1) ~best_pilot:pilot ~best_score:pilot_score
          else go_pilot (i + 1) ~best_pilot ~best_score
    in
    let result =
      go_pilot ~best_pilot:(Idx.pilot @@ -1) ~best_score:Int.max_value 0
    in
    if result.i = Int.max_value then failwith "Nope?" else result
  in
  let evict_conflicts ~bucket ~istart ~iend ~hpilot acc =
    let rec go i acc =
      if i.i >= iend.i then acc
      else
        let pos = position ~hpilot ~hkey:(Std_array.get hashes i) ~slots in
        let old_bucket = Std_array.get slot_owners pos in
        let acc' =
          if old_bucket.i >= 0 then (
            set_bucket_slots old_bucket (Idx.bucket @@ -1);
            Set.add acc
              Weighted_bucket.
                {
                  weight = Int.pow (bucket_size old_bucket) 2;
                  bucket = old_bucket;
                })
          else acc
        in
        Std_array.set slot_owners pos bucket;
        go (Idx.succ i) acc'
    in
    go istart acc
  in
  let find_pilot bucket =
    let rec go_pilot ~pilot ~istart ~iend ~seed =
      if pilot.i > 255 then raise_notrace NoPilotFoundException
      else
        let hpilot = hash32 ~seed pilot.i in
        if check_pilot ~hpilot ~istart ~iend then pilot
        else go_pilot ~pilot:(Idx.succ pilot) ~istart ~iend ~seed
    in

    let rec go pqueue =
      match Set.max_elt pqueue with
      | None -> ()
      | Some Weighted_bucket.{ bucket; _ } -> (
          let istart = Std_array.get bucket_starts bucket in
          let iend = Std_array.get bucket_starts (Idx.succ bucket) in
          let pqueue = Set.remove_index pqueue @@ (Set.length pqueue - 1) in
          try
            let pilot = go_pilot ~pilot:(Idx.pilot 0) ~istart ~iend ~seed in

            (* claim pilot and slots *)
            Byte_array.set pilot_table bucket (Char.of_int_exn pilot.i);
            set_bucket_slots bucket bucket
          with NoPilotFoundException ->
            let pilot = find_best_eviction ~istart ~iend ~seed in
            if pilot.i = -1 then raise NoPilotFoundException;
            Byte_array.set pilot_table bucket (Char.of_int_exn pilot.i);
            let hpilot = hash32 ~seed pilot.i in
            let evicted =
              evict_conflicts ~bucket ~istart ~iend ~hpilot pqueue
            in
            go evicted)
    in
    go (Set.singleton (module Weighted_bucket) { bucket; weight = 0 })
  in
  AP.iter bucket_order ~f:find_pilot;
  Byte_array.to_string pilot_table

let rand_seed () = Random.int Int.max_value land 0xFFFFFFFF

let make_perfect_hash :
    ?alpha:float -> ?bucket_size:float -> ?seed:int -> (int, _) AP.t -> t option
    =
 fun ?(alpha = 0.99) ?(bucket_size = 3.0) ?(seed = rand_seed ()) keys ->
  let n = AP.length keys in
  let slots = Float.of_int n /. alpha |> Float.round_up |> Int.of_float in
  let buckets =
    Float.of_int n /. bucket_size |> Float.round_up |> Int.of_float |> ( + ) 3
  in
  let hashes = AP.map ~f:(hash32 ~seed) keys in
  AP.sort hashes ~compare:Int.compare;
  let hashes : (key, int) Std_array.t = Std_array.of_array hashes in
  let bucket_starts, bucket_order = sort_buckets hashes buckets in
  (* print_s *)
    (* [%message *)
      (* "" *)
        (* (seed : int) *)
        (* (slots : int) *)
        (* (buckets : int) *)
        (* (bucket_starts : _ idx Std_array.t) *)
        (* (bucket_order : (_ idx, _) AP.t)]; *)
  try
    let pilot_table =
      find_pilots ~hashes ~bucket_starts ~bucket_order ~slots ~seed
    in

    Some { slots; buckets; pilot_table; seed }
  with NoPilotFoundException -> None

let hash { buckets; seed; pilot_table; slots } key =
  let hkey = hash32 ~seed key in
  let bucket = get_bucket ~buckets hkey in
  let pilot = String.get pilot_table bucket.i in
  let hpilot = hash32 ~seed (Char.to_int pilot) in
  (position ~hpilot ~hkey ~slots).i

module DSL = Elbrun.DSL

module Elbrun = struct
  let hash32 key ~seed = DSL.((key * i hash32_constant) ^ i seed)
  let reduce32 x ~n = DSL.(trunc (extend_z x *: l n >>: u (l 32)))
  let get_bucket ~buckets hkey = reduce32 ~n:buckets hkey

  let hash { seed; buckets; slots; _ } pilot_table_ptr key =
    DSL.(
      block
        [
          "hkey" =% hash32 ~seed key;
          "bucket" =% get_bucket !%" hkey" ~buckets;
          "pilot" =% load_z i8 (!%"bucket" + i pilot_table_ptr);
          "hpilot" =% hash32 !%"pilot" ~seed;
        ]
        ((!%"hpilot" ^ !%"hkey") % u (i slots)))
end
