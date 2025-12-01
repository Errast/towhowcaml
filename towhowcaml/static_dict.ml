open! Core
module AP = Array.Permissioned

module Packed_bitarray = struct
  type t = { bytes : Bytes.t; width : int; length : int } [@@deriving equal]

  let word_bits = 32
  let word_bytes = word_bits / 8

  let get { bytes; width; length } i =
    assert (i < length);
    let bit_index = i * width in
    let byte_index = bit_index / 8 in
    let word = Bytes.unsafe_get_int32 bytes byte_index |> Int32.to_int_exn in
    let shift = bit_index mod 8 in
    let mask = (1 lsl width) - 1 in
    (word lsr shift) land mask

  let set { bytes; width; length } i x =
    assert (i < length && x >= 0 && x < 1 lsl width);
    let bit_index = i * width in
    let byte_index = bit_index / 8 in
    if byte_index + word_bytes > Bytes.length bytes then
      failwith "index out of range";
    let word = Bytes.unsafe_get_int32 bytes byte_index |> Int32.to_int_exn in
    let shift = bit_index mod 8 in
    let mask = ((1 lsl width) - 1) lsl shift in
    Bytes.unsafe_set_int32 bytes byte_index
    @@ Int32.of_int_exn
    @@ (((word land Int.bit_not mask lor (x lsl shift)) lsl 31) asr 31)

  let create_zeroed ~len ~width =
    assert (len * width < 0xFFFFFFFF);
    assert (width + 7 <= 32);
    assert (width > 0);
    let bytes_len = (((((len * width) - 1) lor 0x7) + 1) / 8) + 3 in
    let bytes = Bytes.create bytes_len in
    Bytes.fill bytes ~pos:0 ~len:bytes_len '\000';
    { bytes; width; length = len }

  let create ~len ~width x =
    let t = create_zeroed ~len ~width in
    for i = 0 to len - 1 do
      set t i x
    done;
    t

  let of_array ~width l =
    let arr = create_zeroed ~len:(Array.length l) ~width in
    Array.iteri ~f:(set arr) l;
    arr

  let to_array t = Array.init t.length ~f:(get t)
  let to_string t = Bytes.to_string t.bytes

  module Elbrun = struct
    open Elbrun.DSL

    let word_bits_div = Int.floor_log2 word_bits

    let get ~array_ptr ~width index =
      comp [ index ] @@ fun [ index ] ->
      block
        [
          "bit_index" =% index * i width;
          "byte_index" =% (!%"bit_index" >> u (i 8));
          "word" =% load int (!%array_ptr + !%"byte_index");
          "shift" =% (!%"bit_index" & i 7);
          "mask" =% (i 1 << i width) - i 1;
        ]
        (!%"word" >> u !%"shift" & !%"mask")
  end
end

type t = { phf : Perfect_hash.t; min_key : int; table : Packed_bitarray.t }

let make_static_dict : (int, _) AP.t -> Perfect_hash.t -> t =
 fun keys phf ->
  let min = AP.min_elt keys ~compare:Int.compare |> Option.value_exn in
  let max = AP.max_elt keys ~compare:Int.compare |> Option.value_exn in
  assert (max - min < 1 lsl 32);
  let table =
    Packed_bitarray.create ~len:phf.slots ~width:(Int.ceil_log2 (max - min)) 1
  in
  AP.iter keys ~f:(fun k ->
      let h = Perfect_hash.hash phf k in
      assert (Packed_bitarray.get table h = 0);
      Packed_bitarray.set table h (k - min));
  { phf; min_key = min; table }

let query { phf; table; min_key } x =
  let h = Perfect_hash.hash phf x in
  let offset = x - min_key in
  if Packed_bitarray.get table offset = offset then Some h else None

type bitarray = Packed_bitarray.t = {
  bytes : Bytes.t;
  width : int;
  length : int;
}

module Elbrun = struct
  let query ~dict_ptr ~phf_pointer ~dict key =
    Elbrun.DSL.(
      comp [ key ] @@ fun [ key ] ->
      Perfect_hash.Elbrun.hash dict.phf phf_pointer key
      |> Packed_bitarray.Elbrun.get ~array_ptr:dict_ptr ~width:dict.table.width)
end
