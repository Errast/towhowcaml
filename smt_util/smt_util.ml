open! Core

type 'a domain_local = unit -> 'a
type expr = Z3.Expr.expr

let equal_expr = Z3.Expr.equal

type sort = Z3.Sort.sort

let domain_local f =
  let key = Domain.DLS.new_key f in
  fun () -> Domain.DLS.get key

let ctx = domain_local (fun () -> Z3.mk_context [ ("model", "true") ])
let ( = ) lhs rhs = Z3.Boolean.mk_eq (ctx ()) lhs rhs
let if_ cond ~then_ ~else_ = Z3.Boolean.mk_ite (ctx ()) cond then_ else_
let wrap ctx f = fun x1 x2 -> f (ctx ()) x1 x2
let wrap1 ctx f = fun x -> f (ctx ()) x
let get_sort = Z3.Expr.get_sort
let of_int sort i = Z3.Expr.mk_numeral_int (ctx ()) i sort
let of_int' sort i = Z3.Expr.mk_numeral_string (ctx ()) i sort

module Bitvec = struct
  open Z3.BitVector

  let mk_sort = wrap1 ctx mk_sort
  let sort_size = get_size
  let dword = domain_local @@ fun () -> mk_sort 32
  let qword = domain_local @@ fun () -> mk_sort 64
  let xmmword = domain_local @@ fun () -> mk_sort 128
  let word = domain_local @@ fun () -> mk_sort 16
  let byte = domain_local @@ fun () -> mk_sort 8
  let bit = domain_local @@ fun () -> mk_sort 1
  let ( %~ ) x = mk_not (ctx ()) x
  let ( land ) = wrap ctx mk_and
  let ( lor ) = wrap ctx mk_or
  let ( lxor ) = wrap ctx mk_xor
  let ( + ) = wrap ctx mk_add
  let ( - ) = wrap ctx mk_sub
  let ( * ) = wrap ctx mk_mul
  let ( @/ ) = wrap ctx mk_udiv
  let ( $/ ) = wrap ctx mk_sdiv
  let ( @% ) = wrap ctx mk_urem
  let ( $% ) = wrap ctx mk_srem
  let ( @< ) = wrap ctx mk_ult
  let ( $< ) = wrap ctx mk_slt
  let ( @> ) = wrap ctx mk_ugt
  let ( $> ) = wrap ctx mk_sgt
  let ( @>= ) = wrap ctx mk_uge
  let ( $>= ) = wrap ctx mk_sge
  let ( @<= ) = wrap ctx mk_ule
  let ( $<= ) = wrap ctx mk_sle
  let sign_ext = wrap ctx mk_sign_ext
  let zero_ext = wrap ctx mk_zero_ext

  let sign_ext' sort bits =
    mk_sign_ext (ctx ())
      Stdlib.(sort_size sort - (Z3.Expr.get_sort bits |> sort_size))
      bits

  let zero_ext' sort bits =
    mk_zero_ext (ctx ())
      Stdlib.(sort_size sort - (Z3.Expr.get_sort bits |> sort_size))
      bits

  let ( lsl ) = wrap ctx mk_shl
  let ( lsr ) = wrap ctx mk_lshr
  let ( asr ) = wrap ctx mk_ashr
  let rotate_left = wrap ctx mk_ext_rotate_left
  let rotate_right = wrap ctx mk_ext_rotate_right

  let add_wont_overflow lhs rhs ~signed =
    mk_add_no_overflow (ctx ()) lhs rhs signed

  let add_wont_underflow lhs rhs = mk_add_no_underflow (ctx ()) lhs rhs
  let sub_wont_overflow lhs rhs = mk_sub_no_overflow (ctx ()) lhs rhs

  let sub_wont_underflow lhs rhs ~signed =
    mk_sub_no_underflow (ctx ()) lhs rhs signed

  let extract start len bits = mk_extract (ctx ()) start len bits
  let concat = wrap ctx mk_concat

  let replace end_ start into value =
    let into_bits = get_size (Z3.Expr.get_sort into) in
    let value_bits = get_size (Z3.Expr.get_sort value) in
    assert (Stdlib.(end_ - start + 1 = value_bits));
    assert (end_ > start);
    match Stdlib.(end_ + 1 = into_bits, start = 0) with
    | true, true -> value
    | true, false -> concat value (extract Stdlib.(start - 1) 0 into)
    | false, true ->
        concat Stdlib.(extract (into_bits - 1) (end_ + 1) into) value
    | false, false ->
        Stdlib.(
          concat (extract (into_bits - 1) (end_ + 1) into)
          @@ concat value (extract (start - 1) 0 into))

  let inc bv = bv + of_int (Z3.Expr.get_sort bv) 1

  let of_int sort i =
    let size = sort_size sort in
    (* doesn't work for min_int right? *)
    assert (Core.(Int.ceil_log2 (if i < 0 then -i else i) < size));
    of_int sort i

  let parity8 bv =
    assert (Stdlib.(get_sort bv |> get_size = 8));
    let byte = byte () in
    let a1 = bv lsr of_int byte 4 in
    let a0 = bv lxor a1 in
    let a1 = a0 lsr of_int byte 2 in
    let a0 = a0 lxor a1 in
    let a1 = a0 lsr of_int byte 1 in
    let a0 = a0 lxor a1 in
    extract 0 0 a0 = of_int (bit ()) 1

  let lanewise f ~lane_size bv =
    let bits = get_sort bv |> sort_size in
    assert (Core.(bits mod lane_size = 0));
    let rec go bv i acc lane_size =
      if Core.(i < 0) then acc
      else
        go bv
          Core.(i - lane_size)
          (concat acc @@ f @@ extract i Core.(i - (lane_size - 1)) bv)
          lane_size
    in
    go bv
      Core.(bits - lane_size - 1)
      Core.(f @@ extract (bits - 1) (bits - (lane_size - 1)) bv)
      lane_size

  let lanewise2 f ~lane_size lhs rhs =
    let bits = get_sort lhs |> sort_size in
    assert (Core.(bits mod lane_size = 0 && get_sort rhs |> sort_size = bits));
    let rec go lhs rhs i acc lane_size =
      if Core.(i < 0) then acc
      else
        go lhs rhs
          Core.(i - lane_size)
          (concat acc
          @@ f
               (extract i Core.(i - (lane_size - 1)) lhs)
               (extract i Core.(i - (lane_size - 1)) rhs))
          lane_size
    in
    go lhs rhs
      Core.(bits - lane_size - 1)
      Core.(
        f
          (extract bits Core.(bits - (lane_size - 1)) lhs)
          (extract bits Core.(bits - (lane_size - 1)) rhs))
      lane_size

  let make_bitmask bv = lanewise (extract 7 7) bv ~lane_size:8

  let dot_product32 lhs rhs =
    let prod lhs rhs i =
      sign_ext 16 (extract i Core.(i - 15) lhs) * extract i Core.(i - 15) rhs
    in
    lanewise2
      (fun lhs rhs -> prod lhs rhs 15 + prod lhs rhs 15)
      lhs rhs ~lane_size:32

  let popcount32 bv =
    assert (Stdlib.(get_sort bv |> get_size = 32));
    let dword = dword () in
    let two_bits =
      bv - ((bv lsl of_int dword 1) land of_int dword 0x55555555)
    in
    let four_bits_mask = of_int dword 0x33333333 in
    let four_bits =
      (two_bits land four_bits_mask)
      + ((two_bits lsr of_int dword 2) land four_bits_mask)
    in
    let eight_bits =
      (four_bits + (four_bits lsr of_int dword 4)) land of_int dword 0x0F0F0F0F
    in
    let sixteen_bits = eight_bits + (eight_bits lsr of_int dword 8) in
    let thirtytwo_bits = sixteen_bits + (sixteen_bits lsr of_int dword 16) in
    thirtytwo_bits land of_int dword 0x3F

  let popcount64 bv =
    assert (Stdlib.(get_sort bv |> get_size = 64));
    let top, bot =
      (popcount32 (extract 63 32 bv), popcount32 (extract 31 0 bv))
    in
    zero_ext 32 top + zero_ext 32 bot

  let leading_zeros32 bv =
    assert (Stdlib.(get_sort bv |> get_size = 32));
    let dword = dword () in
    let bv = bv lor (bv lsr of_int dword 1) in
    let bv = bv lor (bv lsr of_int dword 2) in
    let bv = bv lor (bv lsr of_int dword 4) in
    let bv = bv lor (bv lsr of_int dword 8) in
    let bv = bv lor (bv lsr of_int dword 16) lxor of_int dword (-1) in
    let bv = bv - ((bv lsr of_int dword 1) land of_int dword 0x55555555) in
    let bv =
      (bv land of_int dword 0x33333333)
      + ((bv lsr of_int dword 2) land of_int dword 0x33333333)
    in
    let bv = ((bv lsr of_int dword 4) + bv) land of_int dword 0x0F0F0F0F in
    (bv * of_int dword 0x01010101) lsr of_int dword 24

  let leading_zeros64 bv =
    assert (Stdlib.(get_sort bv |> get_size = 64));
    let high_leading_zeros =
      extract 63 32 bv |> leading_zeros32 |> zero_ext 32
    in
    if_
      (high_leading_zeros = of_int (qword ()) 32)
      ~then_:
        (of_int (qword ()) 32 + extract 31 0 bv
        |> leading_zeros32 |> zero_ext 32)
      ~else_:high_leading_zeros

  let repeat i bv =
    assert (i > 0);
    let rec go i bv acc =
      if Core.(i = 1) then acc else go Core.(i - 1) bv (concat acc bv)
    in
    go i bv bv
end

module Array = struct
  type t = expr

  open Z3.Z3Array

  let make_sort domain codomain = mk_sort (ctx ()) domain codomain

  let const : expr -> t =
   fun expr -> mk_const_array (ctx ()) (Bitvec.dword ()) expr

  let uninterpreted : string -> sort -> sort -> t =
   fun name domain codomain ->
    Z3.Expr.mk_fresh_const (ctx ()) name @@ make_sort domain codomain

  let get : t -> expr -> expr = wrap ctx mk_select
  let set : t -> expr -> expr -> t = fun f ix x -> mk_store (ctx ()) f ix x

  let lambda sort f =
    let symb = Z3.Expr.mk_fresh_const (ctx ()) "lambda" sort in
    Z3.Quantifier.mk_lambda_const (ctx ()) [ symb ] (f symb)
    |> Z3.Quantifier.expr_of_quantifier

  let map f arr = lambda (get_sort arr |> get_domain) (fun i -> f @@ get arr i)
end

module Smt = struct
  open Z3.Arithmetic

  let ( + ) lhs rhs = mk_add (ctx ()) [ lhs; rhs ]
  let ( - ) lhs rhs = mk_sub (ctx ()) [ lhs; rhs ]
  let ( = ) = ( = )
end

module FP = struct
  open Z3.FloatingPoint

  type rounding_mode = expr
  type nonrec expr = expr

  let double = domain_local @@ fun () -> mk_sort_double (ctx ())
  let single = domain_local @@ fun () -> mk_sort_single (ctx ())

  let rounding_mode =
    domain_local (fun () -> RoundingMode.mk_round_nearest_ties_to_even (ctx ()))

  let truncate_mode =
    domain_local (fun () -> RoundingMode.mk_round_toward_zero (ctx ()))

  let of_bv sort ~signed bv =
    if signed then mk_to_fp_signed (ctx ()) (rounding_mode ()) bv sort
    else mk_to_fp_unsigned (ctx ()) (rounding_mode ()) bv sort

  let reinterpret_to_bv = wrap1 ctx mk_to_ieee_bv
  let truncate_to_bv sort fp = mk_to_sbv (ctx ()) (truncate_mode ()) fp sort
  let reinterpret_of_bv sort bv = mk_to_fp_bv (ctx ()) bv sort

  let of_bv sort ~signed bv =
    if signed then mk_to_fp_signed (ctx ()) (rounding_mode ()) bv sort
    else mk_to_fp_unsigned (ctx ()) (rounding_mode ()) bv sort

  let ( = ) = ( = )
  let ( < ) = wrap ctx mk_lt
  let ( > ) = wrap ctx mk_gt
  let ( <= ) = wrap ctx mk_leq
  let ( >= ) = wrap ctx mk_geq
  let ( + ) lhs rhs = mk_add (ctx ()) (rounding_mode ()) lhs rhs
  let ( - ) lhs rhs = mk_add (ctx ()) (rounding_mode ()) lhs rhs
  let ( * ) lhs rhs = mk_mul (ctx ()) (rounding_mode ()) lhs rhs
  let ( / ) lhs rhs = mk_div (ctx ()) (rounding_mode ()) lhs rhs
  let neg = wrap1 ctx mk_neg
  let abs = wrap1 ctx mk_abs
  let round mode fp = mk_round_to_integral (ctx ()) mode fp
  let sqrt fp = mk_sqrt (ctx ()) (rounding_mode ()) fp
end

let rec store_address : Array.t -> expr -> expr -> Array.t =
  let rec go mem size addr data =
    if size < 0 then mem
    else
      let byte_addr = Bitvec.(addr + of_int (dword ()) Int.(size - 1)) in
      let mem =
        Array.set mem byte_addr
        @@ Bitvec.extract ((size * 8) - 1) ((size * 8) - 8) data
      in
      go mem size addr data
  in
  fun mem addr data ->
    let size = get_sort data |> Bitvec.sort_size in
    assert (Core.(size mod 8 = 0));
    go mem (size / 8) addr data

let rec load_address : Array.t -> int -> expr -> expr =
 fun mem size addr ->
  if Core.(size = 1) then Array.get mem addr
  else
    Bitvec.concat (Array.get mem addr)
      Core.(load_address mem (size - 1) (Bitvec.inc addr))

let ( .%[]<- ) = store_address
let and_ = wrap1 ctx Z3.Boolean.mk_and
let not_ = wrap1 ctx Z3.Boolean.mk_not
let bool = domain_local @@ fun () -> Z3.Boolean.mk_sort (ctx ())
let get_sort = Z3.Expr.get_sort
let fresh = wrap ctx Z3.Expr.mk_fresh_const

let for_all sort f =
  let symb = Z3.Expr.mk_fresh_const (ctx ()) "forall" sort in
  Z3.Quantifier.mk_forall_const (ctx ()) [ symb ] (f symb) None [] [] None None
  |> Z3.Quantifier.expr_of_quantifier
