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
  let ( !< ) = wrap ctx mk_ult
  let ( #< ) = wrap ctx mk_slt
  let ( !> ) = wrap ctx mk_ugt
  let ( #> ) = wrap ctx mk_sgt
  let ( !>= ) = wrap ctx mk_uge
  let ( #>= ) = wrap ctx mk_uge
  let ( !<= ) = wrap ctx mk_ule
  let ( #<= ) = wrap ctx mk_ule
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
  let wont_overflow = wrap ctx mk_add_no_overflow
  let wont_carry = wrap ctx mk_add_no_overflow
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

  let const i =
    let c = ctx () in
    mk_const c (Z3.Symbol.mk_int c i) i

  let inc bv = bv + of_int (Z3.Expr.get_sort bv) 1

  let of_int sort i =
    let size = sort_size sort in
    (* doesn't work for min_int right? *)
    assert (Core.(Int.ceil_log2 (if i < 0 then -i else i) < size));
    of_int sort i
end

module Array = struct
  type t = expr -> expr

  let const : expr -> t = Fun.const

  let uninterpreted : string -> sort -> sort -> t =
   fun name domain codomain ->
    let fn = Z3.FuncDecl.mk_fresh_func_decl (ctx ()) name [ domain ] codomain in
    fun ix -> Z3.FuncDecl.apply fn [ ix ]

  let get : t -> expr -> expr = ( @@ )

  let set : t -> expr -> expr -> t =
   fun f ix x i ->
    if_ (i = ix) ~then_:x
      ~else_:begin
        f i
      end
end

module Smt = struct
  open Z3.Arithmetic

  let ( + ) lhs rhs = mk_add (ctx ()) [ lhs; rhs ]
  let ( - ) lhs rhs = mk_sub (ctx ()) [ lhs; rhs ]
  let ( = ) = ( = )
end

module FP = struct
  open Z3.FloatingPoint

  let double = domain_local @@ fun () -> mk_sort (ctx ()) 11 53

  let rounding_mode =
    domain_local (fun () -> RoundingMode.mk_round_nearest_ties_to_even (ctx ()))
  let of_bv bv = mk_to_ieee_bv (ctx ()) bv
  let ( + ) = wrap ctx mk_add (rounding_mode ())
  let ( * ) = wrap ctx mk_mul (rounding_mode ())
  let ( = ) = ( = )
end

let ( .%[] ) = Array.get
let ( .%[]<- ) = Array.set
let and_ = wrap1 ctx Z3.Boolean.mk_and
let bool = domain_local @@ fun () -> Z3.Boolean.mk_sort (ctx ())
let get_sort = Z3.Expr.get_sort
let fresh = wrap ctx Z3.Expr.mk_fresh_const
