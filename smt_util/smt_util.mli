type 'a domain_local = unit -> 'a
type expr = Z3.Expr.expr [@@deriving equal]
type sort = Z3.Sort.sort

val ctx : Z3.context domain_local
val if_ : expr -> then_:expr -> else_:expr -> expr

module Array : sig
  type t = expr

  val const : expr -> t
  val uninterpreted : string -> sort -> sort -> t
  val get : t -> expr -> expr
  val set : t -> expr -> expr -> t
  val lambda : sort -> (expr -> expr) -> expr
  val map : (expr -> expr) -> expr -> expr
end

module Bitvec : sig
  val mk_sort : int -> sort
  val sort_size : sort -> int
  val dword : sort domain_local
  val qword : sort domain_local
  val xmmword : sort domain_local
  val word : sort domain_local
  val byte : sort domain_local
  val bit : sort domain_local
  val ( land ) : expr -> expr -> expr
  val ( lor ) : expr -> expr -> expr
  val ( lxor ) : expr -> expr -> expr
  val ( %~ ) : expr -> expr
  val ( + ) : expr -> expr -> expr
  val ( - ) : expr -> expr -> expr
  val ( * ) : expr -> expr -> expr
  val ( @/ ) : expr -> expr -> expr
  val ( $/ ) : expr -> expr -> expr
  val ( @% ) : expr -> expr -> expr
  val ( $% ) : expr -> expr -> expr
  val ( @< ) : expr -> expr -> expr
  val ( $< ) : expr -> expr -> expr
  val ( @> ) : expr -> expr -> expr
  val ( $> ) : expr -> expr -> expr
  val ( @>= ) : expr -> expr -> expr
  val ( $>= ) : expr -> expr -> expr
  val ( @<= ) : expr -> expr -> expr
  val ( $<= ) : expr -> expr -> expr
  val sign_ext : int -> expr -> expr
  val zero_ext : int -> expr -> expr
  val sign_ext' : sort -> expr -> expr
  val zero_ext' : sort -> expr -> expr
  val ( lsl ) : expr -> expr -> expr
  val ( lsr ) : expr -> expr -> expr
  val ( asr ) : expr -> expr -> expr
  val rotate_left : expr -> expr -> expr
  val rotate_right : expr -> expr -> expr
  val add_wont_overflow : expr -> expr -> signed:bool -> expr
  val add_wont_underflow : expr -> expr -> expr
  val sub_wont_underflow : expr -> expr -> signed:bool -> expr
  val sub_wont_overflow : expr -> expr -> expr
  val extract : int -> int -> expr -> expr
  val replace : int -> int -> expr -> expr -> expr
  val concat : expr -> expr -> expr
  val inc : expr -> expr
  val of_int : sort -> int -> expr
  val make_bitmask : expr -> expr
  val dot_product32 : expr -> expr -> expr
  val parity8 : expr -> expr
  val popcount32 : expr -> expr
  val popcount64 : expr -> expr
  val leading_zeros32 : expr -> expr
  val leading_zeros64 : expr -> expr
  val lanewise : (expr -> expr) -> lane_size:int -> expr -> expr

  val lanewise2 :
    (expr -> expr -> expr) -> lane_size:int -> expr -> expr -> expr

  val repeat : int -> expr -> expr
end

module Smt : sig
  val ( + ) : expr -> expr -> expr
  val ( - ) : expr -> expr -> expr
  val ( = ) : expr -> expr -> expr
end

module FP : sig
  type rounding_mode

  val single : sort domain_local
  val double : sort domain_local
  val rounding_mode : rounding_mode domain_local
  val truncate_mode : rounding_mode domain_local
  val reinterpret_of_bv : sort -> expr -> expr
  val of_bv : sort -> signed:bool -> expr -> expr
  val reinterpret_to_bv : expr -> expr
  val ( = ) : expr -> expr -> expr
  val ( > ) : expr -> expr -> expr
  val ( < ) : expr -> expr -> expr
  val ( >= ) : expr -> expr -> expr
  val ( <= ) : expr -> expr -> expr
  val truncate_to_bv : int -> expr -> expr
  val ( + ) : expr -> expr -> expr
  val ( - ) : expr -> expr -> expr
  val ( * ) : expr -> expr -> expr
  val ( / ) : expr -> expr -> expr
  val neg : expr -> expr
  val abs : expr -> expr
  val round : rounding_mode -> expr -> expr
  val sqrt : expr -> expr
end

val load_address : Array.t -> int-> expr -> expr
val store_address : Array.t -> expr -> expr -> Array.t
val ( .%[]<- ) : Array.t -> expr -> expr -> Array.t
val and_ : expr list -> expr
val not_ : expr -> expr
val bool : sort domain_local
val get_sort : expr -> sort
val fresh : string -> sort -> expr
val of_int : sort -> int -> expr
val of_int' : sort -> string -> expr
val for_all : sort -> (expr -> expr) -> expr
