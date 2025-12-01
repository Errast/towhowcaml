type 'a domain_local = unit -> 'a
type expr = Z3.Expr.expr [@@deriving equal]
type sort = Z3.Sort.sort

val ctx : Z3.context domain_local
val if_ : expr -> then_:expr -> else_:expr -> expr

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
  val ( !< ) : expr -> expr -> expr
  val ( #< ) : expr -> expr -> expr
  val ( !> ) : expr -> expr -> expr
  val ( #> ) : expr -> expr -> expr
  val ( !>= ) : expr -> expr -> expr
  val ( #>= ) : expr -> expr -> expr
  val ( !<= ) : expr -> expr -> expr
  val ( #<= ) : expr -> expr -> expr
  val sign_ext : int -> expr -> expr
  val zero_ext : int -> expr -> expr
  val sign_ext' : sort -> expr -> expr
  val zero_ext' : sort -> expr -> expr
  val ( lsl ) : expr -> expr -> expr
  val ( lsr ) : expr -> expr -> expr
  val ( asr ) : expr -> expr -> expr
  val wont_overflow : expr -> expr -> bool -> expr
  val wont_carry : expr -> expr -> bool -> expr
  val const : int -> expr
  val extract : int -> int -> expr -> expr
  val replace : int -> int -> expr -> expr -> expr
  val concat : expr -> expr -> expr
  val inc : expr -> expr
  val of_int : sort -> int -> expr
end

module Array : sig
  type t = expr -> expr

  val const : expr -> t
  val uninterpreted : string -> sort -> sort -> t
  val get : t -> expr -> expr
  val set : t -> expr -> expr -> t
end

module Smt : sig
  val ( + ) : expr -> expr -> expr
  val ( - ) : expr -> expr -> expr
  val ( = ) : expr -> expr -> expr
end

module FP : sig
  val double : sort domain_local
  val of_bv : expr -> expr
  val ( + ) : expr -> expr -> expr
  val ( * ) : expr -> expr -> expr
end

val ( .%[] ) : Array.t -> expr -> expr
val ( .%[]<- ) : Array.t -> expr -> expr -> Array.t
val and_ : expr list -> expr
val bool : sort domain_local
val get_sort : expr -> sort
val fresh : string -> sort -> expr
val of_int : sort -> int -> expr
