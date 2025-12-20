open Core
open Radatnet

type value [@@deriving sexp, equal]

module Phys_reg : sig
  type floats =
    [ `st__1
    | `st__2
    | `st__3
    | `st__4
    | `st__5
    | `st__6
    | `st__7
    | X86reg.x87_float ]
  [@@deriving equal, compare, sexp, enumerate]

  type t = [ X86reg.reg_32bit | X86reg.flags | floats ]
  [@@deriving equal, compare, sexp, enumerate]

  include Comparator.S with type t := t

  val to_sort : [< t ] -> Smt_util.sort
  val symb_to_value : t -> Smt_util.expr -> value
  val to_ident : [< t ] -> string
  val fp_index_to_phys_reg  : int -> [> floats ]
  val float_to_fp_index : [< floats] -> int
end

module Reg_map : sig
  type t

  val empty : unit -> t
  val get : t -> Phys_reg.t -> Smt_util.expr
  val set : t -> key:Phys_reg.t -> data:Smt_util.expr -> t
  val set_lazy : t -> key:Phys_reg.t -> data:Smt_util.expr lazy_t -> t
  val set_undefined : t -> Phys_reg.t -> t

  val fold_symmetric_diff :
    t ->
    t ->
    init:'a ->
    f:
      ('a ->
      Phys_reg.t ->
      left:Smt_util.expr lazy_t ->
      right:Smt_util.expr lazy_t ->
      'a) ->
    'a

  val fold :
    t ->
    init:'a ->
    f:(key:Phys_reg.t -> data:Smt_util.expr lazy_t -> 'a -> 'a) ->
    'a
end

type ctx = {
  env : value Map.M(String).t;
  reg_map : Reg_map.t;
  memory : Smt_util.Array.t;
  fp_offset : int;
  min_fp_offset : int;
}

val empty_context : unit -> ctx
val eval_instruction : ctx -> opcode -> ctx
