open! Core
open Types

val float_temp : ident
val long_temp : ident
val vec_temp : ident
val int_temp : ident
val is_temp : ident -> bool

type t [@@deriving sexp_of]

val create : local_type String.Map.t -> t

val deconstruct :
  t ->
  Instr_list.t
  * (ident, Local_info.t) Hashtbl.t
  * local_type Map.M(String).t
  * Set.M(Instr.Ref).t

val set_check_var_is_latest : t -> bool -> unit
val newest_var : t -> ident -> Instr.ref
val newest_var_opt : t -> ident -> Instr.ref option
val get_instr : t -> Instr.ref -> Instr.t
val const : ?varName:ident -> t -> int -> Instr.ref
val float_const : ?varName:ident -> t -> float -> Instr.ref
val long_const : ?varName:ident -> t -> int64 -> Instr.ref
val vec_const : ?varName:ident -> t -> l:int64 -> h:int64 -> Instr.ref
val dup_var : ?varName:ident -> t -> local_type -> Instr.ref -> Instr.ref

type uni_op_add = ?varName:ident -> t -> operand:Instr.ref -> Instr.ref

type uni_op_add_signed =
  ?varName:ident -> t -> operand:Instr.ref -> signed:bool -> Instr.ref

type bi_op_add =
  ?varName:ident -> t -> lhs:Instr.ref -> rhs:Instr.ref -> Instr.ref

type vec_lane_bi_op_add =
  ?varName:ident ->
  t ->
  lhs:Instr.ref ->
  rhs:Instr.ref ->
  shape:vec_lane_shape ->
  Instr.ref

type signed_bi_op_add =
  ?varName:ident ->
  t ->
  lhs:Instr.ref ->
  rhs:Instr.ref ->
  signed:bool ->
  Instr.ref

type signed_vec_lane_bi_op_add =
  ?varName:ident ->
  t ->
  lhs:Instr.ref ->
  rhs:Instr.ref ->
  signed:bool ->
  shape:vec_lane_shape ->
  Instr.ref

type load_op_add = ?varName:ident -> ?offset:int -> t -> Instr.ref -> Instr.ref

type sign_load_op_add =
  ?varName:ident -> ?offset:int -> t -> Instr.ref -> signed:bool -> Instr.ref

type store_op_add =
  ?offset:int -> t -> value:Instr.ref -> addr:Instr.ref -> unit

val equals_zero : uni_op_add
val long_equals_zero : uni_op_add
val zero_extend_low8 : uni_op_add
val zero_extend_high8 : uni_op_add
val zero_extend_16 : uni_op_add
val sign_extend_low8 : uni_op_add
val sign_extend_high8 : uni_op_add
val sign_extend_16 : uni_op_add
val float_to_int32 : uni_op_add
val long_to_int32 : uni_op_add
val count_ones : uni_op_add
val float_neg : uni_op_add
val float_abs : uni_op_add
val float_round : uni_op_add
val float_trunc : uni_op_add
val float_sqrt : uni_op_add
val float_sine : uni_op_add
val float_cosine : uni_op_add
val float_tangent : uni_op_add
val int32_to_float : uni_op_add_signed
val int32_to_long : uni_op_add_signed
val float_to_long : uni_op_add
val int64_to_float : uni_op_add_signed
val vec_convert_low_32bits_to_float_signed : uni_op_add
val add : bi_op_add
val sub : bi_op_add
val mul : bi_op_add
val equal : bi_op_add
val not_equal : bi_op_add
val int_and : bi_op_add
val int_or : bi_op_add
val xor : bi_op_add
val long_eq : bi_op_add
val shift_left : bi_op_add
val rotate_left : bi_op_add
val long_equal : bi_op_add
val long_not_equal : bi_op_add
val float_equal : bi_op_add
val aad_float_not_equal : bi_op_add
val float_greater_than : bi_op_add
val float_less_than : bi_op_add
val float_greater_than_equal : bi_op_add
val float_less_than_equal : bi_op_add
val merge_trunc_low8 : bi_op_add
val merge_trunc_high8 : bi_op_add
val merge_trunc_16 : bi_op_add
val float_add : bi_op_add
val float_sub : bi_op_add
val float_mul : bi_op_add
val float_div : bi_op_add
val float_atan2 : bi_op_add
val long_add : bi_op_add
val long_sub : bi_op_add
val long_mul : bi_op_add
val long_shift_left : bi_op_add
val long_rotate_left : bi_op_add
val long_and : bi_op_add
val long_or : bi_op_add
val vec_and : bi_op_add
val vec_or : bi_op_add
val vec_xor : bi_op_add
val vec_mul_add_16bit : bi_op_add
val vec_add : vec_lane_bi_op_add
val vec_sub : vec_lane_bi_op_add
val vec_mul : vec_lane_bi_op_add
val vec_equal : vec_lane_bi_op_add
val vec_shift_left : vec_lane_bi_op_add
val div : signed_bi_op_add
val remainder : signed_bi_op_add
val shift_right : signed_bi_op_add
val rotate_right : signed_bi_op_add
val less_than : signed_bi_op_add
val greater_than : signed_bi_op_add
val less_than_equal : signed_bi_op_add
val greater_than_equal : signed_bi_op_add
val long_less_than : signed_bi_op_add
val long_greater_than : signed_bi_op_add
val long_less_than_equal : signed_bi_op_add
val long_greater_than_equal : signed_bi_op_add
val long_div : signed_bi_op_add
val long_remainder : signed_bi_op_add
val long_shift_right : signed_bi_op_add
val long_rotate_right : signed_bi_op_add
val vec_narrow_16bit : signed_bi_op_add
val vec_narrow_32bit : signed_bi_op_add
val vec_shift_right : signed_vec_lane_bi_op_add
val vec_max : signed_vec_lane_bi_op_add
val vec_min : signed_vec_lane_bi_op_add
val vec_less_than : signed_vec_lane_bi_op_add
val vec_add_sat : signed_vec_lane_bi_op_add
val vec_sub_sat : signed_vec_lane_bi_op_add

val vec_extract :
  ?varName:ident ->
  t ->
  Instr.ref ->
  lane:int ->
  shape:vec_lane_shape ->
  Instr.ref

val vec_replace :
  ?varName:ident ->
  t ->
  dest:Instr.ref ->
  value:Instr.ref ->
  lane:int ->
  shape:vec_lane_shape ->
  Instr.ref

val vec_shuffle :
  ?varName:ident ->
  t ->
  vec1:Instr.ref ->
  vec2:Instr.ref ->
  ctrl_l:int64 ->
  ctrl_h:int64 ->
  Instr.ref

val load8 : sign_load_op_add
val load16 : sign_load_op_add
val load32 : load_op_add
val float_load32 : load_op_add
val float_load64 : load_op_add
val long_load64 : load_op_add
val vec_load128 : load_op_add
val vec_load32_zero_extend : load_op_add
val vec_load64_zero_extend : load_op_add

val vec_load :
  ?varName:ident ->
  ?offset:int ->
  t ->
  dest:Instr.ref ->
  addr:Instr.ref ->
  shape:vec_lane_shape ->
  lane:int ->
  Instr.ref

val call :
  ?varName:ident -> t -> ident -> Instr.ref list -> local_type -> Instr.ref

val call_indirect :
  ?varName:ident -> t -> Instr.ref -> Instr.ref list -> local_type -> Instr.ref

val get_global : ?varName:ident -> t -> ident -> local_type -> Instr.ref
val landmine : ?varName:ident -> t -> local_type -> Instr.ref
val store8 : store_op_add
val store16 : store_op_add
val store32 : store_op_add
val float_store32 : store_op_add
val float_store64 : store_op_add
val vec_store128 : store_op_add

val vec_store :
  ?offset:int ->
  t ->
  addr:Instr.ref ->
  vec:Instr.ref ->
  shape:vec_lane_shape ->
  lane:int ->
  unit

val set_global : t -> ident -> local_type -> Instr.ref -> unit
val mir_assert : t -> Instr.ref -> unit
val unreachable : t -> unit -> unit
