open Core
open Types

module Ref : sig
  type t = Ref of int [@@unboxed] [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val invalid : t
end

type ref = Ref.t

val ref : int -> ref

type uni_op =
  (* Int-valued *)
  | EqualsZero
  | LongEqualsZero
  | SignExtendLow8
  | SignExtendHigh8
  | SignExtend16
  | ZeroExtendLow8
  | ZeroExtendHigh8
  | ZeroExtend16
  | FloatToInt32
  | LongToInt32
  | CountOnes
  | VecInt8SignBitmask
  (* Float-valued *)
  | FloatNeg
  | FloatAbs
  | FloatRound
  | FloatTrunc
  | FloatSqrt
  | FloatSine
  | FloatCosine
  | FloatTangent
  | Int32ToFloatUnsigned
  | Int32ToFloatSigned
  | Int64ToFloatUnsigned
  | Int64ToFloatSigned
  (* Long-valued *)
  | FloatToLong
  | Int32ToLongUnsigned
  | Int32ToLongSigned
  (* Vec-valued *)
  | VecConvertLow32BitsToFloatsSigned
[@@deriving sexp]

type bi_op =
  (* Int-valued *)
  | Add
  | Subtract
  | Multiply
  | Equal
  | NotEqual
  | And
  | Or
  | Xor
  | ShiftLeft
  | RotateLeft
  | LongEq
  | LongNotEq
  | FloatEq
  | FloatNotEq
  | FloatGreaterThan
  | FloatLessThan
  | FloatGreaterThanEqual
  | FloatLessThanEqual
  | MergeTruncLow8
  | MergeTruncHigh8
  | MergeTrunc16
  (* Float-valued *)
  | FloatAdd
  | FloatSub
  | FloatMult
  | FloatDiv
  | FloatAtan2
  (* Long-valued *)
  | LongShiftLeft
  | LongAdd
  | LongSub
  | LongMultiply
  | LongRotateLeft
  | LongAnd
  | LongOr
  (* Vec-valued *)
  | VecAnd
  | VecOr
  | VecXor
  | VecMulAdd16Bit
[@@deriving sexp]

type vec_lane_bi_op = VecSub | VecShiftLeft | VecLaneEqual | VecAdd | VecMul
[@@deriving sexp]

type signed_bi_op =
  (* Int-valued *)
  | Divide
  | Remainder
  | ShiftRight
  | RotateRight
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | LongGreaterThan
  | LongLessThan
  | LongGreaterThanEqual
  | LongLessThanEqual
  (* Long valued *)
  | LongDivide
  | LongRemainder
  | LongShiftRight
  | LongRotateRight
  (* Vec valued *)
  | VecNarrow16Bit
  | VecNarrow32Bit
[@@deriving sexp]

type signed_vec_lane_bi_op =
  | VecShiftRight
  | VecMax
  | VecMin
  | VecLessThan
  | VecAddSaturating
  | VecSubSaturating
[@@deriving sexp]

type load_op =
  (* Int-valued *)
  | Load32
  (* Float-valued *)
  | FloatLoad32
  | FloatLoad64
  (* Long-valued *)
  | LongLoad64
  (* Vec-valued *)
  | VecLoad32ZeroExtend
  | VecLoad64ZeroExtend
  | VecLoad128
[@@deriving sexp]

type signed_load_op = Load8 | Load16 [@@deriving sexp]

type store_op =
  | Store32
  | Store16
  | Store8
  | FloatStore32
  | FloatStore64
  | LongStore64
  | VecStore128
[@@deriving sexp]

type t =
  | Const of Variable.t * int
  | FloatConst of Variable.t * float
  | LongConst of Variable.t * Int64.t
  | VecConst of { var : Variable.t; lower_bits : Int64.t; upper_bits : Int64.t }
  | DupVar of { var : Variable.t; src : Ref.t; typ : local_type }
  | UniOp of { var : Variable.t; op : uni_op; operand : Ref.t }
  | BiOp of { var : Variable.t; op : bi_op; lhs : Ref.t; rhs : Ref.t }
  | VecLaneBiOp of {
      var : Variable.t;
      op : vec_lane_bi_op;
      shape : vec_lane_shape;
      lhs : Ref.t;
      rhs : Ref.t;
    }
  | SignedBiOp of {
      var : Variable.t;
      op : signed_bi_op;
      signed : bool;
      lhs : Ref.t;
      rhs : Ref.t;
    }
  | SignedVecLaneBiOp of {
      var : Variable.t;
      op : signed_vec_lane_bi_op;
      signed : bool;
      shape : vec_lane_shape;
      lhs : Ref.t;
      rhs : Ref.t;
    }
  | VecExtractLaneOp of {
      var : Variable.t;
      src : Ref.t;
      shape : vec_lane_shape;
      lane : int;
    }
  | VecReplaceLaneOp of {
      var : Variable.t;
      dest : Ref.t;
      lane_value : Ref.t;
      shape : vec_lane_shape;
      lane : int;
    }
  | VecShuffleOp of {
      var : Variable.t;
      arg1 : Ref.t;
      arg2 : Ref.t;
      control_lower_bits : Int64.t;
      control_upper_bits : Int64.t;
    }
  | LoadOp of { var : Variable.t; op : load_op; addr : Ref.t; offset : int }
  | SignedLoadOp of {
      var : Variable.t;
      op : signed_load_op;
      addr : Ref.t;
      signed : bool;
      offset : int;
    }
  | VecLoadLaneOp of {
      var : Variable.t;
      dest_vec : Ref.t;
      addr : Ref.t;
      shape : vec_lane_shape;
      lane : int;
      offset : int;
    }
  | CallOp of { func : ident; args : Ref.t list }
  | CallIndirectOp of { table_index : Ref.t; args : Ref.t list }
  | ReturnedOp of { var : Variable.t; typ : local_type }
  | GetGlobalOp of {
      var : Variable.t;
      global_name : ident;
      global_type : local_type;
    }
  | OutsideContext of { var : Variable.t; typ : local_type }
  | Landmine of { var : Variable.t; typ : local_type }
  | StoreOp of { op : store_op; addr : Ref.t; value : Ref.t; offset : int }
  | VecStoreLaneOp of {
      value : Ref.t;
      addr : Ref.t;
      shape : vec_lane_shape;
      lane : int;
      offset : int;
    }
  | SetGlobalOp of {
      global_name : ident;
      value : Ref.t;
      global_type : local_type;
    }
  | AssertOp of { condition : Ref.t }
  | Memset of { count : Ref.t; value : Ref.t; dest : Ref.t }
  | Memcopy of { count : Ref.t; src : Ref.t; dest : Ref.t }
  | Unreachable
[@@deriving sexp]

val value_type : t -> local_type
val replace_var : t -> Variable.t -> t
val replace_instr_ref : t -> from:Ref.t -> into:Ref.t -> t
val is_pure : t -> bool
val is_assignment : t -> bool
val assignment_var : t -> Variable.t option
