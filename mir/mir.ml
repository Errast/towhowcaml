open Core

type ident = string [@@deriving sexp]
type opt_ident = string option [@@deriving sexp]
type variable = { name: ident; index: int } [@@deriving sexp]
type local_type = Int | Float | Long | Vec [@@deriving sexp]
type vec_lane_shape = I8 | I16 | I32 | I64 | F32 | F64 [@@deriving sexp]
module Instr = struct
  module Ref = struct
    module T = struct
      type t = InstrRef of int [@@unboxed] [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end
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

  type vec_lane_bi_op =
    | VecSub
    | VecShiftLeft
    | VecLaneEqual
    | VecAdd
    | VecMul
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

  type signed_load_op =
    | Load8
    | Load16
    [@@deriving sexp]

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
    | Const of variable * int
    | FloatConst of variable * float
    | LongConst of variable * Int64.t
    | VecConst of { variable: variable; lower_bits: Int64.t; upper_bits: Int64.t }
    | DupVar of { var: variable; src: Ref.t; dest: Ref.t; typ: local_type }
    | UniOp of { var: variable; op: uni_op; operand: Ref.t }
    | BiOp of { var: variable; op: bi_op; lhs: Ref.t; rhs: Ref.t }
    | VecLaneBiOp of { var: variable; op: vec_lane_bi_op; shape: vec_lane_shape; lhs: Ref.t; rhs: Ref.t }
    | SignedBiOp of { var: variable; op: signed_bi_op; signed: bool; lhs: Ref.t; rhs: Ref.t }
    | SignedVecLaneBiOp of { var: variable; op: signed_vec_lane_bi_op; signed: bool; shape: vec_lane_shape; lhs: Ref.t; rhs: Ref.t }
    | VecExtractLaneOp of { var: variable; src: Ref.t; shape: vec_lane_shape; lane: int }
    | VecReplaceLaneOp of { var: variable; src: Ref.t; lane_value: Ref.t; shape: vec_lane_shape; lane: int }
    | VecShuffleOp of { var: variable; arg1: Ref.t; arg2: Ref.t; control_lower_bits: Int64.t; control_upper_bits: Int64.t }
    | LoadOp of { var: variable; op: load_op; addr: Ref.t; offset: int }
    | SignedLoadOp of { var: variable; op: signed_load_op; addr: Ref.t; signed: bool; offset: int }
    | VecLoadLaneOp of { var: variable; dest_vec: Ref.t; addr: Ref.t; shape: vec_lane_shape; lane: int; offset: int }
    | CallOp of { var: variable; func: ident; args: Ref.t list; return_type: local_type }
    | CallIndirectOp of { var: variable; table_index: Ref.t; args: Ref.t list; return_type: local_type }
    | GetGlobalOp of { var: variable; global_name: ident; global_type: local_type }
    | StoreOp of { op: store_op; addr: Ref.t; value: Ref.t; offset: int }
    | VecStoreLaneOp of { value: Ref.t; addr: Ref.t; shape: vec_lane_shape; lane: int; offset: int }
    | SetGlobalOp of { global_name: ident; global_type: local_type }
    | AssertOp of { condition: Ref.t }
    | OutsideContext
    | Landmine
    | Unreachable
    [@@deriving sexp]
end
type local_info = { subscript: int; index: Instr.Ref.t; typ: local_type }
type block_ref = BlockRef of int [@@unboxed] [@@deriving sexp]
type block = { id: block_ref;
               next_block: block_ref;
               conditional: block_ref ;
               instrs: (Instr.t, Perms.Export.immutable) Array.Permissioned.t;
               roots: (opt_ident, local_info) Map.Poly.t
               }

type local = { name: ident; typ: local_type }
type func_sig = { args: local list; return: local_type }
