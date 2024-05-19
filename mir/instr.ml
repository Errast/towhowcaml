open Core
open Types

module Ref = struct
  module T = struct
    type t = Ref of int [@@unboxed] [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let invalid = Ref (-1)
end

type ref = Ref.t

let ref i = Ref.Ref i

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
  | LoadOp of {
      var : Variable.t;
      op : load_op;
      addr : Ref.t;
      offset : int; [@default 0] [@sexp_drop_default.equal]
    }
  | SignedLoadOp of {
      var : Variable.t;
      op : signed_load_op;
      addr : Ref.t;
      signed : bool;
      offset : int; [@default 0] [@sexp_drop_default.equal]
    }
  | VecLoadLaneOp of {
      var : Variable.t;
      dest_vec : Ref.t;
      addr : Ref.t;
      shape : vec_lane_shape;
      lane : int;
      offset : int; [@default 0] [@sexp_drop_default.equal]
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
  | StoreOp of {
      op : store_op;
      addr : Ref.t;
      value : Ref.t;
      offset : int; [@default 0] [@sexp_drop_default.equal]
    }
  | VecStoreLaneOp of {
      value : Ref.t;
      addr : Ref.t;
      shape : vec_lane_shape;
      lane : int;
      offset : int; [@default 0] [@sexp_drop_default.equal]
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

let local_type_of_lane_shape = function
  | `I8 | `I16 | `I32 -> Int
  | `I64 -> Long
  | `F32 | `F64 -> Float

let value_type = function
  | Const _ -> Int
  | FloatConst _ -> Float
  | LongConst _ -> Long
  | VecConst _ -> Vec
  | DupVar { typ; _ } -> typ
  | UniOp { op; _ } -> (
      match op with
      | EqualsZero | LongEqualsZero | SignExtendLow8 | SignExtendHigh8
      | SignExtend16 | ZeroExtendLow8 | ZeroExtendHigh8 | ZeroExtend16
      | FloatToInt32 | LongToInt32 | CountOnes | VecInt8SignBitmask ->
          Int
      | FloatNeg | FloatAbs | FloatRound | FloatTrunc | FloatSqrt | FloatSine
      | FloatCosine | FloatTangent | Int32ToFloatSigned | Int32ToFloatUnsigned
      | Int64ToFloatSigned | Int64ToFloatUnsigned ->
          Float
      | FloatToLong | Int32ToLongSigned | Int32ToLongUnsigned -> Long
      | VecConvertLow32BitsToFloatsSigned -> Vec)
  | BiOp { op; _ } -> (
      match op with
      | Add | Subtract | Multiply | Equal | NotEqual | And | Or | Xor
      | ShiftLeft | RotateLeft | LongEq | LongNotEq | FloatEq | FloatNotEq
      | FloatGreaterThan | FloatLessThan | FloatGreaterThanEqual
      | FloatLessThanEqual | MergeTruncLow8 | MergeTruncHigh8 | MergeTrunc16 ->
          Int
      | FloatAdd | FloatSub | FloatMult | FloatDiv | FloatAtan2 -> Float
      | LongShiftLeft | LongAdd | LongSub | LongMultiply | LongRotateLeft
      | LongAnd | LongOr ->
          Long
      | VecAnd | VecOr | VecXor | VecMulAdd16Bit -> Vec)
  | VecLaneBiOp _ -> Vec
  | SignedBiOp { op; _ } -> (
      match op with
      | Divide | Remainder | ShiftRight | RotateRight | LessThan | LessThanEqual
      | GreaterThan | GreaterThanEqual | LongGreaterThan | LongGreaterThanEqual
      | LongLessThan | LongLessThanEqual ->
          Int
      | LongDivide | LongRemainder | LongShiftRight | LongRotateRight -> Long
      | VecNarrow16Bit | VecNarrow32Bit -> Vec)
  | SignedVecLaneBiOp _ -> Vec
  | VecExtractLaneOp { shape; _ } -> local_type_of_lane_shape shape
  | VecReplaceLaneOp _ -> Vec
  | VecShuffleOp _ -> Vec
  | LoadOp { op; _ } -> (
      match op with
      | Load32 -> Int
      | FloatLoad32 | FloatLoad64 -> Float
      | LongLoad64 -> Long
      | VecLoad32ZeroExtend | VecLoad64ZeroExtend | VecLoad128 -> Vec)
  | SignedLoadOp { op = Load8 | Load16; _ } -> Int
  | VecLoadLaneOp _ -> Vec
  | ReturnedOp { typ; _ } -> typ
  | GetGlobalOp { global_type; _ } -> global_type
  | OutsideContext { typ; _ } -> typ
  | Landmine { typ; _ } -> typ
  | _ -> raise @@ Invalid_argument "Instruction doesn't evaluate to a value"

let replace_var instr var =
  match instr with
  | Const (_, i) -> Const (var, i)
  | FloatConst (_, f) -> FloatConst (var, f)
  | LongConst (_, l) -> LongConst (var, l)
  | VecConst v -> VecConst { v with var }
  | DupVar v -> DupVar { v with var }
  | UniOp v -> UniOp { v with var }
  | BiOp v -> BiOp { v with var }
  | VecLaneBiOp v -> VecLaneBiOp { v with var }
  | SignedBiOp v -> SignedBiOp { v with var }
  | SignedVecLaneBiOp v -> SignedVecLaneBiOp { v with var }
  | VecExtractLaneOp v -> VecExtractLaneOp { v with var }
  | VecReplaceLaneOp v -> VecReplaceLaneOp { v with var }
  | VecShuffleOp v -> VecShuffleOp { v with var }
  | LoadOp v -> LoadOp { v with var }
  | SignedLoadOp v -> SignedLoadOp { v with var }
  | VecLoadLaneOp v -> VecLoadLaneOp { v with var }
  | ReturnedOp v -> ReturnedOp { v with var }
  | GetGlobalOp v -> GetGlobalOp { v with var }
  | OutsideContext v -> OutsideContext { v with var }
  | Landmine v -> Landmine { v with var }
  | CallOp _ | CallIndirectOp _ | StoreOp _ | VecStoreLaneOp _ | SetGlobalOp _
  | AssertOp _ | Memset _ | Memcopy _ | Unreachable ->
      instr

let replace_instr_ref t ~from ~into =
  let open Ref in
  let swap ref = if ref = from then into else ref in
  match t with
  | DupVar v when v.src <> from -> t
  | DupVar v -> DupVar { v with src = into }
  | UniOp v when v.operand <> from -> t
  | UniOp v -> UniOp { v with operand = into }
  | BiOp v when v.lhs <> from && v.rhs <> from -> t
  | BiOp v -> BiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | VecLaneBiOp v when v.lhs <> from && v.rhs <> from -> t
  | VecLaneBiOp v -> VecLaneBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | SignedBiOp v when v.lhs <> from && v.rhs <> from -> t
  | SignedBiOp v -> SignedBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | SignedVecLaneBiOp v when v.lhs <> from && v.rhs <> from -> t
  | SignedVecLaneBiOp v ->
      SignedVecLaneBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | VecExtractLaneOp v when v.src <> from -> t
  | VecExtractLaneOp v -> VecExtractLaneOp { v with src = into }
  | VecReplaceLaneOp v when v.dest <> from && v.lane_value <> from -> t
  | VecReplaceLaneOp v ->
      VecReplaceLaneOp
        { v with dest = swap v.dest; lane_value = swap v.lane_value }
  | VecShuffleOp v when v.arg1 <> from && v.arg2 <> from -> t
  | VecShuffleOp v ->
      VecShuffleOp { v with arg1 = swap v.arg1; arg2 = swap v.arg2 }
  | LoadOp v when v.addr <> from -> t
  | LoadOp v -> LoadOp { v with addr = into }
  | SignedLoadOp v when v.addr <> from -> t
  | SignedLoadOp v -> SignedLoadOp { v with addr = into }
  | VecLoadLaneOp v when v.addr <> from && v.dest_vec <> from -> t
  | VecLoadLaneOp v ->
      VecLoadLaneOp { v with addr = swap v.addr; dest_vec = swap v.dest_vec }
  | CallOp v when List.for_all v.args ~f:(fun r -> r <> from) -> t
  | CallOp v -> CallOp { v with args = List.map v.args ~f:swap }
  | CallIndirectOp v
    when v.table_index <> from && List.for_all v.args ~f:(fun r -> r <> from) ->
      t
  | CallIndirectOp v ->
      CallIndirectOp
        { table_index = swap v.table_index; args = List.map v.args ~f:swap }
  | StoreOp v when v.addr <> from && v.value <> from -> t
  | StoreOp v -> StoreOp { v with addr = swap v.addr; value = swap v.value }
  | VecStoreLaneOp v when v.addr <> from && v.value <> from -> t
  | VecStoreLaneOp v ->
      VecStoreLaneOp { v with addr = swap v.addr; value = swap v.value }
  | SetGlobalOp v when v.value <> from -> t
  | SetGlobalOp v -> SetGlobalOp { v with value = into }
  | AssertOp v when v.condition <> from -> t
  | AssertOp _ -> AssertOp { condition = into }
  | Memset v when v.dest <> from && v.value <> from && v.count <> from -> t
  | Memset v ->
      Memset { dest = swap v.dest; value = swap v.value; count = swap v.count }
  | Memcopy v when v.dest <> from && v.src <> from && v.count <> from -> t
  | Memcopy v ->
      Memcopy { count = swap v.count; src = swap v.src; dest = swap v.dest }
  | Landmine _ | Unreachable | OutsideContext _ | ReturnedOp _ | GetGlobalOp _
  | Const _ | FloatConst _ | LongConst _ | VecConst _ ->
      t

(* Using flambda terminology, this definition of pureness only accounts for effects, not coeffects *)
(* This is only used for dead code elimination, as we currently don't reorder instructions *)
let is_pure = function
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | DupVar _ | UniOp _
  | BiOp _ | VecLaneBiOp _ | SignedBiOp _ | SignedVecLaneBiOp _ | LoadOp _
  | SignedLoadOp _ | VecReplaceLaneOp _ | VecExtractLaneOp _ | VecShuffleOp _
  | VecLoadLaneOp _ | Landmine _ ->
      true
  | CallOp _ | CallIndirectOp _ | ReturnedOp _ | GetGlobalOp _
  | OutsideContext _ | StoreOp _ | VecStoreLaneOp _ | AssertOp _ | Memset _
  | Memcopy _ | Unreachable | SetGlobalOp _ ->
      false

let is_assignment = function
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | DupVar _ | UniOp _
  | BiOp _ | VecLaneBiOp _ | SignedBiOp _ | SignedVecLaneBiOp _ | LoadOp _
  | SignedLoadOp _ | VecReplaceLaneOp _ | VecExtractLaneOp _ | VecShuffleOp _
  | VecLoadLaneOp _ | ReturnedOp _ | GetGlobalOp _ | OutsideContext _
  | Landmine _ ->
      true
  | CallOp _ | CallIndirectOp _ | StoreOp _ | VecStoreLaneOp _ | AssertOp _
  | Unreachable | SetGlobalOp _ | Memset _ | Memcopy _ ->
      false

let assignment_var = function
  | Const (var, _)
  | FloatConst (var, _)
  | LongConst (var, _)
  | VecConst { var; _ }
  | DupVar { var; _ }
  | UniOp { var; _ }
  | BiOp { var; _ }
  | VecLaneBiOp { var; _ }
  | SignedBiOp { var; _ }
  | SignedVecLaneBiOp { var; _ }
  | VecExtractLaneOp { var; _ }
  | VecReplaceLaneOp { var; _ }
  | VecShuffleOp { var; _ }
  | LoadOp { var; _ }
  | SignedLoadOp { var; _ }
  | VecLoadLaneOp { var; _ }
  | ReturnedOp { var; _ }
  | GetGlobalOp { var; _ }
  | OutsideContext { var; _ }
  | Landmine { var; _ } ->
      Some var
  | CallOp _ | CallIndirectOp _ | StoreOp _ | VecStoreLaneOp _ | SetGlobalOp _
  | AssertOp _ | Unreachable | Memset _ | Memcopy _ ->
      None
