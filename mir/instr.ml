open Core
open Types

module Ref = struct
  module T = struct
    type t = Ref of int [@@unboxed] [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let invalid = Ref (-1)
  let to_int (Ref r) = r
end

type ref = Ref.t = Ref of int [@@unboxed]

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
  | CountLeadingZeros
  (* Float-valued *)
  | FloatNeg
  | FloatAbs
  | FloatRound
  | FloatTrunc
  | FloatSqrt
  | Int32ToFloatUnsigned
  | Int32ToFloatSigned
  | Int64ToFloatUnsigned
  | Int64ToFloatSigned
  | BitcastInt64ToFloat
  (* Long-valued *)
  | FloatToLong
  | Int32ToLongUnsigned
  | Int32ToLongSigned
  | BitcastFloatToLong
  | LongCountLeadingZeros
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
  | RotateRight
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
  (* Long-valued *)
  | LongShiftLeft
  | LongAdd
  | LongSub
  | LongMultiply
  | LongRotateLeft
  | LongRotateRight
  | LongAnd
  | LongOr
  | LongXor
  (* Vec-valued *)
  | VecAnd
  | VecOr
  | VecXor
  | VecMulAdd16Bit
[@@deriving sexp]

type vec_lane_bi_op = VecSub | VecEqual | VecAdd | VecMul | VecNotEqual
[@@deriving sexp]

type signed_bi_op =
  (* Int-valued *)
  | Divide
  | Remainder
  | ShiftRight
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
  (* Vec valued *)
  | VecNarrow16Bit
  | VecNarrow32Bit
[@@deriving sexp]

type signed_vec_lane_bi_op =
  | VecMax
  | VecMin
  | VecLessThan
  | VecLessThanEqual
  | VecGreaterThan
  | VecGreaterThanEqual
  | VecAddSaturating
  | VecSubSaturating
  | VecDiv
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
  | VecShiftLeftOp of {
      var : Variable.t;
      operand : Ref.t;
      count : Ref.t;
      shape : int_vec_lane_shape;
    }
  | VecShiftRightOp of {
      var : Variable.t;
      operand : Ref.t;
      count : Ref.t;
      shape : int_vec_lane_shape;
      signed : bool;
    }
  | VecSplatOp of { var : Variable.t; value : Ref.t; shape : vec_lane_shape }
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
  | VecExtend of {
      var : Variable.t;
      signed : bool;
      shape : [ `I8 | `I16 | `I32 ];
      half_used : [ `HighOrder | `LowOrder ];
      operand : Ref.t;
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
  | GetGlobalOp of { var : Variable.t; global : variable }
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
  | SetGlobalOp of { value : Ref.t; global : variable }
  | AssertOp of { condition : Ref.t }
  | Memset of { count : Ref.t; value : Ref.t; dest : Ref.t }
  | Memcopy of { count : Ref.t; src : Ref.t; dest : Ref.t }
  | Unreachable
  | Nop
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
      | FloatToInt32 | LongToInt32 | CountOnes | VecInt8SignBitmask
      | CountLeadingZeros ->
          Int
      | FloatNeg | FloatAbs | FloatRound | FloatTrunc | FloatSqrt
      | Int32ToFloatSigned | Int32ToFloatUnsigned | Int64ToFloatSigned
      | Int64ToFloatUnsigned | BitcastInt64ToFloat ->
          Float
      | BitcastFloatToLong | FloatToLong | Int32ToLongSigned
      | LongCountLeadingZeros | Int32ToLongUnsigned ->
          Long
      | VecConvertLow32BitsToFloatsSigned -> Vec)
  | VecExtend _ -> Vec
  | BiOp { op; _ } -> (
      match op with
      | Add | Subtract | Multiply | Equal | NotEqual | And | Or | Xor
      | ShiftLeft | RotateLeft | RotateRight | LongEq | LongNotEq | FloatEq
      | FloatNotEq | FloatGreaterThan | FloatLessThan | FloatGreaterThanEqual
      | FloatLessThanEqual | MergeTruncLow8 | MergeTruncHigh8 | MergeTrunc16 ->
          Int
      | FloatAdd | FloatSub | FloatMult | FloatDiv -> Float
      | LongShiftLeft | LongAdd | LongSub | LongMultiply | LongRotateLeft
      | LongRotateRight | LongAnd | LongOr | LongXor ->
          Long
      | VecAnd | VecOr | VecXor | VecMulAdd16Bit -> Vec)
  | VecLaneBiOp _ -> Vec
  | SignedBiOp { op; _ } -> (
      match op with
      | Divide | Remainder | ShiftRight | LessThan | LessThanEqual | GreaterThan
      | GreaterThanEqual | LongGreaterThan | LongGreaterThanEqual | LongLessThan
      | LongLessThanEqual ->
          Int
      | LongDivide | LongRemainder | LongShiftRight -> Long
      | VecNarrow16Bit | VecNarrow32Bit -> Vec)
  | SignedVecLaneBiOp _ -> Vec
  | VecSplatOp _ | VecShiftLeftOp _ | VecShiftRightOp _ -> Vec
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
  | GetGlobalOp { global; _ } -> global.typ
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
  | VecExtend v -> VecExtend { v with var }
  | BiOp v -> BiOp { v with var }
  | VecLaneBiOp v -> VecLaneBiOp { v with var }
  | SignedBiOp v -> SignedBiOp { v with var }
  | SignedVecLaneBiOp v -> SignedVecLaneBiOp { v with var }
  | VecShiftLeftOp v -> VecShiftLeftOp { v with var }
  | VecShiftRightOp v -> VecShiftRightOp { v with var }
  | VecSplatOp v -> VecSplatOp { v with var }
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
  | AssertOp _ | Memset _ | Memcopy _ | Unreachable | Nop ->
      instr

let replace_instr_ref t ~from ~into =
  let open Ref in
  let swap ref = if ref = from then into else ref in
  match t with
  | DupVar v when v.src <> from -> t
  | DupVar v -> DupVar { v with src = into }
  | UniOp v when v.operand <> from -> t
  | UniOp v -> UniOp { v with operand = into }
  | VecExtend v when v.operand <> from -> t
  | VecExtend v -> VecExtend { v with operand = into }
  | BiOp v when v.lhs <> from && v.rhs <> from -> t
  | BiOp v -> BiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | VecLaneBiOp v when v.lhs <> from && v.rhs <> from -> t
  | VecLaneBiOp v -> VecLaneBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | SignedBiOp v when v.lhs <> from && v.rhs <> from -> t
  | SignedBiOp v -> SignedBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | SignedVecLaneBiOp v when v.lhs <> from && v.rhs <> from -> t
  | SignedVecLaneBiOp v ->
      SignedVecLaneBiOp { v with lhs = swap v.lhs; rhs = swap v.rhs }
  | VecShiftLeftOp v when v.operand <> from && v.count <> from -> t
  | VecShiftLeftOp v ->
      VecShiftLeftOp { v with operand = swap v.operand; count = swap v.count }
  | VecShiftRightOp v when v.operand <> from && v.count <> from -> t
  | VecShiftRightOp v ->
      VecShiftRightOp { v with operand = swap v.operand; count = swap v.count }
  | VecSplatOp v when v.value <> from -> t
  | VecSplatOp v -> VecSplatOp { v with value = swap v.value }
  | VecExtractLaneOp v when v.src <> from -> t
  | VecExtractLaneOp v -> VecExtractLaneOp { v with src = swap v.src }
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
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | Nop ->
      t

let fold f s = function
  | Landmine _ | Unreachable | OutsideContext _ | ReturnedOp _ | GetGlobalOp _
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | Nop ->
      s
  | DupVar { src = v1; _ }
  | UniOp { operand = v1; _ }
  | VecExtend { operand = v1; _ }
  | VecSplatOp { value = v1; _ }
  | VecExtractLaneOp { src = v1; _ }
  | LoadOp { addr = v1; _ }
  | SignedLoadOp { addr = v1; _ }
  | SetGlobalOp { value = v1; _ }
  | AssertOp { condition = v1 } ->
      f s v1
  | BiOp { lhs = v1; rhs = v2; _ }
  | VecLaneBiOp { lhs = v1; rhs = v2; _ }
  | SignedBiOp { lhs = v1; rhs = v2; _ }
  | SignedVecLaneBiOp { lhs = v1; rhs = v2; _ }
  | VecShiftLeftOp { operand = v1; count = v2; _ }
  | VecShiftRightOp { operand = v1; count = v2; _ }
  | VecReplaceLaneOp { dest = v1; lane_value = v2; _ }
  | VecShuffleOp { arg1 = v1; arg2 = v2; _ }
  | VecLoadLaneOp { dest_vec = v1; addr = v2; _ }
  | StoreOp { addr = v1; value = v2; _ }
  | VecStoreLaneOp { addr = v1; value = v2; _ } ->
      f (f s v1) v2
  | Memset { dest = v1; value = v2; count = v3 }
  | Memcopy { dest = v1; src = v2; count = v3 } ->
      f (f (f s v1) v2) v3
  | CallOp v -> List.fold ~f ~init:s v.args
  | CallIndirectOp v ->
      let s = f s v.table_index in
      List.fold ~f ~init:s v.args

let fold_right f s = function
  | Landmine _ | Unreachable | OutsideContext _ | ReturnedOp _ | GetGlobalOp _
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | Nop ->
      s
  | DupVar { src = v1; _ }
  | UniOp { operand = v1; _ }
  | VecExtend { operand = v1; _ }
  | VecSplatOp { value = v1; _ }
  | VecExtractLaneOp { src = v1; _ }
  | LoadOp { addr = v1; _ }
  | SignedLoadOp { addr = v1; _ }
  | SetGlobalOp { value = v1; _ }
  | AssertOp { condition = v1 } ->
      f v1 s
  | BiOp { lhs = v1; rhs = v2; _ }
  | VecLaneBiOp { lhs = v1; rhs = v2; _ }
  | SignedBiOp { lhs = v1; rhs = v2; _ }
  | SignedVecLaneBiOp { lhs = v1; rhs = v2; _ }
  | VecShiftLeftOp { operand = v1; count = v2; _ }
  | VecShiftRightOp { operand = v1; count = v2; _ }
  | VecReplaceLaneOp { dest = v1; lane_value = v2; _ }
  | VecShuffleOp { arg1 = v1; arg2 = v2; _ }
  | VecLoadLaneOp { dest_vec = v1; addr = v2; _ }
  | StoreOp { addr = v1; value = v2; _ }
  | VecStoreLaneOp { addr = v1; value = v2; _ } ->
      f v1 (f v2 s)
  | Memset { dest = v1; value = v2; count = v3 }
  | Memcopy { dest = v1; src = v2; count = v3 } ->
      f v1 (f v2 (f v3 s))
  | CallOp v -> List.fold_right ~f ~init:s v.args
  | CallIndirectOp v ->
      let s = List.fold_right ~f ~init:s v.args in
      f v.table_index s

let iter f = fold (fun () -> f) ()
let iter_right f = fold_right (fun r () -> f r) ()
let arg_list = fold_right List.cons []

let map f t =
  match t with
  | Landmine _ | Unreachable | OutsideContext _ | ReturnedOp _ | GetGlobalOp _
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | Nop ->
      t
  | DupVar v ->
      let r = f v.src in
      if Ref.equal r v.src then t else DupVar { v with src = r }
  | UniOp v ->
      let r = f v.operand in
      if Ref.equal r v.operand then t else UniOp { v with operand = r }
  | VecExtend v ->
      let r = f v.operand in
      if Ref.equal r v.operand then t else VecExtend { v with operand = r }
  | VecSplatOp v ->
      let r = f v.value in
      if Ref.equal r v.value then t else VecSplatOp { v with value = r }
  | SetGlobalOp v ->
      let r = f v.value in
      if Ref.equal r v.value then t else SetGlobalOp { v with value = r }
  | LoadOp v ->
      let r = f v.addr in
      if Ref.equal r v.addr then t else LoadOp { v with addr = r }
  | VecExtractLaneOp v ->
      let r = f v.src in
      if Ref.equal r v.src then t else VecExtractLaneOp { v with src = r }
  | SignedLoadOp v ->
      let r = f v.addr in
      if Ref.equal r v.addr then t else SignedLoadOp { v with addr = r }
  | AssertOp v ->
      let r = f v.condition in
      if Ref.equal r v.condition then t else AssertOp { condition = r }
  | BiOp v ->
      let r1 = f v.lhs in
      let r2 = f v.rhs in
      if Ref.equal r1 v.lhs && Ref.equal r2 v.rhs then t
      else BiOp { v with lhs = r1; rhs = r2 }
  | VecLaneBiOp v ->
      let r1 = f v.lhs in
      let r2 = f v.rhs in
      if Ref.equal r1 v.lhs && Ref.equal r2 v.rhs then t
      else VecLaneBiOp { v with lhs = r1; rhs = r2 }
  | SignedBiOp v ->
      let r1 = f v.lhs in
      let r2 = f v.rhs in
      if Ref.equal r1 v.lhs && Ref.equal r2 v.rhs then t
      else SignedBiOp { v with lhs = r1; rhs = r2 }
  | SignedVecLaneBiOp v ->
      let r1 = f v.lhs in
      let r2 = f v.rhs in
      if Ref.equal r1 v.lhs && Ref.equal r2 v.rhs then t
      else SignedVecLaneBiOp { v with lhs = r1; rhs = r2 }
  | VecShiftLeftOp v ->
      let r1 = f v.operand in
      let r2 = f v.count in
      if Ref.equal r1 v.operand && Ref.equal r2 v.count then t
      else VecShiftLeftOp { v with operand = r1; count = r2 }
  | VecShiftRightOp v ->
      let r1 = f v.operand in
      let r2 = f v.count in
      if Ref.equal r1 v.operand && Ref.equal r2 v.count then t
      else VecShiftRightOp { v with operand = r1; count = r2 }
  | VecReplaceLaneOp v ->
      let r1 = f v.dest in
      let r2 = f v.lane_value in
      if Ref.equal r1 v.dest && Ref.equal r2 v.lane_value then t
      else VecReplaceLaneOp { v with dest = r1; lane_value = r2 }
  | VecShuffleOp v ->
      let r1 = f v.arg1 in
      let r2 = f v.arg2 in
      if Ref.equal r1 v.arg1 && Ref.equal r2 v.arg2 then t
      else VecShuffleOp { v with arg1 = r1; arg2 = r2 }
  | VecLoadLaneOp v ->
      let r1 = f v.dest_vec in
      let r2 = f v.addr in
      if Ref.equal r1 v.dest_vec && Ref.equal r2 v.addr then t
      else VecLoadLaneOp { v with dest_vec = r1; addr = r2 }
  | VecStoreLaneOp v ->
      let r1 = f v.addr in
      let r2 = f v.value in
      if Ref.equal r1 v.addr && Ref.equal r2 v.value then t
      else VecStoreLaneOp { v with addr = r1; value = r2 }
  | StoreOp v ->
      let r1 = f v.addr in
      let r2 = f v.value in
      if Ref.equal r1 v.addr && Ref.equal r2 v.value then t
      else StoreOp { v with addr = r1; value = r2 }
  | Memcopy v ->
      let r1 = f v.dest in
      let r2 = f v.src in
      let r3 = f v.count in
      if Ref.equal r1 v.dest && Ref.equal r2 v.src && Ref.equal r3 v.count then
        t
      else Memcopy { dest = r1; src = r2; count = r3 }
  | Memset v ->
      let r1 = f v.dest in
      let r2 = f v.value in
      let r3 = f v.count in
      if Ref.equal r1 v.dest && Ref.equal r2 v.value && Ref.equal r3 v.count
      then t
      else Memset { dest = r1; value = r2; count = r3 }
  | CallOp v ->
      let r = List.map ~f v.args in
      if equal_list Ref.equal r v.args then t else CallOp { v with args = r }
  | CallIndirectOp v ->
      let r1 = f v.table_index in
      let r2 = List.map ~f v.args in
      if Ref.equal r1 v.table_index && equal_list Ref.equal r2 v.args then t
      else CallIndirectOp { args = r2; table_index = r1 }

(* Using flambda terminology, this definition of pureness only accounts for effects, not coeffects *)
(* This is only used for dead code elimination, as we currently don't reorder instructions *)
let is_pure = function
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | DupVar _ | UniOp _
  | VecExtend _ | BiOp _ | VecLaneBiOp _ | SignedBiOp _ | SignedVecLaneBiOp _
  | LoadOp _ | SignedLoadOp _ | VecShiftLeftOp _ | VecShiftRightOp _
  | VecSplatOp _ | VecReplaceLaneOp _ | VecExtractLaneOp _ | VecShuffleOp _
  | VecLoadLaneOp _ | Landmine _ | Nop | GetGlobalOp _ | OutsideContext _ ->
      true
  | CallOp _ | CallIndirectOp _ | ReturnedOp _ | StoreOp _ | VecStoreLaneOp _
  | AssertOp _ | Memset _ | Memcopy _ | Unreachable | SetGlobalOp _ ->
      false

let is_assignment = function
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | DupVar _ | UniOp _
  | VecExtend _ | BiOp _ | VecLaneBiOp _ | SignedBiOp _ | SignedVecLaneBiOp _
  | LoadOp _ | SignedLoadOp _ | VecShiftLeftOp _ | VecShiftRightOp _
  | VecSplatOp _ | VecReplaceLaneOp _ | VecExtractLaneOp _ | VecShuffleOp _
  | VecLoadLaneOp _ | ReturnedOp _ | GetGlobalOp _ | OutsideContext _
  | Landmine _ ->
      true
  | CallOp _ | CallIndirectOp _ | StoreOp _ | VecStoreLaneOp _ | AssertOp _
  | Unreachable | SetGlobalOp _ | Memset _ | Memcopy _ | Nop ->
      false

let assignment_var = function
  | Const (var, _)
  | FloatConst (var, _)
  | LongConst (var, _)
  | VecConst { var; _ }
  | DupVar { var; _ }
  | VecExtend { var; _ }
  | UniOp { var; _ }
  | BiOp { var; _ }
  | VecLaneBiOp { var; _ }
  | SignedBiOp { var; _ }
  | SignedVecLaneBiOp { var; _ }
  | VecShiftLeftOp { var; _ }
  | VecShiftRightOp { var; _ }
  | VecSplatOp { var; _ }
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
  | AssertOp _ | Unreachable | Memset _ | Memcopy _ | Nop ->
      None

let assignment_var_exn = function
  | Const (var, _)
  | FloatConst (var, _)
  | LongConst (var, _)
  | VecConst { var; _ }
  | DupVar { var; _ }
  | VecExtend { var; _ }
  | UniOp { var; _ }
  | BiOp { var; _ }
  | VecLaneBiOp { var; _ }
  | SignedBiOp { var; _ }
  | SignedVecLaneBiOp { var; _ }
  | VecShiftLeftOp { var; _ }
  | VecShiftRightOp { var; _ }
  | VecSplatOp { var; _ }
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
      var
  | CallOp _ | CallIndirectOp _ | StoreOp _ | VecStoreLaneOp _ | SetGlobalOp _
  | AssertOp _ | Unreachable | Memset _ | Memcopy _ | Nop ->
      failwith "not an assignment instruction"
