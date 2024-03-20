open Core
open Types
include Types.Instr

let local_type_of_lane_shape = function
  | I8 | I16 | I32 -> Int
  | I64 -> Long
  | F32 | F64 -> Float

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
  | CallOp { return_type; _ } | CallIndirectOp { return_type; _ } -> return_type
  | GetGlobalOp { global_type; _ } -> global_type
  | OutsideContext { typ; _ } -> typ
  | Landmine -> Int (* Could make this "polymorphic" later *)
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
  | CallOp v -> CallOp { v with var }
  | CallIndirectOp v -> CallIndirectOp { v with var }
  | GetGlobalOp v -> GetGlobalOp { v with var }
  | OutsideContext v -> OutsideContext { v with var }
  | StoreOp _ | VecStoreLaneOp _ | SetGlobalOp _ | AssertOp _ | Landmine
  | Unreachable ->
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
  | VecReplaceLaneOp v when v.src <> from && v.lane_value <> from -> t
  | VecReplaceLaneOp v ->
      VecReplaceLaneOp
        { v with src = swap v.src; lane_value = swap v.lane_value }
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
        {
          v with
          table_index = swap v.table_index;
          args = List.map v.args ~f:swap;
        }
  | StoreOp v when v.addr <> from && v.value <> from -> t
  | StoreOp v -> StoreOp { v with addr = swap v.addr; value = swap v.value }
  | VecStoreLaneOp v when v.addr <> from && v.value <> from -> t
  | VecStoreLaneOp v ->
      VecStoreLaneOp { v with addr = swap v.addr; value = swap v.value }
  | SetGlobalOp v when v.value <> from -> t
  | SetGlobalOp v -> SetGlobalOp { v with value = into }
  | AssertOp v when v.condition <> from -> t
  | AssertOp _ -> AssertOp { condition = into }
  | Landmine | Unreachable | OutsideContext _ | GetGlobalOp _ | Const _
  | FloatConst _ | LongConst _ | VecConst _ ->
      t

(* Using flambda terminology, this definition of pureness only accounts for effects, not coeffects *)
(* This is only used for dead code elimination, as we currently don't reorder instructions *)
let is_pure = function
  | Const _ | FloatConst _ | LongConst _ | VecConst _ | DupVar _ | UniOp _
  | BiOp _ | VecLaneBiOp _ | SignedBiOp _ | SignedVecLaneBiOp _ | LoadOp _
  | SignedLoadOp _ | VecReplaceLaneOp _ | VecExtractLaneOp _ | VecShuffleOp _
  | VecLoadLaneOp _ ->
      true
  | CallOp _ | CallIndirectOp _ | GetGlobalOp _ | OutsideContext _ | Landmine
  | StoreOp _ | VecStoreLaneOp _ | AssertOp _ | Unreachable | SetGlobalOp _ ->
      false
