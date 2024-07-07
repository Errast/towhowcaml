open! Core
open Types

type t = {
  instrs : Instr_list.builder;
  currentVar : (ident, Local_info.t) Hashtbl.t;
  locals : (local_type Map.M(String).t[@sexp.opaque]);
  mutable roots : Set.M(Instr.Ref).t;
  mutable check_var_is_latest : bool;
}
[@@deriving sexp_of]

let deconstruct { instrs; currentVar; locals; roots; _ } =
  (Vec.to_array instrs, currentVar, locals, roots)

let get_instr t (Instr.Ref.Ref i) = Vec.get t.instrs i
let set_check_var_is_latest t b = t.check_var_is_latest <- b
let float_temp : ident = "__fl"
let long_temp : ident = "__i64"
let vec_temp : ident = "__vec"
let int_temp : ident = "__i32"

let is_temp id =
  String.(id = int_temp || id = float_temp || id = long_temp || id = vec_temp)

let create locals =
  let mk_temp typ =
    Local_info.{ subscript = 0; index = Instr.Ref.invalid; typ }
  in
  let currentVar =
    Hashtbl.of_alist_exn ~growth_allowed:false
      ~size:(Map.length locals + 4)
      (module String)
      [
        (int_temp, mk_temp Int);
        (float_temp, mk_temp Float);
        (long_temp, mk_temp Long);
        (vec_temp, mk_temp Vec);
      ]
  in
  {
    instrs = Vec.create ();
    currentVar;
    locals;
    roots = Set.empty (module Instr.Ref);
    check_var_is_latest = true;
  }

let add_instr t instr =
  let instr_ref = Instr.Ref.Ref (Vec.length t.instrs) in
  if not @@ Instr.is_pure instr then t.roots <- Set.add t.roots instr_ref;
  Vec.add t.instrs instr;
  instr_ref

let type_temp = function
  | Int -> int_temp
  | Float -> float_temp
  | Vec -> vec_temp
  | Long -> long_temp

let new_info t ~add_ctx varName =
  match Map.find t.locals varName with
  | Some typ ->
      if add_ctx then
        let index =
          add_instr t
          @@ Instr.OutsideContext { typ; var = Variable.{ name = varName } }
        in
        Local_info.{ subscript = 0; index; typ }
      else Local_info.{ subscript = 0; index = Instr.Ref.invalid; typ }
  | _ ->
      raise
      @@ Not_found_s [%message "No local found" ~varName:(varName : string)]

let new_var t varName typ =
  let varName, subscript =
    match varName with
    (* Use a temp local; skip type check *)
    | None ->
        let varName = type_temp typ in
        let subscript =
          Hashtbl.find t.currentVar varName
          |> Option.value_map ~f:(fun c -> c.subscript) ~default:1
        in
        (varName, subscript)
    | Some varName ->
        let info =
          Hashtbl.find t.currentVar varName
          |> Option.value_or_thunk ~default:(fun () ->
                 new_info t varName ~add_ctx:false)
        in
        if Poly.(info.typ <> typ) then
          raise
          @@ Not_found_s
               [%message
                 "Wrong type for local"
                   ~given:(typ : local_type)
                   (info : Local_info.t)];
        (varName, info.subscript + 1)
  in
  Hashtbl.set t.currentVar ~key:varName
    ~data:{ typ; subscript; index = Instr.ref (Vec.length t.instrs) };
  Variable.{ name = varName }

let newest_var t varName =
  let info =
    Hashtbl.find_or_add t.currentVar varName ~default:(fun () ->
        new_info t varName ~add_ctx:true)
  in
  info.index

let newest_var_opt t varName =
  Hashtbl.find t.currentVar varName |> Option.map ~f:(fun i -> i.index)

let get_var t (Instr.Ref.Ref var as instr_ref) =
  Vec.get t.instrs var |> Instr.assignment_var
  |> Option.value_or_thunk ~default:(fun () ->
         raise_s
           [%message
             "Instr isn't an assignment" ~instr_ref:(instr_ref : Instr.Ref.t)])

let verify_var (Instr.Ref.Ref var as instr_ref) typ ?(check_type = true) t =
  if var < 0 || var >= Vec.length t.instrs then
    raise @@ Invalid_argument "Invalid instr ref";
  let found_typ = Instr.value_type @@ Vec.get t.instrs var in
  if check_type && Poly.(found_typ <> typ) then
    raise_s
      [%message
        "Wrong local type"
          ~given:(typ : local_type)
          ~found:(found_typ : local_type)];
  let declared = get_var t instr_ref in
  if
    t.check_var_is_latest
    && (not (is_temp declared.name))
    && Hashtbl.find t.currentVar declared.name
       |> Option.value_map ~default:false ~f:(fun i ->
              not @@ Instr.Ref.equal instr_ref i.index)
  then raise_s [%message "Outdated local" ~found:(declared : Variable.t)]
  else ()

let verify_lane shape lane =
  let lanes =
    match shape with
    | `I8 -> 16
    | `I16 -> 8
    | `I32 | `F32 -> 4
    | `I64 | `F64 -> 2
  in
  if lane < 0 || lane > lanes then
    raise_s
      [%message
        "Invalid lane number" ~shape:(shape : vec_lane_shape) ~lane:(lane : int)]

let vec_lane_type = function
  | `I8 | `I16 | `I32 -> Int
  | `F32 | `F64 -> Float
  | `I64 -> Long

let const ?varName t value =
  (* wtf is this here for? *)
  (* assert (Sys.int_size_in_bits <= 32 || value asr 32 = 0); *)
  add_instr t @@ Instr.Const (new_var t varName Int, value)

let float_const ?varName t value =
  add_instr t @@ Instr.FloatConst (new_var t varName Float, value)

let long_const ?varName t value =
  add_instr t @@ Instr.LongConst (new_var t varName Long, value)

let vec_const ?varName t ~l ~h =
  add_instr t
  @@ Instr.VecConst
       { var = new_var t varName Vec; lower_bits = l; upper_bits = h }

let dup_var ?varName t typ src =
  verify_var src typ t;
  add_instr t @@ Instr.DupVar { var = new_var t varName typ; src; typ }

type uni_op_add = ?varName:ident -> t -> Instr.ref -> Instr.ref

type uni_op_add_signed =
  ?varName:ident -> t -> Instr.ref -> signed:bool -> Instr.ref

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

type load_op_add = ?varName:ident -> ?offset:int -> t -> Instr.ref -> Instr.ref

type sign_load_op_add =
  ?varName:ident -> ?offset:int -> t -> Instr.ref -> signed:bool -> Instr.ref

type store_op_add =
  ?offset:int -> t -> value:Instr.ref -> addr:Instr.ref -> unit

let uni_op operand_typ op res_typ ?varName t operand =
  verify_var operand operand_typ t;
  add_instr t @@ Instr.UniOp { var = new_var t varName res_typ; operand; op }

let uni_op_signed operand_typ ~op_s ~op_u res_typ ?varName t operand ~signed =
  uni_op operand_typ (if signed then op_s else op_u) res_typ t operand ?varName

let bi_op typ op res_typ ?varName t ~lhs ~rhs =
  verify_var lhs typ t;
  verify_var rhs typ t;
  add_instr t @@ Instr.BiOp { var = new_var t varName res_typ; lhs; rhs; op }

let vec_bi_op op ?varName t ~lhs ~rhs ~shape =
  verify_var lhs Vec t;
  verify_var rhs Vec t;
  add_instr t
  @@ Instr.VecLaneBiOp { var = new_var t varName Vec; lhs; rhs; op; shape }

let sign_bi_op typ op res_typ ?varName t ~lhs ~rhs ~signed =
  verify_var lhs typ t;
  verify_var rhs typ t;
  add_instr t
  @@ Instr.SignedBiOp { var = new_var t varName res_typ; lhs; rhs; signed; op }

(* the signed tag does nothing for float shapes *)
type (_, _) vec_lane_shape_signed =
  | I8 : ([> `I8 ], signed:bool -> Instr.ref) vec_lane_shape_signed
  | I16 : ([> `I16 ], signed:bool -> Instr.ref) vec_lane_shape_signed
  | I32 : ([> `I32 ], signed:bool -> Instr.ref) vec_lane_shape_signed
  | I64 : ([> `I64 ], signed:bool -> Instr.ref) vec_lane_shape_signed
  | F32 : ([> `F32 ], Instr.ref) vec_lane_shape_signed
  | F64 : ([> `F64 ], Instr.ref) vec_lane_shape_signed

let shape_of_gadt (type t s) (s : (t, s) vec_lane_shape_signed) : vec_lane_shape
    =
  match s with
  | I8 -> `I8
  | I16 -> `I16
  | I32 -> `I32
  | I64 -> `I64
  | F32 -> `F32
  | F64 -> `F64

type ('r, 's) signed_vec_lane_bi_op_add =
  shape:('r, 's) vec_lane_shape_signed ->
  ?varName:ident ->
  t ->
  lhs:Instr.ref ->
  rhs:Instr.ref ->
  's

(* pretty funny *)
let sign_vec_bi_op :
    type s r.
    Instr.signed_vec_lane_bi_op ->
    ?varName:ident ->
    t ->
    lhs:Instr.ref ->
    rhs:Instr.ref ->
    shape:(s, r) vec_lane_shape_signed ->
    r =
 fun op ?varName t ~lhs ~rhs ~shape ->
  verify_var lhs Vec t;
  verify_var rhs Vec t;
  let signed_fun ~signed =
    add_instr t
    @@ Instr.SignedVecLaneBiOp
         {
           var = new_var t varName Vec;
           shape = shape_of_gadt shape;
           lhs;
           rhs;
           signed;
           op;
         }
  in
  let not_signed_fun () = signed_fun ~signed:true in
  match shape with
  | I8 -> signed_fun
  | I16 -> signed_fun
  | I32 -> signed_fun
  | I64 -> signed_fun
  | F32 -> not_signed_fun ()
  | F64 -> not_signed_fun ()

let load_op op res_typ ?varName ?(offset = 0) t addr =
  verify_var addr Int t;
  add_instr t
  @@ Instr.LoadOp { var = new_var t varName res_typ; op; addr; offset }

let sign_load_op op res_typ ?varName ?(offset = 0) t addr ~signed =
  verify_var addr Int t;
  add_instr t
  @@ Instr.SignedLoadOp
       { var = new_var t varName res_typ; op; addr; offset; signed }

let store_op typ op ?(offset = 0) t ~value ~addr =
  verify_var value typ t;
  verify_var addr Int t;
  add_instr t @@ Instr.StoreOp { op; addr; value; offset } |> ignore

let equals_zero = uni_op Int Instr.EqualsZero Int
let long_equals_zero = uni_op Long Instr.LongEqualsZero Int
let zero_extend_low8 = uni_op Int Instr.ZeroExtendLow8 Int
let zero_extend_high8 = uni_op Int Instr.ZeroExtendHigh8 Int
let zero_extend_16 = uni_op Int Instr.ZeroExtend16 Int
let sign_extend_low8 = uni_op Int Instr.SignExtendLow8 Int
let sign_extend_high8 = uni_op Int Instr.SignExtendHigh8 Int
let sign_extend_16 = uni_op Int Instr.SignExtend16 Int
let float_to_int32 = uni_op Float Instr.FloatToInt32 Int
let long_to_int32 = uni_op Long Instr.LongToInt32 Int
let count_ones = uni_op Int Instr.CountOnes Int
let float_neg = uni_op Float Instr.FloatNeg Float
let float_abs = uni_op Float Instr.FloatAbs Float
let float_round = uni_op Float Instr.FloatRound Float
let float_trunc = uni_op Float Instr.FloatTrunc Float
let float_sqrt = uni_op Float Instr.FloatSqrt Float
let float_sine = uni_op Float Instr.FloatSine Float
let float_cosine = uni_op Float Instr.FloatCosine Float
let float_tangent = uni_op Float Instr.FloatTangent Float

let int32_to_float =
  uni_op_signed Int ~op_s:Instr.Int32ToFloatSigned
    ~op_u:Instr.Int32ToFloatUnsigned Float

let int64_to_float =
  uni_op_signed Long ~op_s:Instr.Int64ToFloatSigned
    ~op_u:Instr.Int64ToFloatUnsigned Float

let int32_to_long =
  uni_op_signed Int ~op_s:Instr.Int32ToLongSigned
    ~op_u:Instr.Int32ToLongUnsigned Long

let float_to_long = uni_op Float Instr.FloatToLong Long

let vec_convert_low_32bits_to_float_signed =
  uni_op Vec Instr.VecConvertLow32BitsToFloatsSigned Vec

let vec_int8_sign_bitmask = uni_op Vec Instr.VecInt8SignBitmask Int

let vec_extend ?varName b ~shape ~signed ~half_used operand =
  verify_var operand Vec b;
  add_instr b
  @@ VecExtend
       { var = new_var b varName Vec; shape; signed; operand; half_used }

let add = bi_op Int Instr.Add Int
let sub = bi_op Int Instr.Subtract Int
let mul = bi_op Int Instr.Multiply Int
let equal = bi_op Int Instr.Equal Int
let not_equal = bi_op Int Instr.NotEqual Int
let int_and = bi_op Int Instr.And Int
let int_or = bi_op Int Instr.Or Int
let xor = bi_op Int Instr.Xor Int
let long_eq = bi_op Long Instr.LongEq Long
let shift_left = bi_op Int Instr.ShiftLeft Int
let rotate_left = bi_op Int Instr.RotateLeft Int
let long_equal = bi_op Long Instr.LongEq Long
let long_not_equal = bi_op Long Instr.LongNotEq Int
let float_equal = bi_op Float Instr.FloatEq Int
let float_not_equal = bi_op Float Instr.FloatNotEq Int
let float_greater_than = bi_op Float Instr.FloatGreaterThan Int
let float_less_than = bi_op Float Instr.FloatLessThan Int
let float_greater_than_equal = bi_op Float Instr.FloatGreaterThanEqual Int
let float_less_than_equal = bi_op Float Instr.FloatLessThanEqual Int
let merge_trunc_low8 = bi_op Int Instr.MergeTruncLow8 Int
let merge_trunc_high8 = bi_op Int Instr.MergeTruncHigh8 Int
let merge_trunc_16 = bi_op Int Instr.MergeTrunc16 Int
let float_add = bi_op Float Instr.FloatAdd Float
let float_sub = bi_op Float Instr.FloatSub Float
let float_mul = bi_op Float Instr.FloatMult Float
let float_div = bi_op Float Instr.FloatDiv Float
let float_atan2 = bi_op Float Instr.FloatAtan2 Float
let long_add = bi_op Long Instr.LongAdd Long
let long_sub = bi_op Long Instr.LongSub Long
let long_mul = bi_op Long Instr.LongMultiply Long
let long_shift_left = bi_op Long Instr.LongShiftLeft Long
let long_rotate_left = bi_op Long Instr.LongRotateLeft Long
let long_and = bi_op Long Instr.LongAnd Long
let long_or = bi_op Long Instr.LongOr Long
let vec_and = bi_op Vec Instr.VecAnd Vec
let vec_or = bi_op Vec Instr.VecOr Vec
let vec_xor = bi_op Vec Instr.VecXor Vec
let vec_mul_add_16bit = bi_op Vec Instr.VecMulAdd16Bit Vec
let vec_add = vec_bi_op Instr.VecAdd
let vec_sub = vec_bi_op Instr.VecSub
let vec_mul = vec_bi_op Instr.VecMul
let vec_equal = vec_bi_op Instr.VecEqual
let vec_not_equal = vec_bi_op Instr.VecNotEqual
let div = sign_bi_op Int Instr.Divide Int
let remainder = sign_bi_op Int Instr.Remainder Int
let shift_right = sign_bi_op Int Instr.ShiftRight Int
let rotate_right = sign_bi_op Int Instr.RotateRight Int
let less_than = sign_bi_op Int Instr.LessThan Int
let greater_than = sign_bi_op Int Instr.GreaterThan Int
let less_than_equal = sign_bi_op Int Instr.LessThanEqual Int
let greater_than_equal = sign_bi_op Int Instr.GreaterThanEqual Int
let long_less_than = sign_bi_op Long Instr.LongLessThan Int
let long_greater_than = sign_bi_op Long Instr.LongGreaterThan Int
let long_less_than_equal = sign_bi_op Long Instr.LongLessThanEqual Int
let long_greater_than_equal = sign_bi_op Long Instr.LongGreaterThanEqual Int
let long_div = sign_bi_op Long Instr.LongDivide Long
let long_remainder = sign_bi_op Long Instr.LongRemainder Long
let long_shift_right = sign_bi_op Long Instr.LongShiftRight Long
let long_rotate_right = sign_bi_op Long Instr.LongRotateRight Long
let vec_narrow_16bit = sign_bi_op Vec Instr.VecNarrow16Bit Long
let vec_narrow_32bit = sign_bi_op Vec Instr.VecNarrow32Bit Long
let vec_max ~shape = sign_vec_bi_op Instr.VecMax ~shape
let vec_min ~shape = sign_vec_bi_op Instr.VecMin ~shape
let vec_less_than ~shape = sign_vec_bi_op Instr.VecLessThan ~shape
let vec_less_than_equal ~shape = sign_vec_bi_op Instr.VecLessThan ~shape
let vec_greater_than ~shape = sign_vec_bi_op Instr.VecGreaterThan ~shape
let vec_greater_than_equal ~shape = sign_vec_bi_op Instr.VecGreaterThan ~shape

let vec_add_sat ~(shape : ([ `I8 | `I16 ], _) vec_lane_shape_signed) =
  sign_vec_bi_op Instr.VecAddSaturating ~shape

let vec_sub_sat ~(shape : ([ `I8 | `I16 ], _) vec_lane_shape_signed) =
  sign_vec_bi_op Instr.VecSubSaturating ~shape

let vec_div ~shape =
  sign_vec_bi_op Instr.VecDiv ~shape

let vec_shift_left ?varName t ~lhs ~rhs ~(shape : int_vec_lane_shape) =
  verify_var lhs Vec t;
  verify_var rhs Int t;
  add_instr t
  @@ Instr.VecShiftLeftOp
       { var = new_var t varName Vec; operand = lhs; count = rhs; shape }

let vec_shift_right ?varName t ~lhs ~rhs ~signed ~(shape : int_vec_lane_shape) =
  verify_var lhs Vec t;
  verify_var rhs Int t;
  add_instr t
  @@ Instr.VecShiftRightOp
       {
         var = new_var t varName Vec;
         operand = lhs;
         count = rhs;
         shape;
         signed;
       }

let vec_splat ?varName t value ~shape =
  verify_var value (Instr.local_type_of_lane_shape shape) t;
  add_instr t @@ Instr.VecSplatOp { var = new_var t varName Vec; value; shape }

let vec_extract ?varName t src ~lane ~shape =
  verify_lane shape lane;
  verify_var src Vec t;
  add_instr t
  @@ Instr.VecExtractLaneOp { var = new_var t varName (vec_lane_type shape); src; shape; lane }

let vec_replace ?varName t ~dest ~value ~lane ~shape =
  verify_lane shape lane;
  verify_var dest Vec t;
  verify_var value (vec_lane_type shape) t;
  add_instr t
  @@ Instr.VecReplaceLaneOp
       { var = new_var t varName Vec; lane_value = value; dest; shape; lane }

let vec_shuffle ?varName t ~vec1 ~vec2 ~ctrl_l ~ctrl_h =
  verify_var vec1 Vec t;
  verify_var vec2 Vec t;
  add_instr t
  @@ Instr.VecShuffleOp
       {
         var = new_var t varName Vec;
         control_upper_bits = ctrl_h;
         control_lower_bits = ctrl_l;
         arg1 = vec1;
         arg2 = vec2;
       }

let load8 = sign_load_op Instr.Load8 Int
let load16 = sign_load_op Instr.Load16 Int
let load32 = load_op Instr.Load32 Int
let float_load32 = load_op Instr.FloatLoad32 Float
let float_load64 = load_op Instr.FloatLoad64 Float
let long_load64 = load_op Instr.LongLoad64 Long
let vec_load32_zero_extend = load_op Instr.VecLoad32ZeroExtend Vec
let vec_load64_zero_extend = load_op Instr.VecLoad64ZeroExtend Vec
let vec_load128 = load_op Instr.VecLoad128 Vec

let vec_load ?varName ?(offset = 0) t ~dest ~addr ~shape ~lane =
  verify_lane shape lane;
  verify_var dest Vec t;
  verify_var addr Int t;
  add_instr t
  @@ Instr.VecLoadLaneOp
       {
         var = new_var t varName Vec;
         dest_vec = dest;
         addr;
         shape;
         lane;
         offset;
       }

let call t func args = add_instr t @@ Instr.CallOp { func; args } |> ignore

let call_indirect t table_index args =
  add_instr t @@ Instr.CallIndirectOp { table_index; args } |> ignore

let returned ?varName t typ =
  (match Vec.get_opt t.instrs (Vec.length t.instrs - 1) with
  | Some Instr.(CallOp _ | CallIndirectOp _ | ReturnedOp _) -> ()
  | _ -> raise_s [%message "invalid previous instr for returned" (t : t)]);
  add_instr t @@ Instr.ReturnedOp { var = new_var t varName typ; typ }

let get_global ?varName t global =
  add_instr t
  @@ Instr.GetGlobalOp { var = new_var t varName global.typ; global }

let landmine ?varName t typ =
  add_instr t @@ Instr.Landmine { var = new_var t varName typ; typ }

let store8 = store_op Int Store8
let store16 = store_op Int Store16
let store32 = store_op Int Store32
let float_store32 = store_op Float Store32
let float_store64 = store_op Float FloatStore64
let long_store64 = store_op Long LongStore64
let vec_store128 = store_op Vec VecStore128

let vec_store ?(offset = 0) t ~addr ~vec ~shape ~lane =
  verify_lane shape lane;
  verify_var vec Vec t;
  verify_var addr Int t;
  add_instr t @@ Instr.VecStoreLaneOp { value = vec; addr; shape; lane; offset }
  |> ignore

let set_global t global value =
  verify_var value global.typ t;
  add_instr t @@ Instr.SetGlobalOp { value; global } |> ignore

let mir_assert t condition =
  verify_var condition Int t;
  add_instr t @@ Instr.AssertOp { condition } |> ignore

let unreachable t () = add_instr t Unreachable |> ignore

let memset t ~count ~value ~dest =
  verify_var dest Int t;
  verify_var value Int t;
  verify_var count Int t;
  add_instr t @@ Instr.Memset { dest; value; count } |> ignore

let memcopy t ~count ~src ~dest =
  verify_var dest Int t;
  verify_var src Int t;
  verify_var count Int t;
  add_instr t @@ Instr.Memcopy { dest; src; count } |> ignore
