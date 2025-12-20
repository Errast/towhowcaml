open! Core
open Types
open Smt_util

type ctx = {
  vals : expr Option_array.t;
  memory : Array.t Map.M(Int).t;
  vars : expr Map.M(String).t;
}

let update_val ctx i v =
  assert (Option_array.is_none ctx.vals i);
  Option_array.set_some ctx.vals i v;
  ctx

let get_val : ctx -> Instr.ref -> expr =
 fun ctx (Ref i) -> Option_array.get_some_exn ctx.vals i

let bv_of_bool ?(sort = Bitvec.dword ()) b =
  if_ b ~then_:(of_int sort 1) ~else_:(of_int sort 0)

let fp_of shape bv =
  FP.reinterpret_of_bv
    FP.(match shape with `F32 -> single () | `F64 -> double ())
    bv

let lane_size = function
  | `I8 -> 8
  | `I16 -> 16
  | `I64 | `F64 -> 64
  | `I32 | `F32 -> 32

let interpret_uni_op : Instr.uni_op -> expr -> expr =
 fun op operand ->
  match op with
  | EqualsZero -> Smt.(operand = of_int (Bitvec.dword ()) 0) |> bv_of_bool
  | LongEqualsZero -> Smt.(operand = of_int (Bitvec.qword ()) 0) |> bv_of_bool
  | SignExtendLow8 -> Bitvec.(extract 7 0 operand |> sign_ext 24)
  | SignExtendHigh8 -> Bitvec.(extract 15 8 operand |> sign_ext 24)
  | SignExtend16 -> Bitvec.(extract 15 0 operand |> sign_ext 16)
  | ZeroExtendLow8 -> Bitvec.(extract 7 0 operand |> zero_ext 24)
  | ZeroExtendHigh8 -> Bitvec.(extract 15 8 operand |> zero_ext 24)
  | ZeroExtend16 -> Bitvec.(extract 15 0 operand |> zero_ext 16)
  (* TODO: truncate gets stuck when the float is out of range of the int *)
  | FloatToInt32 -> FP.truncate_to_bv 32 operand
  | LongToInt32 -> Bitvec.extract 31 0 operand
  | CountOnes -> Bitvec.popcount32 operand
  | VecInt8SignBitmask -> Bitvec.make_bitmask operand
  | CountLeadingZeros -> Bitvec.leading_zeros32 operand
  | FloatNeg -> FP.neg operand
  | FloatAbs -> FP.abs operand
  | FloatRound -> FP.round (FP.rounding_mode ()) operand
  | FloatTrunc -> FP.round (FP.truncate_mode ()) operand
  | FloatSqrt -> FP.sqrt operand
  | Int32ToFloatUnsigned | Int64ToFloatUnsigned ->
      FP.of_bv (FP.double ()) ~signed:false operand
  | Int32ToFloatSigned | Int64ToFloatSigned ->
      FP.of_bv (FP.double ()) ~signed:true operand
  | BitcastInt64ToFloat -> FP.reinterpret_of_bv (FP.double ()) operand
  (* TODO: same as above *)
  | FloatToLong -> FP.truncate_to_bv 64 operand
  | Int32ToLongUnsigned -> Bitvec.zero_ext 32 operand
  | Int32ToLongSigned -> Bitvec.sign_ext 32 operand
  | BitcastFloatToLong -> FP.reinterpret_to_bv operand
  | LongCountLeadingZeros -> Bitvec.leading_zeros64 operand
  | VecConvertLow32BitsToFloatsSigned ->
      Bitvec.(
        concat
          FP.(of_bv (double ()) ~signed:true @@ extract 63 32 operand)
          FP.(of_bv (double ()) ~signed:true @@ extract 31 0 operand))

let interpret_bi_op : Instr.bi_op -> expr -> expr -> expr =
 fun op lhs rhs ->
  match op with
  | Add | LongAdd -> Bitvec.(lhs + rhs)
  | Subtract | LongSub -> Bitvec.(lhs - rhs)
  | Multiply | LongMultiply -> Bitvec.(lhs * rhs)
  | Equal | LongEq | FloatEq -> Smt.(lhs = rhs) |> bv_of_bool
  | NotEqual | LongNotEq | FloatNotEq -> Smt.(not_ @@ lhs = rhs) |> bv_of_bool
  | And | LongAnd | VecAnd -> Bitvec.(lhs land rhs)
  | Or | LongOr | VecOr -> Bitvec.(lhs lor rhs)
  | Xor | LongXor | VecXor -> Bitvec.(lhs lxor rhs)
  | RotateLeft | LongRotateLeft -> Bitvec.rotate_left lhs rhs
  | RotateRight | LongRotateRight -> Bitvec.rotate_left lhs rhs
  | ShiftLeft -> Bitvec.(lhs lsl (rhs land of_int (dword ()) 0x1F))
  | FloatGreaterThan -> FP.(lhs > rhs)
  | FloatLessThan -> FP.(lhs < rhs)
  | FloatGreaterThanEqual -> FP.(lhs >= rhs)
  | FloatLessThanEqual -> FP.(lhs <= rhs)
  | MergeTruncLow8 -> Bitvec.(concat (extract 63 8 lhs) (extract 7 0 rhs))
  | MergeTruncHigh8 ->
      Bitvec.(
        concat (extract 63 16 lhs)
        @@ concat (extract 15 8 rhs) (extract 7 0 lhs))
  | MergeTrunc16 -> Bitvec.(concat (extract 63 16 lhs) (extract 15 0 rhs))
  | FloatAdd -> FP.(lhs + rhs)
  | FloatSub -> FP.(lhs - rhs)
  | FloatMult -> FP.(lhs * rhs)
  | FloatDiv -> FP.(lhs / rhs)
  | LongShiftLeft -> Bitvec.(lhs lsl (rhs land of_int (dword ()) 0x3F))
  | VecMulAdd16Bit -> Bitvec.dot_product32 lhs rhs

type int_shapes = [ `I8 | `I16 | `I32 | `I64 ]
type float_shapes = [ `F32 | `F64 ]

let interpret_vec_lane_bi_op :
    Instr.vec_lane_bi_op -> Types.vec_lane_shape -> expr -> expr -> expr =
 fun op shape lhs rhs ->
  let lane_size =
    match shape with
    | `I8 -> 8
    | `I16 -> 16
    | `I64 | `F64 -> 64
    | `I32 | `F32 -> 32
  in
  let lane = Bitvec.mk_sort lane_size in
  Bitvec.lanewise2 ~lane_size
    (match (shape, op) with
    | #int_shapes, VecSub -> Bitvec.( - )
    | #int_shapes, VecAdd -> Bitvec.( + )
    | #int_shapes, VecMul -> Bitvec.( * )
    | #float_shapes, VecSub -> FP.( - )
    | #float_shapes, VecAdd -> FP.( + )
    | #float_shapes, VecMul -> FP.( * )
    | _, VecEqual ->
        fun lhs rhs ->
          if_ Smt.(lhs = rhs) ~then_:(of_int lane (-1)) ~else_:(of_int lane 0)
    | _, VecNotEqual ->
        fun lhs rhs ->
          if_ Smt.(lhs = rhs) ~then_:(of_int lane 0) ~else_:(of_int lane (-1)))
    lhs rhs

let interpret_signed_bi_op : Instr.signed_bi_op -> bool -> expr -> expr -> expr
    =
 fun op signed lhs rhs ->
  match op with
  | Divide | LongDivide -> Bitvec.(if signed then lhs $/ rhs else lhs @/ rhs)
  | Remainder | LongRemainder ->
      Bitvec.(if signed then lhs $% rhs else lhs @% rhs)
  | ShiftRight | LongShiftRight ->
      Bitvec.(if signed then lhs asr rhs else lhs lsr rhs)
  | LessThan | LongLessThan ->
      Bitvec.(if signed then lhs $< rhs else lhs @< rhs)
  | LessThanEqual | LongLessThanEqual ->
      Bitvec.(if signed then lhs $<= rhs else lhs @<= rhs)
  | GreaterThan | LongGreaterThan ->
      Bitvec.(if signed then lhs $> rhs else lhs @> rhs)
  | GreaterThanEqual | LongGreaterThanEqual ->
      Bitvec.(if signed then lhs $>= rhs else lhs @>= rhs)
  | VecNarrow16Bit ->
      let work =
        Bitvec.lanewise ~lane_size:16
          Bitvec.(
            if signed then fun bv ->
              if_
                (bv $> of_int (word ()) 0x7F)
                ~then_:(of_int (byte ()) 0x7F)
                ~else_:
                  (if_
                     (bv $< of_int (word ()) (-0x80))
                     ~then_:(of_int (byte ()) (-0x80))
                     ~else_:(extract 7 0 bv))
            else fun bv ->
              if_
                (bv @> of_int (word ()) 255)
                ~then_:(of_int (byte ()) 255)
                ~else_:(extract 7 0 bv))
      in
      Bitvec.concat (work rhs) (work lhs)
  | VecNarrow32Bit ->
      let work =
        Bitvec.lanewise ~lane_size:32
          Bitvec.(
            if signed then fun bv ->
              if_
                (bv $> of_int (word ()) 0x7FFF)
                ~then_:(of_int (byte ()) 0x7FFF)
                ~else_:
                  (if_
                     (bv $< of_int (word ()) (-0x8000))
                     ~then_:(of_int (byte ()) (-0x8000))
                     ~else_:(extract 7 0 bv))
            else fun bv ->
              if_
                (bv @> of_int (word ()) 0xFFFF)
                ~then_:(of_int (byte ()) 0xFFFF)
                ~else_:(extract 7 0 bv))
      in
      Bitvec.concat (work rhs) (work lhs)

let interpret_signed_vec_lane_bi_op :
    Instr.signed_vec_lane_bi_op ->
    bool ->
    Types.vec_lane_shape ->
    expr ->
    expr ->
    expr =
 fun op signed shape lhs rhs ->
  let lane_size =
    match shape with
    | `I8 -> 8
    | `I16 -> 16
    | `I64 | `F64 -> 64
    | `I32 | `F32 -> 32
  in
  let lane = Bitvec.mk_sort lane_size in
  let max_unsigned =
    let open Bitvec in
    function
    | `I8 -> of_int (byte ()) 0xFF
    | `I16 -> of_int (word ()) 0xFFFF
    | `I32 -> of_int (dword ()) 0xFFFFFFFF
    | `I64 -> of_int' (qword ()) "18446744073709551615"
  in
  let max_signed =
    let open Bitvec in
    function
    | `I8 -> of_int (byte ()) 0x7F
    | `I16 -> of_int (word ()) 0x7FFF
    | `I32 -> of_int (dword ()) 0x7FFFFFFF
    | `I64 -> of_int' (qword ()) "9223372036954775807"
  in
  let min_signed =
    let open Bitvec in
    function
    | `I8 -> of_int (byte ()) 0x80
    | `I16 -> of_int (word ()) 0x8000
    | `I32 -> of_int (dword ()) 0x80000000
    | `I64 -> of_int' (qword ()) "-9223372036954775808"
  in
  Bitvec.lanewise2 ~lane_size
    (match (shape, signed, op) with
    | #int_shapes, true, VecMax ->
        fun l r -> if_ Bitvec.(l $> r) ~then_:l ~else_:r
    | #int_shapes, false, VecMax ->
        fun l r -> if_ Bitvec.(l @> r) ~then_:l ~else_:r
    | (#float_shapes as shape), _, VecMax ->
        fun l r -> if_ FP.(fp_of shape l > fp_of shape r) ~then_:l ~else_:r
    | #int_shapes, true, VecMin ->
        fun l r -> if_ Bitvec.(l $< r) ~then_:l ~else_:r
    | #int_shapes, false, VecMin ->
        fun l r -> if_ Bitvec.(l @< r) ~then_:l ~else_:r
    | (#float_shapes as shape), _, VecMin ->
        fun l r -> if_ FP.(fp_of shape l < fp_of shape r) ~then_:l ~else_:r
    | #int_shapes, true, VecLessThan ->
        fun l r -> Bitvec.(l $< r) |> bv_of_bool ~sort:lane
    | #int_shapes, false, VecLessThan ->
        fun l r -> Bitvec.(l @< r) |> bv_of_bool ~sort:lane
    | (#float_shapes as shape), _, VecLessThan ->
        fun l r -> FP.(fp_of shape l < fp_of shape r) |> bv_of_bool ~sort:lane
    | #int_shapes, true, VecLessThanEqual ->
        fun l r -> Bitvec.(l $<= r) |> bv_of_bool ~sort:lane
    | #int_shapes, false, VecLessThanEqual ->
        fun l r -> Bitvec.(l @<= r) |> bv_of_bool ~sort:lane
    | (#float_shapes as shape), _, VecLessThanEqual ->
        fun l r -> FP.(fp_of shape l <= fp_of shape r) |> bv_of_bool ~sort:lane
    | #int_shapes, true, VecGreaterThanEqual ->
        fun l r -> Bitvec.(l $>= r) |> bv_of_bool ~sort:lane
    | #int_shapes, false, VecGreaterThanEqual ->
        fun l r -> Bitvec.(l @>= r) |> bv_of_bool ~sort:lane
    | (#float_shapes as shape), _, VecGreaterThanEqual ->
        fun l r -> FP.(fp_of shape l >= fp_of shape r) |> bv_of_bool ~sort:lane
    | #int_shapes, true, VecGreaterThan ->
        fun l r -> Bitvec.(l $<= r) |> bv_of_bool ~sort:lane
    | #int_shapes, false, VecGreaterThan ->
        fun l r -> Bitvec.(l @<= r) |> bv_of_bool ~sort:lane
    | (#float_shapes as shape), _, VecGreaterThan ->
        fun l r -> FP.(fp_of shape l <= fp_of shape r) |> bv_of_bool ~sort:lane
    | #int_shapes, true, VecDiv ->
        fun l r -> Bitvec.(l $/ r) |> bv_of_bool ~sort:lane
    | #int_shapes, false, VecDiv ->
        fun l r -> Bitvec.(l @/ r) |> bv_of_bool ~sort:lane
    | (#float_shapes as shape), _, VecDiv ->
        fun l r -> FP.(fp_of shape l / fp_of shape r) |> bv_of_bool ~sort:lane
    | (#int_shapes as shape), false, VecAddSaturating ->
        fun l r ->
          Bitvec.(
            if_
              (add_wont_overflow ~signed:false l r)
              ~then_:(lhs + rhs) ~else_:(max_unsigned shape))
    | (#int_shapes as shape), true, VecAddSaturating ->
        fun l r ->
          Bitvec.(
            if_
              (add_wont_overflow ~signed:true l r)
              ~then_:
                (if_ (add_wont_underflow l r) ~then_:(l + r)
                   ~else_:(min_signed shape))
              ~else_:(max_signed shape))
    | #int_shapes, false, VecSubSaturating ->
        fun l r ->
          Bitvec.(
            if_
              (sub_wont_underflow ~signed:false l r)
              ~then_:(lhs - rhs) ~else_:(of_int lane 0))
    | (#int_shapes as shape), true, VecSubSaturating ->
        fun l r ->
          Bitvec.(
            if_
              (sub_wont_underflow ~signed:true l r)
              ~then_:
                (if_ (sub_wont_overflow l r) ~then_:(l - r)
                   ~else_:(max_signed shape))
              ~else_:(min_signed shape))
    | #float_shapes, _, (VecAddSaturating | VecSubSaturating) ->
        failwith "shouldn't happen")
    lhs rhs

let interpret_vec_shl shape operand count =
  Bitvec.(
    lanewise2 (fun l r -> l lsl r) ~lane_size:(lane_size shape) operand count)

let interpret_vec_shr signed shape operand count =
  Bitvec.(
    lanewise2
      (if signed then fun l r -> l asr r else fun l r -> l lsr r)
      ~lane_size:(lane_size shape) operand count)

let interpret_vec_splat shape operand =
  let operand =
    match shape with
    | #float_shapes -> FP.reinterpret_to_bv operand
    | _ -> operand
  in
  let lane_size = lane_size shape in
  Bitvec.repeat (128 / lane_size) (Bitvec.extract (lane_size - 1) 0 operand)

let interpret_vec_extract shape lane operand =
  let lane_size = lane_size shape in
  let lane_start = lane * lane_size in
  let lane_end = lane_start + lane_size - 1 in
  let value = Bitvec.extract lane_end lane_start operand in
  match shape with
  | `F32 -> FP.reinterpret_of_bv (FP.single ()) value
  | `F64 -> FP.reinterpret_of_bv (FP.double ()) value
  | _ -> value

let interpret_vec_replace shape lane src dest =
  let lane_size = lane_size shape in
  let lane_start = lane * lane_size in
  let lane_end = lane_start + lane_size - 1 in
  let lane_value =
    match shape with
    | #float_shapes -> FP.reinterpret_to_bv src
    | _ -> Bitvec.extract (lane_size - 1) 0 src
  in
  match (lane_start, 127 - lane_end) with
  | 0, _ -> Bitvec.concat (Bitvec.extract 127 (lane_end + 1) dest) lane_value
  | _, 0 -> Bitvec.concat lane_value (Bitvec.extract (lane_start - 1) 0 dest)
  | _, _ ->
      Bitvec.concat (Bitvec.extract 127 (lane_end + 1) dest)
      @@ Bitvec.concat lane_value (Bitvec.extract (lane_start - 1) 0 dest)

let interpret_vec_shuffle ctrl_high ctrl_low src1 src2 =
  let get_byte = function
    | i when i < 16 -> Bitvec.extract ((8 * i) + 7) (8 * i) src1
    | i when i < 32 -> Bitvec.extract ((8 * (i - 16)) + 7) (8 * (i - 16)) src2
    | _ -> failwith "invalid index"
  in
  let go_ctrl ctrl =
    Core.Array.init 8 ~f:(fun i ->
        Int64.((ctrl lsr Int.(8 * i)) |> to_int_trunc) land 0xFF |> get_byte)
    |> Core.Array.reduce_exn ~f:Bitvec.concat
  in
  Bitvec.concat (go_ctrl ctrl_high) (go_ctrl ctrl_low)

let interpret_vec_extend :
    bool ->
    [< `I8 | `I16 | `I32 | `I64 ] ->
    [< `LowOrder | `HighOrder ] ->
    expr ->
    expr =
 fun signed shape half_used src ->
  let half =
    match half_used with
    | `LowOrder -> Bitvec.extract 63 0 src
    | `HighOrder -> Bitvec.extract 127 64 src
  in
  let lane_size = lane_size shape in
  Bitvec.lanewise ~lane_size
    (if signed then Bitvec.sign_ext lane_size else Bitvec.zero_ext lane_size)
    half

let mem_op ctx addr ~plane ~offset =
  let mem = Map.find_exn ctx.memory plane in
  let addr =
    if offset <> 0 then Bitvec.(addr + of_int (dword ()) offset) else addr
  in
  (mem, addr)

let interpret_load_op :
    ctx -> Instr.load_op -> offset:int -> plane:int -> expr -> expr =
 fun ctx op ~offset ~plane addr ->
  let mem, addr = mem_op ctx addr ~plane ~offset in
  match op with
  | Load32 -> load_address mem 4 addr
  | FloatLoad32 -> load_address mem 4 addr |> FP.(reinterpret_of_bv (single ()))
  | FloatLoad64 -> load_address mem 8 addr |> FP.(reinterpret_of_bv (double ()))
  | LongLoad64 -> load_address mem 8 addr
  | VecLoad32ZeroExtend -> load_address mem 4 addr |> Bitvec.zero_ext 96
  | VecLoad64ZeroExtend -> load_address mem 8 addr |> Bitvec.zero_ext 64
  | VecLoad128 -> load_address mem 16 addr

let interpret_vec_load_lane =
 fun ctx shape lane ~offset ~plane addr dest_vec ->
  let mem, addr = mem_op ctx addr ~plane ~offset in
  let lane_size = lane_size shape in
  let lane_bytes = lane_size / 8 in
  interpret_vec_replace shape lane (load_address mem lane_bytes addr) dest_vec

let interpret_signed_load_op :
    ctx ->
    Instr.signed_load_op ->
    offset:int ->
    plane:int ->
    bool ->
    expr ->
    expr =
 fun ctx op ~offset ~plane signed addr ->
  let mem, addr = mem_op ctx addr ~plane ~offset in
  match op with
  | Load8 when signed -> load_address mem 1 addr |> Bitvec.sign_ext 24
  | Load8 -> load_address mem 1 addr |> Bitvec.zero_ext 24
  | Load16 when signed -> load_address mem 2 addr |> Bitvec.sign_ext 16
  | Load16 -> load_address mem 2 addr |> Bitvec.zero_ext 16

let interpret_store_op :
    ctx -> Instr.store_op -> offset:int -> plane:int -> expr -> expr -> ctx =
 fun ctx op ~offset ~plane addr value ->
  let mem, addr = mem_op ctx addr ~plane ~offset in
  let mem =
    match op with
    | Store32 | LongStore64 | VecStore128 -> store_address mem addr value
    | Store16 -> store_address mem addr (Bitvec.extract 15 0 value)
    | Store8 -> store_address mem addr (Bitvec.extract 7 0 value)
    | FloatStore32 | FloatStore64 ->
        FP.reinterpret_to_bv value |> store_address mem addr
  in
  { ctx with memory = Map.set ctx.memory ~key:plane ~data:mem }

let interpret_vec_store ctx shape lane ~offset ~plane addr value =
  let mem, addr = mem_op ctx addr ~plane ~offset in
  let mem = interpret_vec_extract shape lane value |> store_address mem addr in
  { ctx with memory = Map.set ctx.memory ~key:plane ~data:mem }

let interpret_memset ctx plane count value dest =
  let mem = Map.find_exn ctx.memory plane in
  let mem =
    Array.lambda (Bitvec.dword ())
      Bitvec.(
        fun i ->
          let offset = i - dest in
          if_ (offset @< count) ~then_:(extract 7 0 value)
            ~else_:(Array.get mem i))
  in
  { ctx with memory = Map.set ctx.memory ~key:plane ~data:mem }

let interpret_memcopy ctx plane count src dest =
  let mem = Map.find_exn ctx.memory plane in
  let mem =
    Array.lambda (Bitvec.dword ())
      Bitvec.(
        fun i ->
          let offset = i - dest in
          if_ (offset @< count)
            ~then_:(Array.get mem @@ (src + offset))
            ~else_:(Array.get mem i))
  in
  { ctx with memory = Map.set ctx.memory ~key:plane ~data:mem }

let interpret_instr : ctx -> int -> Instr.t -> ctx =
 fun ctx ix instr ->
  match instr with
  | Const (_, i) -> update_val ctx ix Bitvec.(of_int (dword ()) i)
  | FloatConst (_, f) ->
      update_val ctx ix @@ of_int' (FP.double ()) @@ Float.to_string f
  | LongConst (_, l) ->
      update_val ctx ix @@ of_int' (Bitvec.qword ()) (Int64.to_string l)
  | VecConst { upper_bits; lower_bits; _ } ->
      update_val ctx ix
      @@ Bitvec.(
           concat
             (of_int' (qword ()) @@ Int64.to_string upper_bits)
             (of_int' (qword ()) @@ Int64.to_string lower_bits))
  | DupVar { src; _ } -> update_val ctx ix @@ get_val ctx src
  | UniOp { op; operand; _ } ->
      update_val ctx ix @@ interpret_uni_op op @@ get_val ctx operand
  | BiOp { op; lhs; rhs; _ } ->
      update_val ctx ix
      @@ interpret_bi_op op (get_val ctx lhs) (get_val ctx rhs)
  | VecLaneBiOp { shape; op; lhs; rhs; _ } ->
      update_val ctx ix
      @@ interpret_vec_lane_bi_op op shape (get_val ctx lhs) (get_val ctx rhs)
  | SignedBiOp { op; signed; lhs; rhs; _ } ->
      update_val ctx ix
      @@ interpret_signed_bi_op op signed (get_val ctx lhs) (get_val ctx rhs)
  | SignedVecLaneBiOp { op; signed; shape; lhs; rhs; _ } ->
      update_val ctx ix
      @@ interpret_signed_vec_lane_bi_op op signed shape (get_val ctx lhs)
           (get_val ctx rhs)
  | VecShiftLeftOp { shape; operand; count; _ } ->
      update_val ctx ix
      @@ interpret_vec_shl shape (get_val ctx operand) (get_val ctx count)
  | VecShiftRightOp { signed; shape; operand; count; _ } ->
      update_val ctx ix
      @@ interpret_vec_shr signed shape (get_val ctx operand)
           (get_val ctx count)
  | VecSplatOp { shape; value; _ } ->
      update_val ctx ix @@ interpret_vec_splat shape (get_val ctx value)
  | VecExtractLaneOp { shape; lane; src; _ } ->
      update_val ctx ix @@ interpret_vec_extract shape lane (get_val ctx src)
  | VecReplaceLaneOp { shape; lane; lane_value; dest; _ } ->
      update_val ctx ix
      @@ interpret_vec_replace shape lane (get_val ctx lane_value)
           (get_val ctx dest)
  | VecShuffleOp { control_upper_bits; control_lower_bits; arg1; arg2; _ } ->
      update_val ctx ix
      @@ interpret_vec_shuffle control_upper_bits control_lower_bits
           (get_val ctx arg1) (get_val ctx arg2)
  | VecExtend { signed; shape; half_used; operand; _ } ->
      update_val ctx ix
      @@ interpret_vec_extend signed shape half_used (get_val ctx operand)
  | LoadOp { op; addr; offset; plane; _ } ->
      update_val ctx ix
      @@ interpret_load_op ctx op ~offset ~plane (get_val ctx addr)
  | SignedLoadOp { op; signed; addr; offset; plane; _ } ->
      update_val ctx ix
      @@ interpret_signed_load_op ctx op ~offset ~plane signed
           (get_val ctx addr)
  | VecLoadLaneOp { shape; lane; offset; plane; addr; dest_vec; _ } ->
      update_val ctx ix
      @@ interpret_vec_load_lane ctx shape lane ~offset ~plane
           (get_val ctx addr) (get_val ctx dest_vec)
  | CallOp _ -> failwith "nope"
  | CallIndirectOp _ -> failwith "nope"
  | ReturnedOp _ -> failwith "nope"
  | GetGlobalOp o -> update_val ctx ix @@ Map.find_exn ctx.vars o.global.name
  | OutsideContext _ -> failwith "nope"
  | Landmine { typ = Int; _ } ->
      update_val ctx ix @@ of_int (Bitvec.dword ()) 0xDEADBEEF
  | Landmine _ -> failwith "landmine"
  | StoreOp { op; offset; plane; addr; value } ->
      interpret_store_op ctx op ~offset ~plane (get_val ctx addr)
        (get_val ctx value)
  | VecStoreLaneOp { shape; lane; offset; plane; addr; value } ->
      interpret_vec_store ctx shape lane ~offset ~plane (get_val ctx addr)
        (get_val ctx value)
  | SetGlobalOp o ->
      {
        ctx with
        vars = Map.set ctx.vars ~key:o.global.name ~data:(get_val ctx o.value);
      }
  | Memset { count; value; dest; plane } ->
      interpret_memset ctx plane (get_val ctx count) (get_val ctx value)
        (get_val ctx dest)
  | Memcopy { count; src; dest; plane } ->
      interpret_memcopy ctx plane (get_val ctx count) (get_val ctx src)
        (get_val ctx dest)
  | AssertOp _ | Unreachable | Nop -> ctx
