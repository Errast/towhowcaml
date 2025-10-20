open! Core
open Util
module Out = Out_channel
module OA = Option_array
module AP = Array.Permissioned

type 'a alloc_location =
  | Drop
  | Stack
  | Local of 'a
  | Tee of 'a * Instr.Ref.t
  | Variable of string
[@@deriving sexp, equal]

let count_uses : Block.t -> (_, immutable) AP.t =
 fun block ->
  let arr = Array.create ~len:(AP.length block.instrs) 0 in
  AP.iter block.instrs ~f:(Instr.iter (fun (Ref i) -> arr.(i) <- arr.(i) + 1));
  (match block.terminator with
  | Branch { condition = Ref r; _ }
  | BranchReturn { condition = Ref r; _ }
  | Switch { switch_on = Ref r; _ } ->
      arr.(r) <- arr.(r) + 1
  | Goto _ | Return -> ());
  AP.of_array arr

let stack_allocate_block :
    Block.t -> (Types.local_type alloc_location, immutable) AP.t =
 fun block ->
  let num_uses = count_uses block in
  let locs =
    AP.mapi block.instrs ~f:(fun i instr ->
        match instr with
        | OutsideContext { var; _ } -> Variable var.name
        | _ when Set.mem block.roots (Ref i) ->
            Variable (Instr.assignment_var_exn instr).name
        | _ -> Drop)
  in
  let rec handle_uses user instr start_stack =
    let handle_use user (Instr.Ref index) ((stack, changes) as acc) =
      (* print_s *)
      (* [%message *)
      (* "" *)
      (* (user : Instr.Ref.t) *)
      (* (index : int) *)
      (* (stack : Instr.Ref.t list) *)
      (* (changes : (Instr.Ref.t * unit alloc_location) list)]; *)
      let[@tail_mod_cons] rec remove_from_stack should_backtrack ref = function
        | [] -> []
        | r :: stack when Instr.Ref.equal r ref ->
            should_backtrack := true;
            stack
        | r :: stack -> r :: remove_from_stack should_backtrack ref stack
      in
      let spill_to_locals should_backtrack stack =
        List.fold ~init:stack ~f:(fun stack -> function
          | _, Local _ -> stack
          | ref, (Tee _ | Stack) -> remove_from_stack should_backtrack ref stack
          | _ -> assert false)
      in
      match AP.get locs index with
      | Drop ->
          let rec go = function
            | Instr.Ref i :: stack when i = index ->
                ( stack,
                  ( Instr.Ref index,
                    if AP.get num_uses i > 1 then Tee ((), user) else Stack )
                  :: changes )
            | _ :: stack -> go stack
            | [] ->
                let should_backtrack = ref false in
                let new_stack =
                  spill_to_locals should_backtrack start_stack changes
                in
                if !should_backtrack then handle_uses user instr new_stack
                else (stack, (Ref index, Local ()) :: changes)
          in
          go stack
      | Stack -> failwith "stack value used multiple times?"
      | Variable _ | Tee _ | Local _ ->
          let should_backtrack = ref false in
          let new_stack =
            spill_to_locals should_backtrack start_stack changes
          in
          if !should_backtrack then handle_uses user instr new_stack else acc
    in
    Instr.fold_right (handle_use user) (start_stack, []) instr
  in
  let write_changes =
    List.iter ~f:(fun (Instr.Ref i, loc) -> AP.set locs i loc)
  in
  let final_stack =
    AP.foldi block.instrs ~init:[] ~f:(fun i stack instr ->
        let stack, changes = handle_uses (Ref i) instr stack in
        (* print_s *)
        (* [%message *)
        (* "" *)
        (* (i : int) *)
        (* (stack : Instr.Ref.t list) *)
        (* (changes : (Instr.Ref.t * unit alloc_location) list)]; *)
        write_changes changes;
        if AP.get num_uses i > 0 then Ref i :: stack else stack)
  in
  (match block.terminator with
  | Goto _ | Return -> ()
  | Branch { condition = c; _ }
  | BranchReturn { condition = c; _ }
  | Switch { switch_on = c; _ } ->
      handle_uses
        (Ref (AP.length block.instrs))
        (Instr.AssertOp { condition = c }) (* fun hack *)
        final_stack
      |> snd |> write_changes);
  AP.mapi locs ~f:(fun i -> function
    | Local () -> Local (Instr.value_type @@ AP.get block.instrs i)
    | Tee ((), stackuser) ->
        Tee (Instr.value_type @@ AP.get block.instrs i, stackuser)
    | Variable var -> Variable var
    | Drop -> Drop
    | Stack -> Stack)

type number_of_locals = { int : int; long : int; float : int; vec : int }
[@@deriving sexp]

let option_array_to_some_array : 'a OA.t -> ('a, [< _ perms ]) AP.t =
 fun arr ->
  for i = 0 to Option_array.length arr - 1 do
    assert (Option_array.is_some arr i)
  done;
  (* why? *)
  Option_array.copy arr |> Obj.magic

let allocate_locations :
    (Types.local_type alloc_location, _) AP.t ->
    Block.t ->
    ((Types.local_type * int) alloc_location, immutable) AP.t * number_of_locals
    =
 fun locs block ->
  let fresh_int = ref 0 in
  let free_int = ref [] in
  let fresh_long = ref 0 in
  let free_long = ref [] in
  let fresh_float = ref 0 in
  let free_float = ref [] in
  let fresh_vec = ref 0 in
  let free_vec = ref [] in
  let alloced_locs = Option_array.create ~len:(AP.length locs) in
  let get_local typ =
    let fresh, free =
      match typ with
      | Types.Int -> (fresh_int, free_int)
      | Long -> (fresh_long, free_long)
      | Float -> (fresh_float, free_float)
      | Vec -> (fresh_vec, free_vec)
    in
    match !free with
    | local :: free_list ->
        free := free_list;
        local
    | [] ->
        let local = !fresh in
        fresh := local + 1;
        local
  in

  let add_use user (Instr.Ref r) =
    if OA.is_none alloced_locs r then
      match AP.get locs r with
      | (Stack | Variable _ | Drop) as loc -> OA.set_some alloced_locs r loc
      | Local typ -> OA.set_some alloced_locs r @@ Local (typ, get_local typ)
      | Tee (typ, Ref stackuser) when user <> stackuser ->
          OA.set_some alloced_locs r @@ Tee ((typ, get_local typ), Ref stackuser)
      | Tee _ -> ()
  in
  let free_local local_id typ =
    let free_list =
      match typ with
      | Types.Int -> free_int
      | Float -> free_float
      | Long -> free_long
      | Vec -> free_vec
    in
    free_list := local_id :: !free_list
  in

  (match block.terminator with
  | Goto _ | Return -> ()
  | Branch { condition = r; _ }
  | BranchReturn { condition = r; _ }
  | Switch { switch_on = r; _ } ->
      add_use (AP.length block.instrs) r);

  for i = AP.length locs - 1 downto 0 do
    let alloced_loc =
      match AP.get locs i with
      | Drop ->
          assert (OA.is_none alloced_locs i);
          OA.set_some alloced_locs i Drop;
          Drop
      | Variable _ as loc when OA.is_none alloced_locs i ->
          OA.set_some alloced_locs i @@ loc;
          loc
      | _ -> OA.get_some_exn alloced_locs i
    in

    (match alloced_loc with
    | Drop | Stack | Variable _ -> ()
    | Local (typ, l) | Tee ((typ, l), _) -> free_local l typ);

    Instr.iter_right (add_use i) @@ AP.get block.instrs i
  done;

  ( option_array_to_some_array alloced_locs,
    {
      int = !fresh_int;
      long = !fresh_long;
      float = !fresh_float;
      vec = !fresh_vec;
    } )

let validate_stack :
    Block.t ->
    ((Types.local_type * int) alloc_location, immutable) AP.t ->
    number_of_locals ->
    unit =
 fun block locs num_locals ->
  let int_locals = Array.create ~len:num_locals.int Instr.Ref.invalid in
  let long_locals = Array.create ~len:num_locals.long Instr.Ref.invalid in
  let float_locals = Array.create ~len:num_locals.float Instr.Ref.invalid in
  let vec_locals = Array.create ~len:num_locals.vec Instr.Ref.invalid in
  let variables = Hashtbl.create (module String) in

  let get_locals = function
    | Types.Int -> int_locals
    | Types.Float -> float_locals
    | Types.Long -> long_locals
    | Types.Vec -> vec_locals
  in
  let consume_stack instr ref stack =
    match stack with
    | r :: stack when Instr.Ref.equal r ref -> stack
    | _ :: _ ->
        raise_s
          [%message
            "wrong value on stack"
              (instr : Instr.t)
              ~expected:(ref : Instr.Ref.t)
              ~found:(stack : Instr.Ref.t list)]
    | [] -> raise_s [%message "stack too small" (ref : Instr.Ref.t)]
  in
  let retrieve_local stack typ var = (get_locals typ).(var) :: stack in
  let retrieve_arg user stack (Instr.Ref arg_index as ref) =
    match AP.get locs arg_index with
    | Drop -> raise_s [%message "consumed dropped value" (ref : Instr.Ref.t)]
    | Stack -> stack
    | Local (typ, var) -> retrieve_local stack typ var
    | Tee ((typ, var), stackuser) ->
        if Instr.Ref.equal stackuser user then stack
        else retrieve_local stack typ var
    | Variable var -> Hashtbl.find_exn variables var :: stack
  in
  let store_result ref stack = function
    | Drop -> stack
    | Stack -> ref :: stack
    | Tee ((typ, var), _) ->
        (get_locals typ).(var) <- ref;
        ref :: stack
    | Local (typ, var) ->
        (get_locals typ).(var) <- ref;
        stack
    | Variable var ->
        Hashtbl.set variables ~key:var ~data:ref;
        stack
  in
  let stack =
    AP.foldi block.instrs ~init:[] ~f:(fun i stack -> function
      | OutsideContext { var; _ } ->
          Hashtbl.add_exn variables ~key:var.name ~data:(Ref i);
          stack
      | instr ->
          let stack = Instr.fold (retrieve_arg (Ref i)) stack instr in
          let stack = Instr.fold_right (consume_stack instr) stack instr in
          store_result (Ref i) stack (AP.get locs i))
  in
  let stack =
    match block.terminator with
    | Goto _ | Return -> stack
    | Branch { condition = c; _ }
    | BranchReturn { condition = c; _ }
    | Switch { switch_on = c; _ } ->
        retrieve_arg (Ref (AP.length block.instrs)) stack c
        |> consume_stack Nop c
  in
  if not @@ List.is_empty stack then
    raise_s [%message "stack not empty" (stack : Instr.Ref.t list)]

let test block =
  let types = stack_allocate_block block in
  let locs, nums = allocate_locations types block in
  print_s
  @@ [%message "" (locs : ((Types.local_type * int) alloc_location, _) iparray)];
  print_s @@ [%sexp_of: number_of_locals] nums;
  validate_stack block locs nums

let nums_max n1 n2 =
  {
    int = Int.max n1.int n2.int;
    long = Int.max n1.long n2.long;
    float = Int.max n1.float n2.float;
    vec = Int.max n1.vec n2.vec;
  }

let output_block :
    Out_channel.t -> Block.t -> (_ alloc_location, immutable) AP.t -> _ -> unit
    =
 fun out block locs expected_terminator ->
  let output_string = Out_channel.output_string out in
  let output_int i = string_of_int i |> output_string in
  let output_char = Out_channel.output_char out in
  let output_local typ id =
    output_string
      (match typ with
      | Types.Int -> "$__int_scratch_"
      | Long -> "$__long_scratch_"
      | Float -> "$__float_scratch_"
      | Vec -> "$__vec_scratch_");
    output_int id
  in
  let output_get_value user (Instr.Ref r) =
    match AP.get locs r with
    | Stack -> ()
    | Tee (_, stackuser) when Instr.Ref.equal stackuser user -> ()
    | Tee ((typ, local), _) | Local (typ, local) ->
        output_string "\n    (local.get ";
        output_local typ local;
        output_char ')'
    | Variable var ->
        output_string "\n    (global.get $";
        output_string var;
        output_char ')'
    | Drop -> failwith "cannot get dropped value"
  in
  let output_set_value (Instr.Ref r) =
    match AP.get locs r with
    | Stack -> ()
    | Drop -> output_string "\n    (drop)"
    | Tee ((typ, id), _) ->
        output_string "\n    (local.tee ";
        output_local typ id;
        output_char ')'
    | Local (typ, id) ->
        output_string "\n    (local.set ";
        output_local typ id;
        output_char ')'
    | Variable var ->
        output_string "\n    (global.set $";
        output_string var;
        output_char ')'
  in
  let output_lane_shape shape =
    output_string
    @@
    match shape with
    | `I8 -> "i8x16"
    | `I16 -> "i16x8"
    | `I32 -> "i32x4"
    | `I64 -> "i64x2"
    | `F32 -> "f32x4"
    | `F64 -> "f64x2"
  in
  let output_shape_size shape =
    output_string
      (match shape with
      | `I8 -> "8"
      | `I16 -> "16"
      | `I32 | `F32 -> "32"
      | `I64 | `F64 -> "64")
  in
  let signed_suffix = function true -> "_s" | false -> "_u" in
  let output_offset offset =
    if offset <> 0 then (
      output_string " offset ";
      string_of_int offset)
    else ""
  in
  let output_instr i instr =
    Instr.iter (output_get_value (Ref i)) instr;
    (match instr with
    | Nop | ReturnedOp _ | OutsideContext _ -> ()
    | _ ->
        output_string "\n    ";
        output_string
          (match instr with
          | Instr.Const (_, c) ->
              output_string "i32.const ";
              string_of_int c
          | FloatConst (_, c) ->
              output_string "f64.const ";
              string_of_float c
          | LongConst (_, c) ->
              output_string "i64.const ";
              Int64.to_string c
          | VecConst c ->
              output_string "vec128.const i64x2 ";
              Int64.to_string c.lower_bits |> output_string;
              Int64.to_string c.upper_bits
          | DupVar _ -> ""
          | UniOp { op; _ } -> (
              match op with
              | EqualsZero -> "i32.eqz"
              | LongEqualsZero -> "i64.eqz"
              | SignExtendLow8 -> "i32.extend8_s"
              | SignExtendHigh8 -> "i32.const 8 i32.shr_u i32.extend8_s"
              | SignExtend16 -> "i32.extend16_s"
              | ZeroExtendLow8 -> "i32.const 0xFF i32.and"
              | ZeroExtendHigh8 ->
                  "i32.const 8 i32.shr_u i32.const 0xFF i32.and"
              | ZeroExtend16 -> "i32.const 0xFFFF i32.and"
              | FloatToInt32 -> "i32.trunc_f64_s"
              | LongToInt32 -> "i32.wrap_i64"
              | CountOnes -> "i32.popcnt"
              | VecInt8SignBitmask -> "i8x16.bitmask"
              | CountLeadingZeros -> "i32.clz"
              | FloatNeg -> "f64.neg"
              | FloatAbs -> "f64.abs"
              | FloatRound -> "f64.nearest"
              | FloatTrunc -> "f64.trunc"
              | FloatSqrt -> "f64.sqrt"
              | Int32ToFloatUnsigned -> "f64.convert_i32_u"
              | Int32ToFloatSigned -> "f64.convert_i32_s"
              | Int64ToFloatSigned -> "f64.convert_i64_s"
              | Int64ToFloatUnsigned -> "f64.convert_i64_u"
              | BitcastInt64ToFloat -> "f64.reinterpret_i64"
              | FloatToLong -> "i64.trunc_f64_s"
              | Int32ToLongUnsigned -> "i64.extend_i32_u"
              | Int32ToLongSigned -> "i64.extend_i32_s"
              | BitcastFloatToLong -> "i64.reinterpret_f64"
              | LongCountLeadingZeros -> "i64.clz"
              | VecConvertLow32BitsToFloatsSigned -> "f64x2.convert_low_i32x4_s"
              )
          | BiOp { op; _ } -> (
              match op with
              | Add -> "i32.add"
              | Subtract -> "i32.subtract"
              | Multiply -> "i32.mult"
              | Equal -> "i32.eq"
              | NotEqual -> "i32.ne"
              | And -> "i32.and"
              | Or -> "i32.or"
              | Xor -> "i32.xor"
              | ShiftLeft -> "i32.shl"
              | RotateLeft -> "i32.rotl"
              | RotateRight -> "i32.rotr"
              | LongEq -> "i64.eq"
              | LongNotEq -> "i64.ne"
              | FloatEq -> "f64.eq"
              | FloatNotEq -> "f64.ne"
              | FloatGreaterThan -> "f64.gt"
              | FloatLessThan -> "f64.lt"
              | FloatGreaterThanEqual -> "f64.ge"
              | FloatLessThanEqual -> "f64.le"
              | MergeTruncLow8 ->
                  "(i32.const 0xFF) (i32.and) (local.set __instr_scratch) \
                   (i32.const 0xFFFFFF00) (i32.and) (local.get \
                   __instr_scratch) (i32.or)"
              | MergeTruncHigh8 ->
                  "(i32.const 0xFF) (i32.and) (i32.const 8) (i32.shl) \
                   (local.set __instr_scratch) (i32.const 0xFFFFFF00) \
                   (i32.and) (local.get __instr_scratch) (i32.or)"
              | MergeTrunc16 ->
                  "(i32.const 0xFFFF) (i32.and) (local.set __instr_scratch) \
                   (i32.const 0xFFFF0000) (i32.and) (local.get \
                   __instr_scratch) (i32.or)"
              | FloatAdd -> "f64.add"
              | FloatSub -> "f64.sub"
              | FloatMult -> "f64.mult"
              | FloatDiv -> "f64.div"
              | LongShiftLeft -> "i64.shl"
              | LongAdd -> "i64.add"
              | LongSub -> "i64.sub"
              | LongMultiply -> "i64.mult"
              | LongRotateLeft -> "i64.rotl"
              | LongRotateRight -> "i64.rotr"
              | LongAnd -> "i64.and"
              | LongOr -> "i64.or"
              | LongXor -> "i64.xor"
              | VecAnd -> "v128.and"
              | VecOr -> "v128.or"
              | VecXor -> "v128.xor"
              | VecMulAdd16Bit -> "i32x4.dot_i16x8_s")
          | VecLaneBiOp { op; shape; _ } -> (
              output_lane_shape shape;
              match op with
              | VecSub -> ".add"
              | VecEqual -> ".eq"
              | VecAdd -> ".add"
              | VecMul -> ".mul"
              | VecNotEqual -> ".eq")
          | SignedBiOp { op; signed; _ } ->
              (output_string
              @@
              match op with
              | Divide -> "i32.div"
              | Remainder -> "i32.rem"
              | ShiftRight -> "i32.shr"
              | LessThan -> "i32.lt"
              | LessThanEqual -> "i32.le"
              | GreaterThan -> "i32.gt"
              | GreaterThanEqual -> "i32.ge"
              | LongDivide -> "i64.div"
              | LongRemainder -> "i64.rem"
              | LongShiftRight -> "i64.shr"
              | LongLessThan -> "i64.lt"
              | LongLessThanEqual -> "i64.le"
              | LongGreaterThan -> "i64.gt"
              | LongGreaterThanEqual -> "i64.ge"
              | VecNarrow16Bit -> "i8x16.narrow_i16x8"
              | VecNarrow32Bit -> "i16x8.narrow_i32x4");
              signed_suffix signed
          | SignedVecLaneBiOp { op; signed; shape; _ } -> (
              output_lane_shape shape;
              (output_string
              @@
              match op with
              | VecMin -> ".min"
              | VecMax -> ".max"
              | VecLessThan -> ".lt"
              | VecLessThanEqual -> ".le"
              | VecGreaterThan -> ".gt"
              | VecGreaterThanEqual -> ".ge"
              | VecAddSaturating -> ".add_sat"
              | VecSubSaturating -> ".sub_sat"
              | VecDiv -> ".div");
              match shape with `F32 | `F64 -> "" | _ -> signed_suffix signed)
          | VecShiftLeftOp { shape; _ } ->
              output_lane_shape shape;
              ".shl"
          | VecShiftRightOp { shape; signed; _ } ->
              output_lane_shape shape;
              output_string ".shr";
              signed_suffix signed
          | VecSplatOp { shape; _ } ->
              output_lane_shape shape;
              "splat"
          | VecExtractLaneOp { shape; lane; _ } ->
              output_lane_shape shape;
              (match shape with
              | `I8 | `I16 -> output_string ".extract_lane_u "
              | _ -> output_string ".extract_lane ");
              string_of_int lane
          | VecReplaceLaneOp { shape; lane; _ } ->
              output_lane_shape shape;
              (match shape with
              | `I8 | `I16 -> output_string ".replace_lane_u "
              | _ -> output_string ".replace_lane ");
              string_of_int lane
          | VecShuffleOp { control_lower_bits; control_upper_bits; _ } ->
              output_string "i8x16.shuffle";
              let rec output_bytes i n =
                if n > 0 then (
                  let byte = Int64.to_int_trunc i land 0xFF in

                  output_char ' ';
                  output_int byte;
                  output_bytes (Int64.shift_right_logical i 8) (n - 1))
              in
              output_bytes control_lower_bits 8;
              output_bytes control_upper_bits 8;
              ""
          | VecExtend { shape; signed; half_used; _ } ->
              output_lane_shape
                (match[@warning "-8"] shape with
                | `I8 -> `I16
                | `I16 -> `I32
                | `I32 -> `I64);
              output_string ".extend_";
              output_string
                (match half_used with
                | `HighOrder -> "high_"
                | `LowOrder -> "low_");
              output_lane_shape shape;
              signed_suffix signed
          | LoadOp { op; offset; _ } -> (
              let basic str =
                output_string str;
                output_offset offset
              in
              match op with
              | Load32 -> basic "i32.load"
              | FloatLoad32 ->
                  output_string "(f32.load";
                  output_string @@ output_offset offset;
                  ") (f64.promote_f32)"
              | FloatLoad64 -> basic "f64.load"
              | LongLoad64 -> basic "i64.load"
              | VecLoad32ZeroExtend -> basic "v128.load32_zero"
              | VecLoad64ZeroExtend -> basic "v128.load64_zero"
              | VecLoad128 -> basic "v128.load")
          | SignedLoadOp { op; offset; signed; _ } ->
              output_string
                (match op with Load8 -> "i32.load8" | Load16 -> "i32.load16");
              output_string @@ signed_suffix signed;
              output_offset offset
          | VecLoadLaneOp { shape; lane; offset; _ } ->
              output_string "v128.load";
              output_shape_size shape;
              output_string "_lane";
              if offset <> 0 then (
                output_string " offset ";
                output_int offset);
              output_char ' ';
              string_of_int lane
          | CallOp { func; _ } ->
              output_string "call $";
              func
          (* *)
          | CallIndirectOp { args; _ } ->
              let args =
                List.map
                  ~f:(fun (Ref i) -> AP.get block.instrs i |> Instr.value_type)
                  args
              in
              let returns =
                let rec go i rs =
                  match AP.get block.instrs i with
                  | ReturnedOp { typ; _ } ->
                      if i + 1 < AP.length block.instrs then
                        go (i + 1) (typ :: rs)
                      else typ :: rs
                  | _ -> rs
                in
                go (i + 1) [] |> List.rev
              in
              let to_char = function
                | Types.Int -> output_char 'i'
                | Long -> output_char 'l'
                | Float -> output_char 'f'
                | Vec -> output_char 'v'
              in
              output_string "call_indirect (type $__FUNC";
              List.iter args ~f:to_char;
              output_char '_';
              List.iter returns ~f:to_char;
              ")"
          | GetGlobalOp { global; _ } ->
              output_string "global.get $";
              global.name
          | Landmine { typ = Int; _ } -> "i32.const 0xDEADBEEF"
          | Landmine _ -> failwith "landmine"
          | StoreOp { op; offset; _ } ->
              output_string
                (match op with
                | Store32 -> "i32.store"
                | Store16 -> "i32.store16"
                | Store8 -> "i32.store8"
                | FloatStore32 -> "(f32.demote_f64) f32.store"
                | FloatStore64 -> "f64.store"
                | LongStore64 -> "i64.store"
                | VecStore128 -> "v128.store");
              output_offset offset
          | VecStoreLaneOp { shape; lane; offset; _ } ->
              output_string "v128.store";
              output_shape_size shape;
              output_string "_lane";
              if offset <> 0 then (
                output_string " offset ";
                output_int offset);
              output_char ' ';
              string_of_int lane
          | SetGlobalOp { global; _ } ->
              output_string "global.set $";
              global.name
          | AssertOp _ -> "(if (result) (then) (else unreachable))"
          | Memset _ -> "memory.fill"
          | Memcopy _ -> "memory.copy"
          | Unreachable -> "unreachable"
          | Nop | ReturnedOp _ | OutsideContext _ -> ""));
    match instr with
    | OutsideContext _ -> ()
    | _ when not @@ Instr.is_assignment instr -> ()
    | _ -> output_set_value (Ref i)
  in

  AP.iteri block.instrs ~f:output_instr;

  match (block.terminator, expected_terminator) with
  | _, `Any | Goto _, `Goto | Return, `Return -> ()
  | (Branch { condition; _ } | BranchReturn { condition; _ }), `Branch
  | Switch { switch_on = condition; _ }, `Switch ->
      output_get_value (Ref (AP.length block.instrs)) condition
  | _ -> failwith "unexpected terminator"

let run out func =
  let structured = Structure_cfg.structure_cfg func in
  let num_locals, locals =
    AP.fold_map func.blocks ~init:{ int = 0; long = 0; float = 0; vec = 0 }
      ~f:(fun max_nums block ->
        let locs, nums =
          allocate_locations (stack_allocate_block block) block
        in
        (nums_max max_nums nums, locs))
  in
  let output_string = Out_channel.output_string out in
  let output_int i = string_of_int i |> output_string in
  let output_char = Out_channel.output_char out in
  let output_block id expected =
    output_block out (AP.get func.blocks id) (AP.get locals id) expected
  in
  let rec output_tree : Structure_cfg.wasm_control -> unit = function
    | WasmBlock body ->
        output_string "\n    (block";
        output_tree body;
        output_char ')'
    | WasmLoop body ->
        output_string "\n    (loop";
        output_tree body;
        output_char ')'
    | WasmIf (block, if_true, if_false) ->
        output_block block `Branch;
        output_string "\n    (if (then ";
        output_tree if_true;
        output_string ")\n    (else";
        output_tree if_false;
        output_char ')'
    | WasmCodeReturn block ->
        output_block block `Return;
        output_string "\n    (return)"
    | WasmReturn -> output_string "\n    (return)"
    | WasmBr label ->
        output_string "\n    (br ";
        output_int label;
        output_char ')'
    | WasmBrTable { bb_id; targets; default } ->
        output_block bb_id `Switch;
        output_string "\n    (br_table ";
        List.iter targets ~f:(fun i ->
            output_int i;
            output_char ' ');
        output_int default;
        output_char ')'
    | WasmSeq (b1, b2) ->
        output_tree b1;
        output_tree b2
    | WasmCode block -> output_block block `Goto
    | WasmFallthrough -> ()
  in
  output_string "\n  (func $";
  output_string func.name;
  output_string "(param";
  let param_types = function
    | { Types.typ = Int; _ } -> " i32"
    | { typ = Long; _ } -> " i64"
    | { typ = Float; _ } -> " f64"
    | { typ = Vec; _ } -> " v128"
  in
  List.iter func.signature.args ~f:(fun t -> param_types t |> output_string);
  output_string ") (result";
  List.iter func.signature.returns ~f:(fun t -> param_types t |> output_string);
  output_string ")\n ";
  let make_local name num typ =
    for i = 0 to num - 1 do
      output_string " (local $__";
      output_string name;
      output_string "_scratch_";
      output_int i;
      output_string typ;
      output_char ')'
    done
  in
  make_local "int" num_locals.int "i32";
  make_local "long" num_locals.long "i64";
  make_local "float" num_locals.float "f64";
  make_local "vec" num_locals.vec "v128";
  output_tree structured;
  output_string "  )\n"

let run_block : Out_channel.t -> Block.t -> unit =
 fun out block ->
  let locs, _ = allocate_locations (stack_allocate_block block) block in
  (* print_s *)
  (* [%message "" (locs : ((Types.local_type * int) alloc_location, _) iparray)]; *)
  output_block out block locs `Any
