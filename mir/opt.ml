open! Core
open Instr
module AP = Array.Permissioned
module A = Array

type context = {
  instrs : Instr.t array;
  uses : int array;
  roots : Set.M(Instr.Ref).t;
}
[@@deriving sexp_of]

let unref (Ref i) = i
let decref uses (Ref i) = uses.(i) <- uses.(i) - 1
let incref uses (Ref i) = uses.(i) <- uses.(i) + 1

let count_uses instrs roots =
  let use_map = Array.create ~len:(Array.length instrs) 0 in
  Array.iter instrs ~f:(Instr.iter (incref use_map));
  Set.iter roots ~f:(incref use_map);
  use_map

let update c (Ref r) instr =
  Instr.iter (decref c.uses) c.instrs.(r);
  Instr.iter (incref c.uses) instr;
  c.instrs.(r) <- instr
(* Vec.add worklist r *)

exception Escape

let peephole_const c instr =
  let const (Ref r) =
    match c.instrs.(r) with Const (_, v) -> v | _ -> raise_notrace Escape
  in
  let mk var value = (Const (var, value land 0xFFFFFFFF), []) in
  let int_of_bool b = if b then 1 else 0 in
  try
    match instr with
    | UniOp { op; operand; var } -> (
        let mk = mk var in
        match op with
        | EqualsZero -> mk (const operand = 0 |> int_of_bool)
        | SignExtendLow8 -> mk @@ Bits.sign_extend_8 @@ const operand
        | SignExtendHigh8 -> mk @@ Bits.sign_extend_8 (const operand lsr 8)
        | SignExtend16 -> mk @@ Bits.sign_extend_16 @@ const operand
        | ZeroExtendLow8 -> mk @@ (const operand land 0xFF)
        | ZeroExtendHigh8 -> mk @@ ((const operand lsr 8) land 0xFF)
        | ZeroExtend16 -> mk @@ (const operand land 0xFFFF)
        | _ -> (instr, []))
    | BiOp { op; lhs; rhs; var } -> (
        let mk = mk var in
        match op with
        | Add -> mk (const lhs + const rhs)
        | Subtract -> mk (const lhs - const rhs)
        | Multiply -> mk (const lhs * const rhs)
        | Equal -> mk (const lhs = const rhs |> int_of_bool)
        | NotEqual -> mk (const lhs <> const rhs |> int_of_bool)
        | And -> mk (const lhs land const rhs)
        | Or -> mk (const lhs lor const rhs)
        | Xor -> mk (const lhs lxor const rhs)
        | ShiftLeft -> mk @@ (const lhs lsl (const rhs land 0x1F))
        | RotateLeft ->
            mk
              (let lhs = const lhs and rhs = const rhs land 0x1F in
               (lhs lsl rhs) lor (lhs lsr (32 - rhs)))
        | RotateRight ->
            mk
              (let lhs = const lhs and rhs = const rhs land 0x1F in
               (lhs lsr rhs) lor (lhs lsl (32 - rhs)))
        | MergeTruncLow8 ->
            mk @@ (const lhs land 0xFF lor (const rhs land 0xFFFFFF00))
        | MergeTruncHigh8 ->
            mk @@ (((const lhs land 0xFF) lsl 8) lor (const rhs land 0xFFFFFF00))
        | MergeTrunc16 ->
            mk @@ (const lhs land 0xFFFF lor (const rhs land 0xFFFF0000))
        | _ -> (instr, []))
    | SignedBiOp { op; lhs; rhs; var; signed = s } -> (
        let mk = mk var in
        let const_s r = if s then const r |> Bits.sign_extend else const r in
        match op with
        | Divide ->
            mk
            @@
            (* having separate functions for all this is dumb *)
            if s then Bits.signed_divide_32 (const lhs) (const rhs)
            else Bits.unsigned_divide_32 (const lhs) (const rhs)
        | Remainder ->
            mk
            @@
            if s then Bits.signed_remainder_32 (const lhs) (const rhs)
            else Bits.unsigned_remainder_32 (const lhs) (const rhs)
        | ShiftRight ->
            mk
            @@
            if s then Bits.signed_shift_right_32 (const lhs) (const rhs)
            else Bits.unsigned_shift_right_32 (const lhs) (const rhs)
        | LessThan -> const_s lhs < const_s rhs |> int_of_bool |> mk
        | LessThanEqual -> const_s lhs <= const_s rhs |> int_of_bool |> mk
        | GreaterThan -> const_s lhs > const_s rhs |> int_of_bool |> mk
        | GreaterThanEqual -> const_s lhs >= const_s rhs |> int_of_bool |> mk
        | _ -> (instr, []))
    | AssertOp { condition } ->
        if const condition <> 0 then (Nop, [])
        else raise_s [%message "always-fail assert" (instr : Instr.t)]
    | _ -> (instr, [])
  with Escape -> (instr, [])

let peephole_opts c i =
  match i with
  | UniOp ({ op = SignExtendLow8 | ZeroExtendLow8; operand; _ } as v) -> (
      match c.instrs.(unref operand) with
      | UniOp { op = (SignExtendLow8 | ZeroExtendLow8) as op; operand; _ } ->
          if Poly.(v.op = op) then
            (DupVar { var = v.var; src = v.operand; typ = Int }, [])
          else (UniOp { v with operand }, [])
      | BiOp { op = MergeTruncLow8 | MergeTrunc16; lhs; _ } ->
          (UniOp { v with operand = lhs }, [])
      | BiOp { op = MergeTruncHigh8; rhs; _ } ->
          (UniOp { v with operand = rhs }, [])
      | SignedLoadOp { op = Load8; signed; _ }
        when equal_bool signed Poly.(v.op = SignExtendLow8) ->
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | LoadOp l when c.uses.(unref operand) = 1 ->
          (* fine because it has the same instrrefs and can't be optimized further *)
          c.instrs.(unref operand) <-
            SignedLoadOp
              {
                op = Load8;
                signed = Poly.(v.op = SignExtendLow8);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | SignedLoadOp l when c.uses.(unref operand) = 1 ->
          c.instrs.(unref operand) <-
            SignedLoadOp
              {
                op = Load8;
                signed = Poly.(v.op = SignExtendLow8);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | _ -> peephole_const c i)
  | UniOp ({ op = SignExtendHigh8 | ZeroExtendHigh8; operand; _ } as v) -> (
      match c.instrs.(unref operand) with
      | UniOp { op = (SignExtendLow8 | ZeroExtendHigh8) as op; operand; _ } ->
          if Poly.(v.op = op) then
            (DupVar { var = v.var; src = v.operand; typ = Int }, [])
          else (UniOp { v with operand }, [])
      | BiOp { op = MergeTruncHigh8; lhs; _ } ->
          let op =
            Poly.(
              if v.op = SignExtendHigh8 then SignExtendLow8 else ZeroExtendLow8)
          in
          (UniOp { v with op; operand = lhs }, [])
      | BiOp { op = MergeTrunc16; lhs; _ } ->
          (UniOp { v with operand = lhs }, [])
      | BiOp { op = MergeTruncLow8; rhs; _ } ->
          (UniOp { v with operand = rhs }, [])
      | _ -> peephole_const c i)
  | UniOp ({ op = SignExtend16 | ZeroExtend16; operand; _ } as v) -> (
      match c.instrs.(unref operand) with
      | UniOp { op = (SignExtend16 | ZeroExtend16) as op; operand; _ } ->
          if Poly.(v.op = op) then
            (DupVar { var = v.var; src = v.operand; typ = Int }, [])
          else (UniOp { v with operand }, [])
      | BiOp { op = MergeTrunc16; rhs; _ } ->
          (UniOp { v with operand = rhs }, [])
      | SignedLoadOp { op = Load16; signed; _ }
        when equal_bool signed Poly.(v.op = SignExtend16) ->
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | LoadOp l when c.uses.(unref operand) = 1 ->
          c.instrs.(unref operand) <-
            SignedLoadOp
              {
                op = Load8;
                signed = Poly.(v.op = SignExtendLow8);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | SignedLoadOp l when c.uses.(unref operand) = 1 ->
          c.instrs.(unref operand) <-
            SignedLoadOp
              {
                op = Load16;
                signed = Poly.(v.op = SignExtend16);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          (DupVar { var = v.var; typ = Int; src = operand }, [])
      | _ -> peephole_const c i)
  | BiOp ({ op = MergeTruncLow8; lhs = small; rhs = big; _ } as v) -> (
      match (c.instrs.(unref small), c.instrs.(unref big)) with
      | _, BiOp { op = MergeTruncLow8; rhs = big; _ } ->
          (BiOp { v with rhs = big }, [])
      | ( ( BiOp { op = MergeTruncLow8; lhs = small; _ }
          | UniOp { op = SignExtendLow8 | ZeroExtendLow8; operand = small; _ }
            ),
          _ ) ->
          (BiOp { v with lhs = small }, [])
      | ( (BiOp { op = Add | ShiftLeft | Subtract | And | Or | Xor; _ } as instr),
          _ )
        when c.uses.(unref small) = 1 ->
          update c small
          @@ Instr.map
               (fun r ->
                 match c.instrs.(unref r) with
                 | BiOp { op = MergeTruncLow8; lhs = small; _ }
                 | UniOp
                     {
                       op = SignExtendLow8 | ZeroExtendLow8;
                       operand = small;
                       _;
                     } ->
                     small
                 | _ -> r)
               instr;
          (i, [ small ])
      | _ -> peephole_const c i)
  | BiOp ({ op = MergeTruncHigh8; lhs = small; rhs = big; _ } as v) -> (
      match (c.instrs.(unref small), c.instrs.(unref big)) with
      | _, BiOp { op = MergeTruncHigh8; rhs = big; _ } ->
          (BiOp { v with rhs = big }, [])
      | BiOp { op = MergeTruncHigh8; lhs; _ }, _ -> (BiOp { v with lhs }, [])
      | _ -> peephole_const c i)
  | BiOp ({ op = MergeTrunc16; lhs = small; rhs = big; _ } as v) -> (
      match (c.instrs.(unref small), c.instrs.(unref big)) with
      | ( _,
          BiOp
            {
              op = MergeTruncLow8 | MergeTruncHigh8 | MergeTrunc16;
              rhs = big;
              _;
            } ) ->
          (BiOp { v with rhs = big }, [])
      | ( ( BiOp { op = MergeTrunc16; lhs = small; _ }
          | UniOp { op = SignExtend16 | ZeroExtend16; operand = small; _ } ),
          _ ) ->
          (BiOp { v with lhs = small }, [])
      | ( (BiOp { op = Add | ShiftLeft | Subtract | And | Or | Xor; _ } as instr),
          _ )
        when c.uses.(unref small) = 1 ->
          update c small
          @@ Instr.map
               (fun r ->
                 match c.instrs.(unref r) with
                 | BiOp { op = MergeTrunc16; lhs = small; _ }
                 | UniOp
                     { op = SignExtend16 | ZeroExtend16; operand = small; _ } ->
                     small
                 | _ -> r)
               instr;
          (i, [ small ])
      | _ -> peephole_const c i)
  | StoreOp ({ op = Store8; value; _ } as v) -> (
      match c.instrs.(unref value) with
      | UniOp
          {
            op = ZeroExtendLow8 | SignExtendLow8 | ZeroExtend16 | SignExtend16;
            operand;
            _;
          } ->
          (StoreOp { v with value = operand }, [])
      | BiOp { op = MergeTruncLow8 | MergeTrunc16; lhs; _ } ->
          (StoreOp { v with value = lhs }, [])
      | _ -> peephole_const c i)
  | StoreOp ({ op = Store16; value; _ } as v) -> (
      match c.instrs.(unref value) with
      | UniOp { op = ZeroExtend16 | SignExtend16; operand; _ } ->
          (StoreOp { v with value = operand }, [])
      | BiOp { op = MergeTrunc16; lhs; _ } ->
          (StoreOp { v with value = lhs }, [])
      | _ -> peephole_const c i)
  | VecReplaceLaneOp ({ dest; lane; shape = `I64 | `F64; _ } as r) -> (
      match c.instrs.(unref dest) with
      | ( BiOp { op = VecAnd | VecOr | VecXor | VecMulAdd16Bit; _ }
        | VecLaneBiOp _ | SignedVecLaneBiOp _ | VecShiftLeftOp _
        | VecShiftRightOp _ ) as biop
        when c.uses.(unref dest) = 1 ->
          update c dest
          @@ Instr.map
               (fun (Ref r) ->
                 match c.instrs.(r) with
                 | VecReplaceLaneOp ({ shape = `I64 | `F64; dest; _ } as v)
                   when v.lane = lane ->
                     dest
                 | _ -> Ref r)
               biop;
          (i, [])
      | _ -> (
          match c.instrs.(unref r.lane_value) with
          | VecExtractLaneOp ({ shape = `I64 | `F64; _ } as e) ->
              let ctrl_h, ctrl_l =
                match (r.lane, e.lane) with
                | 0, 0 ->
                    (0x0F_0E_0D_0C_0B_0A_09_08L, 0x17_16_15_14_13_12_11_10L)
                | _, 0 ->
                    (0x17_16_15_14_13_12_11_10L, 0x07_06_05_04_03_02_01_00L)
                | 0, _ ->
                    (0x0F_0E_0D_0C_0B_0A_09_08L, 0x1F_1E_1D_1C_1B_1A_19_18L)
                | _, _ ->
                    (0x1F_1E_1D_1C_1B_1A_19_18L, 0x07_06_05_04_03_02_01_00L)
              in
              ( VecShuffleOp
                  {
                    var = r.var;
                    arg1 = r.dest;
                    arg2 = e.src;
                    control_upper_bits = ctrl_h;
                    control_lower_bits = ctrl_l;
                  },
                [] )
          | _ -> peephole_const c i))
  | VecExtractLaneOp ({ src; lane; shape = `I64 | `F64; _ } as e) -> (
      match c.instrs.(unref src) with
      | ( BiOp { op = VecAnd | VecOr | VecXor | VecMulAdd16Bit; _ }
        | VecLaneBiOp _ | SignedVecLaneBiOp _ | VecShiftLeftOp _
        | VecShiftRightOp _ ) as biop
        when c.uses.(unref src) = 1 ->
          update c src
          @@ Instr.map
               (fun (Ref r) ->
                 match c.instrs.(r) with
                 | VecReplaceLaneOp ({ shape = `I64 | `F64; dest; _ } as v)
                   when v.lane <> lane ->
                     dest
                 | _ -> Ref r)
               biop;
          (i, [])
      | VecReplaceLaneOp
          { shape = (`I64 | `F64) as shape; lane = lane2; dest; lane_value; _ }
        ->
          if lane2 = lane then
            ( (if Poly.(shape = e.shape) then
                 DupVar
                   {
                     var = e.var;
                     typ = Mir_builder.vec_lane_type shape;
                     src = lane_value;
                   }
               else i),
              [] )
          else (VecExtractLaneOp { e with src = dest }, [])
      | _ -> peephole_const c i)
  | _ -> peephole_const c i

let peephole_dup c instr =
  let instrs = c.instrs in
  let newInstr =
    Instr.map
      (fun (Ref i) ->
        match instrs.(i) with DupVar { src; _ } -> src | _ -> Ref i)
      instr
  in
  if phys_equal newInstr instr then peephole_opts c instr else (newInstr, [])

let peephole c =
  let instrs = c.instrs in
  let length = A.length instrs in
  let worklist = List.init length ~f:(fun i -> Ref i) in
  let rec go = function
    | Ref i :: worklist ->
        let oldInstr = instrs.(i) in
        let newInstr, wl = peephole_dup c oldInstr in
        if not @@ phys_equal oldInstr newInstr then (
          update c (Ref i) newInstr;
          go @@ wl @ (Ref i :: worklist));
        go @@ wl @ worklist
    | _ -> ()
  in
  go worklist

let dead_instrs c =
  let uses = c.uses in
  for i = 0 to A.length c.instrs - 1 do
    let instr = A.unsafe_get c.instrs i in
    if uses.(i) = 0 then (
      Instr.iter (fun (Ref r) -> uses.(r) <- uses.(r) - 1) instr;
      c.instrs.(i) <- Nop)
  done

let opt (block : Block.t) =
  let instrs = AP.to_array block.instrs in
  let c =
    { instrs; uses = count_uses instrs block.roots; roots = block.roots }
  in
  (* print_s @@ sexp_of_array sexp_of_int c.uses; *)
  peephole c;
  assert (equal_array equal_int c.uses @@ count_uses instrs block.roots);
  dead_instrs c;
  assert (equal_array equal_int c.uses @@ count_uses instrs block.roots);
  { block with instrs = AP.of_array instrs }
