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

let count_uses instrs roots terminator =
  let use_map = Array.create ~len:(Array.length instrs) 0 in
  Array.iteri instrs ~f:(fun i instr ->
      Instr.iter (incref use_map) instr;
      if not @@ is_pure instr then use_map.(i) <- use_map.(i) + 1);
  Set.iter roots ~f:(incref use_map);
  (match terminator with
  | Block.Goto _ | Return -> ()
  | Branch { condition = i; _ }
  | BranchReturn { condition = i; _ }
  | Switch { switch_on = i; _ } ->
      incref use_map i);
  use_map

let wl_append wl a = wl := a :: !wl

let update c wl (Ref r) instr =
  if not @@ phys_equal c.instrs.(r) instr then (
    Instr.iter (decref c.uses) c.instrs.(r);
    Instr.iter (incref c.uses) instr;
    c.instrs.(r) <- instr;
    wl_append wl @@ Ref r)

(* Vec.add worklist r *)

exception Escape

let peephole_const c instr =
  let const (Ref r) =
    match c.instrs.(r) with Const (_, v) -> v | _ -> raise_notrace Escape
  in
  let mk var value = Const (var, value land 0xFFFFFFFF) in
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
        | _ -> instr)
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
        | _ -> instr)
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
        | _ -> instr)
    | _ -> instr
  with Escape -> instr

let rec demand_low8 c wl i =
  match c.instrs.(unref i) with
  | UniOp
      {
        op = SignExtendLow8 | SignExtend16 | ZeroExtendLow8 | ZeroExtend16;
        operand;
        _;
      } ->
      demand_low8 c wl operand
  | UniOp { op = SignExtendHigh8 | ZeroExtendHigh8; operand; _ } ->
      demand_high8 c wl operand
  | BiOp { op = MergeTruncLow8 | MergeTrunc16; lhs; _ } -> demand_low8 c wl lhs
  | BiOp { op = MergeTruncHigh8; rhs; _ } -> demand_low8 c wl rhs
  | BiOp { op = Add | ShiftLeft | Subtract | And | Or | Xor; _ } as instr
    when c.uses.(unref i) = 1 ->
      update c wl i @@ Instr.map (demand_low8 c wl) instr;
      i
  | LoadOp { op = Load32; addr; offset; var } when c.uses.(unref i) = 1 ->
      update c wl i
      @@ SignedLoadOp { var; offset; addr; op = Load8; signed = true };
      i
  | SignedLoadOp ({ op = Load16; _ } as l) when c.uses.(unref i) = 1 ->
      update c wl i @@ SignedLoadOp { l with op = Load8 };
      i
  | _ -> i

and demand_high8 c wl i =
  match c.instrs.(unref i) with
  | UniOp { op = SignExtend16 | ZeroExtend16; operand; _ } ->
      demand_high8 c wl operand
  | BiOp { op = MergeTruncHigh8; lhs; _ } -> demand_low8 c wl lhs
  | BiOp { op = MergeTrunc16 | MergeTruncLow8; rhs; _ } -> demand_high8 c wl rhs
  | _ -> i

and demand_16 c wl i =
  match c.instrs.(unref i) with
  | UniOp { op = SignExtend16 | ZeroExtend16; operand; _ } ->
      demand_16 c wl operand
  | BiOp { op = MergeTrunc16; lhs; _ } -> demand_16 c wl lhs
  | BiOp { op = Add | ShiftLeft | Subtract | And | Or | Xor; _ } as instr
    when c.uses.(unref i) = 1 ->
      update c wl i @@ Instr.map (demand_16 c wl) instr;
      i
  | LoadOp { op = Load32; addr; offset; var } when c.uses.(unref i) = 1 ->
      update c wl i
      @@ SignedLoadOp { var; offset; addr; op = Load16; signed = true };
      i
  | _ -> i

let rec demand_high64 c wl i =
  match c.instrs.(unref i) with
  | VecReplaceLaneOp { shape = `I64 | `F64; lane; dest; lane_value; _ } -> (
      if lane = 0 then demand_high64 c wl dest
      else
        match c.instrs.(unref lane_value) with
        | VecExtractLaneOp { shape = `I64 | `F64; lane; src; _ } ->
            if lane = 0 then demand_low64 c wl src else demand_high64 c wl src
        | _ -> i)
  | ( BiOp { op = VecAnd | VecOr | VecXor | VecMulAdd16Bit; _ }
    | VecLaneBiOp _ | SignedVecLaneBiOp _ | VecShiftLeftOp _ | VecShiftRightOp _
      ) as instr
    when c.uses.(unref i) = 1 ->
      update c wl i @@ Instr.map (demand_high64 c wl) instr;
      i
  | _ -> i

and demand_low64 c wl i =
  match c.instrs.(unref i) with
  | VecReplaceLaneOp { shape = `I64 | `F64; lane; dest; lane_value; _ } -> (
      if lane = 1 then demand_low64 c wl dest
      else
        match c.instrs.(unref lane_value) with
        | VecExtractLaneOp { shape = `I64 | `F64; lane; src; _ } ->
            if lane = 0 then demand_low64 c wl src else demand_high64 c wl src
        | _ -> i)
  | ( BiOp { op = VecAnd | VecOr | VecXor | VecMulAdd16Bit; _ }
    | VecLaneBiOp _ | SignedVecLaneBiOp _ | VecShiftLeftOp _ | VecShiftRightOp _
      ) as instr
    when c.uses.(unref i) = 1 ->
      update c wl i @@ Instr.map (demand_low64 c wl) instr;
      i
  | VecLoadLaneOp { shape = `I64 | `F64; lane = 1; dest_vec; _ } ->
      demand_low64 c wl dest_vec
  | VecLoadLaneOp { shape = `I64 | `F64; lane = 0; addr; offset; var; _ }
    when c.uses.(unref i) = 1 ->
      update c wl i @@ LoadOp { op = VecLoad64ZeroExtend; addr; offset; var };
      i
  | _ -> i

let peephole_opts c wl i =
  match i with
  (* TODO abstract this logic into demand_low8_bits, etc. funcs *)
  | UniOp ({ op = SignExtendLow8 | ZeroExtendLow8; _ } as v) -> (
      let new_operand = demand_low8 c wl v.operand in
      match c.instrs.(unref new_operand) with
      | UniOp { op = (SignExtendLow8 | ZeroExtendLow8) as op; operand; _ } ->
          if Poly.(v.op = op) then
            DupVar { var = v.var; src = v.operand; typ = Int }
          else UniOp { v with operand }
      | SignedLoadOp { op = Load8; signed; _ }
        when equal_bool signed Poly.(v.op = SignExtendLow8) ->
          DupVar { var = v.var; typ = Int; src = new_operand }
      | SignedLoadOp l when c.uses.(unref new_operand) = 1 ->
          c.instrs.(unref new_operand) <-
            SignedLoadOp
              {
                op = Load8;
                signed = Poly.(v.op = SignExtendLow8);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          DupVar { var = v.var; typ = Int; src = new_operand }
      | _ ->
          if Ref.equal new_operand v.operand then peephole_const c i
          else UniOp { v with operand = new_operand })
  | UniOp ({ op = SignExtendHigh8 | ZeroExtendHigh8; _ } as v) -> (
      let new_operand = demand_high8 c wl v.operand in
      match c.instrs.(unref new_operand) with
      | UniOp { op = (SignExtendLow8 | ZeroExtendHigh8) as op; operand; _ } ->
          if Poly.(v.op = op) then
            DupVar { var = v.var; src = v.operand; typ = Int }
          else UniOp { v with operand }
      | _ ->
          if Ref.equal v.operand new_operand then peephole_const c i
          else UniOp { v with operand = new_operand })
  | UniOp ({ op = SignExtend16 | ZeroExtend16; _ } as v) -> (
      let new_operand = demand_16 c wl v.operand in
      match c.instrs.(unref new_operand) with
      | UniOp { op = (SignExtend16 | ZeroExtend16) as op; operand; _ } ->
          if Poly.(v.op = op) then
            DupVar { var = v.var; src = v.operand; typ = Int }
          else UniOp { v with operand }
      | SignedLoadOp { op = Load16; signed; _ }
        when equal_bool signed Poly.(v.op = SignExtend16) ->
          DupVar { var = v.var; typ = Int; src = new_operand }
      | SignedLoadOp l when c.uses.(unref new_operand) = 1 ->
          c.instrs.(unref new_operand) <-
            SignedLoadOp
              {
                op = Load16;
                signed = Poly.(v.op = SignExtend16);
                addr = l.addr;
                offset = l.offset;
                var = l.var;
              };
          DupVar { var = v.var; typ = Int; src = new_operand }
      | _ -> peephole_const c i)
  | BiOp ({ op = MergeTruncLow8; rhs = big; _ } as v) -> (
      let new_small = demand_low8 c wl v.lhs in
      match c.instrs.(unref big) with
      | BiOp { op = MergeTruncLow8; rhs = big; _ } ->
          BiOp { v with rhs = big; lhs = new_small }
      | _ ->
          if Ref.equal new_small v.lhs then peephole_const c i
          else BiOp { v with lhs = new_small })
  | BiOp ({ op = MergeTruncHigh8; rhs = big; _ } as v) -> (
      let new_small = demand_high8 c wl v.lhs in
      match c.instrs.(unref big) with
      | BiOp { op = MergeTruncHigh8; rhs = big; _ } ->
          BiOp { v with rhs = big; lhs = new_small }
      | _ ->
          if Ref.equal new_small v.lhs then peephole_const c i
          else BiOp { v with lhs = new_small })
  | BiOp ({ op = MergeTrunc16; rhs = big; _ } as v) -> (
      let new_small = demand_16 c wl v.lhs in
      match c.instrs.(unref big) with
      | BiOp
          { op = MergeTruncLow8 | MergeTruncHigh8 | MergeTrunc16; rhs = big; _ }
        ->
          BiOp { v with rhs = big; lhs = new_small }
      | _ ->
          if Ref.equal v.lhs new_small then peephole_const c i
          else BiOp { v with lhs = new_small })
  | StoreOp ({ op = Store8; value; _ } as v) ->
      let new_val = demand_low8 c wl value in
      if Ref.equal value new_val then i else StoreOp { v with value = new_val }
  | StoreOp ({ op = Store16; value; _ } as v) ->
      let new_val = demand_16 c wl value in
      if Ref.equal value new_val then i else StoreOp { v with value = new_val }
  | VecReplaceLaneOp ({ dest; lane; shape = `I64 | `F64; _ } as r) -> (
      let new_dest =
        if lane = 0 then demand_high64 c wl dest else demand_low64 c wl dest
      in
      match c.instrs.(unref r.lane_value) with
      (* | VecExtractLaneOp ({ shape = `I64 | `F64; _ } as e) -> *)
      (* let ctrl_h, ctrl_l = *)
      (* match (r.lane, e.lane) with *)
      (* | 0, 0 -> (0x0F_0E_0D_0C_0B_0A_09_08L, 0x17_16_15_14_13_12_11_10L) *)
      (* | _, 0 -> (0x17_16_15_14_13_12_11_10L, 0x07_06_05_04_03_02_01_00L) *)
      (* | 0, _ -> (0x0F_0E_0D_0C_0B_0A_09_08L, 0x1F_1E_1D_1C_1B_1A_19_18L) *)
      (* | _, _ -> (0x1F_1E_1D_1C_1B_1A_19_18L, 0x07_06_05_04_03_02_01_00L) *)
      (* in *)
      (* VecShuffleOp *)
      (* { *)
      (* var = r.var; *)
      (* arg1 = new_dest; *)
      (* arg2 = e.src; *)
      (* control_upper_bits = ctrl_h; *)
      (* control_lower_bits = ctrl_l; *)
      (* } *)
      | _ ->
          if Ref.equal new_dest dest then peephole_const c i
          else VecReplaceLaneOp { r with dest = new_dest })
  | VecExtractLaneOp ({ src; lane; shape = `I64 | `F64; _ } as e) ->
      let new_src =
        if lane = 0 then demand_low64 c wl src else demand_high64 c wl src
      in
      if Ref.equal new_src src then peephole_const c i
      else VecExtractLaneOp { e with src = new_src }
  | _ -> peephole_const c i

let peephole_dup c wl instr =
  let instrs = c.instrs in
  let newInstr =
    Instr.map
      (fun (Ref i) ->
        match instrs.(i) with DupVar { src; _ } -> src | _ -> Ref i)
      instr
  in
  if phys_equal newInstr instr then peephole_opts c wl instr else newInstr

let peephole c =
  let instrs = c.instrs in
  let length = A.length instrs in
  let worklist = List.init length ~f:(fun i -> Ref i) in
  let rec go = function
    | Ref i :: worklist ->
        (* print_s @@ sexp_of_list Ref.sexp_of_t @@ (Ref i :: worklist); *)
        let wl = Core.ref [] in
        let newInstr = peephole_dup c wl instrs.(i) in
        update c wl (Ref i) newInstr;
        go @@ List.rev !wl @ worklist
    | _ -> ()
  in
  go worklist

let dead_instrs c =
  let uses = c.uses in
  for i = A.length c.instrs - 1 downto 0 do
    let instr = A.get c.instrs i in
    (match instr with
    | AssertOp { condition } -> (
        match c.instrs.(unref condition) with
        | Const (_, const) ->
            if const <> 0 then (
              uses.(i) <- 0;
              c.instrs.(i) <- Nop)
            else raise_s [%message "always-fail assert" (instr : Instr.t)]
        | _ -> ())
    | _ -> ());
    if uses.(i) = 0 then (
      Instr.iter (decref c.uses) instr;
      c.instrs.(i) <- Nop)
  done

let opt (block : Block.t) =
  let instrs = AP.to_array block.instrs in
  let c =
    {
      instrs;
      uses = count_uses instrs block.roots block.terminator;
      roots = block.roots;
    }
  in
  peephole c;
  assert (
    equal_array equal_int c.uses
    @@ count_uses instrs block.roots block.terminator);
  dead_instrs c;
  assert (
    equal_array equal_int c.uses
    @@ count_uses instrs block.roots block.terminator);
  { block with instrs = AP.of_array instrs }

