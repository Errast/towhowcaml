open! Core
open Towhowcaml
module C = Radatnet.Commands

let c = Radatnet.Core.create ()

let () =
  C.open_file c "/home/errast/code/Touhou7/th07.exe";
  C.analyze_all c LevelFour

let intrinsics = make_intrinsics c
let translate = translate_func c ~intrinsics
let test_trans addr = print_s @@ Mir.Func.sexp_of_t @@ translate addr

let test_trans_block addr =
  let name = func_name c addr in
  let blocks = C.get_func_blocks c addr |> Array.map ~f:(make_block c) in
  let index =
    Option.value_exn
    @@ Array.binary_search
         ~compare:(fun b addr -> compare_int b.offset addr)
         blocks `Last_less_than_or_equal_to addr
  in
  print_s @@ Mir.Block.sexp_of_t
  @@ (Func_translator.translate ~blocks ~name ~intrinsics).blocks.(index)

let%expect_test "rol, xlatb" = 
 test_trans_block 0x00483ad4;
 [%expect {|
   ((id 3)
    (instrs
     ((0 (OutsideContext (var ebx) (typ Int)))
      (1 (UniOp (var __i32) (op SignExtend16) (operand (Ref 0))))
      (2 (OutsideContext (var ebp) (typ Int)))
      (3 (StoreOp (op Store16) (addr (Ref 2)) (value (Ref 1)) (offset -162)))
      (4 (Const ebx 4841516))
      (5 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
      (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
      (7 (OutsideContext (var edx) (typ Int)))
      (8 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 7)) (offset -148)))
      (9 (Const __i32 0))
      (10 (StoreOp (op Store16) (addr (Ref 2)) (value (Ref 9)) (offset -160)))
      (11 (Const __i32 0))
      (12 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 11))))
      (13 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 12)) (offset -144)))
      (14
       (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed false)
        (offset -159)))
      (15 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 14))))
      (16 (OutsideContext (var ecx) (typ Int)))
      (17 (BiOp (var ecx) (op MergeTruncLow8) (lhs (Ref 15)) (rhs (Ref 16))))
      (18 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 17))))
      (19 (Const __i32 1))
      (20 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 18)) (rhs (Ref 19))))
      (21 (BiOp (var ecx) (op MergeTruncLow8) (lhs (Ref 20)) (rhs (Ref 17))))
      (22 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 21))))
      (23 (Const __i32 1))
      (24 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 23))))
      (25
       (SignedBiOp (var __i32) (op ShiftRight) (signed true) (lhs (Ref 22))
        (rhs (Ref 24))))
      (26 (BiOp (var ecx) (op MergeTruncLow8) (lhs (Ref 25)) (rhs (Ref 21))))
      (27 (Const __i32 1))
      (28 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 26))))
      (29 (BiOp (var __i32) (op RotateLeft) (lhs (Ref 28)) (rhs (Ref 27))))
      (30 (BiOp (var ecx) (op MergeTruncLow8) (lhs (Ref 29)) (rhs (Ref 26))))
      (31 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 30))))
      (32 (OutsideContext (var eax) (typ Int)))
      (33 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 31)) (rhs (Ref 32))))
      (34 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 33))))
      (35 (Const __i32 15))
      (36 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 35))))
      (37 (BiOp (var __i32) (op And) (lhs (Ref 34)) (rhs (Ref 36))))
      (38 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 37)) (rhs (Ref 33))))
      (39 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 38))))
      (40 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 39))))
      (41 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 40)) (signed false)))
      (42 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 41)) (rhs (Ref 38))))
      (43 (UniOp (var eax) (op SignExtendLow8) (operand (Ref 42))))
      (44 (Const __i32 1028))
      (45 (BiOp (var ecx) (op And) (lhs (Ref 30)) (rhs (Ref 44))))
      (46 (DupVar (var ebx) (src (Ref 7)) (typ Int)))
      (47 (BiOp (var ebx) (op Add) (lhs (Ref 46)) (rhs (Ref 43))))
      (48 (Const __i32 16))
      (49 (BiOp (var ebx) (op Add) (lhs (Ref 47)) (rhs (Ref 48))))
      (50 (LoadOp (var __i32) (op Load32) (addr (Ref 49))))
      (51 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 6))))
      (52 (OutsideContext (var esp) (typ Int)))
      (53
       (CallIndirectOp (table_index (Ref 50))
        (args ((Ref 45) (Ref 52) (Ref 7)))))
      (54 (ReturnedOp (var eax) (typ Int)))
      (55 (ReturnedOp (var esp) (typ Int)))
      (56 (ReturnedOp (var edx) (typ Int)))))
    (terminator Return)
    (roots
     ((Ref 0) (Ref 2) (Ref 3) (Ref 5) (Ref 7) (Ref 8) (Ref 10) (Ref 13)
      (Ref 16) (Ref 32) (Ref 45) (Ref 49) (Ref 51) (Ref 52) (Ref 53) (Ref 54)
      (Ref 55) (Ref 56))))
   |}]

let%expect_test "shufpd" = 
  test_trans_block 0x0048be1e ;
  [%expect {|
    ((id 4)
     (instrs
      ((0 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
       (1
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 0))
         (rhs (Ref 0))))
       (2
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 1))
         (rhs (Ref 1))))
       (3 (Const __i32 4820576))
       (4 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 3))))
       (5
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 4))
         (rhs (Ref 2))))
       (6 (Const __i32 4820560))
       (7 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 6))))
       (8
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 5))
         (rhs (Ref 7))))
       (9
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 8))
         (rhs (Ref 2))))
       (10 (Const __i32 4820544))
       (11 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 10))))
       (12
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 9))
         (rhs (Ref 11))))
       (13
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 12))
         (rhs (Ref 2))))
       (14 (Const __i32 4820528))
       (15 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 14))))
       (16
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 13))
         (rhs (Ref 15))))
       (17
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 16))
         (rhs (Ref 1))))
       (18 (VecExtractLaneOp (var __i64) (src (Ref 17)) (shape I64) (lane 0)))
       (19
        (VecReplaceLaneOp (var __vec) (dest (Ref 16)) (lane_value (Ref 18))
         (shape I64) (lane 0)))
       (20
        (VecShuffleOp (var __vec) (arg1 (Ref 19)) (arg2 (Ref 19))
         (control_lower_bits 1084818905618843912)
         (control_upper_bits 1663540288323457296)))
       (21
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 19))
         (rhs (Ref 20))))
       (22 (VecExtractLaneOp (var __i64) (src (Ref 21)) (shape I64) (lane 0)))
       (23
        (VecReplaceLaneOp (var __vec) (dest (Ref 19)) (lane_value (Ref 22))
         (shape I64) (lane 0)))
       (24 (GetGlobalOp (var __vec) (global ((name __xmm7_global__) (typ Vec)))))
       (25
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 23))
         (rhs (Ref 24))))
       (26 (VecExtractLaneOp (var __i64) (src (Ref 25)) (shape I64) (lane 0)))
       (27
        (VecReplaceLaneOp (var __vec) (dest (Ref 23)) (lane_value (Ref 26))
         (shape I64) (lane 0)))
       (28
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 24))
         (rhs (Ref 27))))
       (29 (VecExtractLaneOp (var __i64) (src (Ref 28)) (shape I64) (lane 0)))
       (30
        (VecReplaceLaneOp (var __vec) (dest (Ref 24)) (lane_value (Ref 29))
         (shape I64) (lane 0)))
       (31 (VecExtractLaneOp (var __i64) (src (Ref 30)) (shape I64) (lane 0)))
       (32 (OutsideContext (var esp) (typ Int)))
       (33
        (StoreOp (op LongStore64) (addr (Ref 32)) (value (Ref 31)) (offset 4)))
       (34 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 32)) (offset 4)))
       (35 (LoadOp (var __i32) (op Load32) (addr (Ref 32))))
       (36 (OutsideContext (var __ret_addr__) (typ Int)))
       (37 (BiOp (var __i32) (op Equal) (lhs (Ref 35)) (rhs (Ref 36))))
       (38 (AssertOp (condition (Ref 37))))
       (39 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (40
        (StoreOp (op FloatStore64) (addr (Ref 39)) (value (Ref 34)) (offset 8)))
       (41 (Const __i32 8))
       (42 (BiOp (var __i32) (op Add) (lhs (Ref 39)) (rhs (Ref 41))))
       (43
        (SetGlobalOp (value (Ref 20))
         (global ((name __xmm3_global__) (typ Vec)))))
       (44
        (SetGlobalOp (value (Ref 27))
         (global ((name __xmm5_global__) (typ Vec)))))
       (45
        (SetGlobalOp (value (Ref 1)) (global ((name __xmm1_global__) (typ Vec)))))
       (46
        (SetGlobalOp (value (Ref 42)) (global ((name __fpuStack__) (typ Int)))))
       (47
        (SetGlobalOp (value (Ref 30))
         (global ((name __xmm7_global__) (typ Vec)))))))
     (terminator Return)
     (roots
      ((Ref 0) (Ref 24) (Ref 32) (Ref 33) (Ref 36) (Ref 38) (Ref 39) (Ref 40)
       (Ref 43) (Ref 44) (Ref 45) (Ref 46) (Ref 47))))
    |}]

let%expect_test "movq, comisd jae" = 
 test_trans_block 0x0048bed8;
 [%expect {|
   ((id 7)
    (instrs
     ((0 (GetGlobalOp (var __vec) (global ((name __xmm7_global__) (typ Vec)))))
      (1 (VecExtractLaneOp (var __fl) (src (Ref 0)) (shape F64) (lane 0)))
      (2 (GetGlobalOp (var __vec) (global ((name __xmm6_global__) (typ Vec)))))
      (3
       (VecReplaceLaneOp (var __vec) (dest (Ref 2)) (lane_value (Ref 1))
        (shape F64) (lane 0)))
      (4 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
      (5 (BiOp (var __vec) (op VecXor) (lhs (Ref 3)) (rhs (Ref 4))))
      (6 (Const __i32 4820656))
      (7 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 6))))
      (8
       (SignedVecLaneBiOp (var __vec) (op VecGreaterThan) (signed true)
        (shape F64) (lhs (Ref 4)) (rhs (Ref 7))))
      (9 (VecExtractLaneOp (var __i32) (src (Ref 8)) (shape I32) (lane 0)))
      (10
       (SetGlobalOp (value (Ref 5)) (global ((name __xmm6_global__) (typ Vec)))))))
    (terminator
     (Branch (succeed (Block 9)) (fail (Block 8)) (condition (Ref 9))))
    (roots ((Ref 0) (Ref 2) (Ref 4) (Ref 10))))
   |}]

let%expect_test "comisd jp" = 
 test_trans_block 0x0048bdb6;
 [%expect {|
   ((id 0)
    (instrs
     ((0 (OutsideContext (var esp) (typ Int)))
      (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
      (2 (GetGlobalOp (var __vec) (global ((name __xmm7_global__) (typ Vec)))))
      (3
       (VecShuffleOp (var __vec) (arg1 (Ref 2)) (arg2 (Ref 2))
        (control_lower_bits 506097522914230528)
        (control_upper_bits 1663540288323457296)))
      (4 (Const __i32 4820352))
      (5 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 4))))
      (6 (BiOp (var __vec) (op VecAnd) (lhs (Ref 3)) (rhs (Ref 5))))
      (7 (Const __i32 4820720))
      (8 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 7))))
      (9
       (VecLaneBiOp (var __vec) (op VecNotEqual) (shape F64) (lhs (Ref 8))
        (rhs (Ref 8))))
      (10
       (VecLaneBiOp (var __vec) (op VecNotEqual) (shape F64) (lhs (Ref 6))
        (rhs (Ref 6))))
      (11 (BiOp (var __vec) (op VecOr) (lhs (Ref 10)) (rhs (Ref 9))))
      (12 (VecExtractLaneOp (var __i32) (src (Ref 11)) (shape I32) (lane 0)))
      (13
       (SetGlobalOp (value (Ref 6)) (global ((name __xmm2_global__) (typ Vec)))))
      (14
       (SetGlobalOp (value (Ref 3)) (global ((name __xmm7_global__) (typ Vec)))))
      (15
       (SignedVecLaneBiOp (var __vec) (op VecGreaterThan) (signed true)
        (shape F64) (lhs (Ref 6)) (rhs (Ref 8))))
      (16 (VecExtractLaneOp (var __i32) (src (Ref 15)) (shape I32) (lane 0)))
      (17 (DupVar (var __input_compare_arg__) (src (Ref 16)) (typ Int)))))
    (terminator
     (Branch (succeed (Block 18)) (fail (Block 1)) (condition (Ref 12))))
    (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 13) (Ref 14) (Ref 17))))
   |}]

let%expect_test "fprem" =
  test_trans_block 0x004892f4;
  [%expect {|
    ((id 9)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int))) (1 (Const __i32 40))
       (2 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (CallOp (func __load_big_float__) (args ((Ref 2)))))
       (4 (ReturnedOp (var __fl) (typ Float)))
       (5 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 24)))
       (6 (LoadOp (var ebx) (op Load32) (addr (Ref 0)) (offset 48)))
       (7 (Const __i32 32767))
       (8 (BiOp (var ebx) (op And) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (DupVar (var ecx) (src (Ref 8)) (typ Int)))
       (10 (BiOp (var ebx) (op Subtract) (lhs (Ref 8)) (rhs (Ref 5))))
       (11 (Const __i32 7))
       (12 (BiOp (var ebx) (op And) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var ebx) (op Or) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (BiOp (var ecx) (op Subtract) (lhs (Ref 9)) (rhs (Ref 14))))
       (16 (DupVar (var ebx) (src (Ref 5)) (typ Int))) (17 (Const __i32 32768))
       (18 (BiOp (var ebx) (op And) (lhs (Ref 16)) (rhs (Ref 17))))
       (19 (BiOp (var ecx) (op Or) (lhs (Ref 15)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 19)) (offset 24)))
       (21 (Const __i32 16))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 21))))
       (23 (CallOp (func __load_big_float__) (args ((Ref 22)))))
       (24 (ReturnedOp (var __fl) (typ Float)))
       (25 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset 24)))
       (26 (CallOp (func __float_mod__) (args ((Ref 4) (Ref 24)))))
       (27 (ReturnedOp (var __fl) (typ Float))) (28 (Const __i32 40))
       (29 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 28))))
       (30 (CallOp (func __store_big_float__) (args ((Ref 29) (Ref 27)))))
       (31 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 8)))
     (roots
      ((Ref 0) (Ref 3) (Ref 4) (Ref 5) (Ref 18) (Ref 19) (Ref 20) (Ref 23)
       (Ref 24) (Ref 25) (Ref 26) (Ref 27) (Ref 30) (Ref 31))))
    |}]

let%expect_test "sub ja" =
  test_trans_block 0x0048929e;
  [%expect {|
    ((id 7)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 24)))
       (2 (Const __i32 32767))
       (3 (BiOp (var eax) (op And) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (Const __i32 63))
       (5 (BiOp (var eax) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (LoadOp (var ebx) (op Load32) (addr (Ref 0)) (offset 48)))
       (7 (Const __i32 32767))
       (8 (BiOp (var ebx) (op And) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (BiOp (var ebx) (op Subtract) (lhs (Ref 8)) (rhs (Ref 5))))
       (10
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 8))
         (rhs (Ref 5))))))
     (terminator
      (Branch (succeed (Block 10)) (fail (Block 8)) (condition (Ref 10))))
     (roots ((Ref 0) (Ref 5) (Ref 9))))
    |}]

let%expect_test "sahf je" =
  test_trans_block 0x0048582f;
  [%expect {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (Const __i32 4841890))
       (3 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 2))))
       (4 (BiOp (var __fl) (op FloatMult) (lhs (Ref 1)) (rhs (Ref 3))))
       (5 (UniOp (var __fl) (op FloatRound) (operand (Ref 4))))
       (6 (Landmine (var eax) (typ Int)))
       (7 (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 5)) (rhs (Ref 4))))
       (8
        (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 5)) (rhs (Ref 4))))
       (9 (BiOp (var __i32) (op Equal) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 1))))
       (11 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 4)) (offset 8)))
       (12 (Const __i32 8))
       (13 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 12))))
       (14
        (SetGlobalOp (value (Ref 13)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 2)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 6) (Ref 10) (Ref 11) (Ref 14))))
    |}]

let%expect_test "add jae" =
  test_trans_block 0x00488a1e;
  [%expect {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (BiOp (var eax) (op Add) (lhs (Ref 1)) (rhs (Ref 1))))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 2)) (rhs (Ref 1))))))
     (terminator
      (Branch (succeed (Block 11)) (fail (Block 2)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 2))))
    |}]

let%expect_test "pmaxsw, pcmpeqd, pmovmskb" =
  test_trans_block 0x00484ef1;
  [%expect {|
    ((id 9)
     (instrs
      ((0 (OutsideContext (var edx) (typ Int))) (1 (Const __i32 0))
       (2 (BiOp (var edx) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
       (3 (Const __i32 32751))
       (4 (BiOp (var edx) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (GetGlobalOp (var __vec) (global ((name __xmm3_global__) (typ Vec)))))
       (6 (Const __i32 52))
       (7
        (VecShiftLeftOp (var __vec) (operand (Ref 5)) (count (Ref 6))
         (shape I64)))
       (8 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
       (9 (BiOp (var __vec) (op VecOr) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (Const ecx 1011))
       (11
        (VecReplaceLaneOp (var __vec) (dest (Ref 7)) (lane_value (Ref 10))
         (shape I32) (lane 0)))
       (12 (GetGlobalOp (var __vec) (global ((name __xmm1_global__) (typ Vec)))))
       (13 (Const __i32 20))
       (14
        (VecShiftRightOp (var __vec) (operand (Ref 12)) (count (Ref 13))
         (shape I64) (signed false)))
       (15
        (VecLaneBiOp (var __vec) (op VecSub) (shape I32) (lhs (Ref 14))
         (rhs (Ref 11))))
       (16 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (17
        (SignedVecLaneBiOp (var __vec) (op VecMax) (signed true) (shape I16)
         (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (VecExtractLaneOp (var __i32) (src (Ref 17)) (shape I32) (lane 0)))
       (19
        (VecShiftLeftOp (var __vec) (operand (Ref 9)) (count (Ref 18))
         (shape I64)))
       (20
        (VecLaneBiOp (var __vec) (op VecEqual) (shape I32) (lhs (Ref 19))
         (rhs (Ref 16))))
       (21 (UniOp (var eax) (op VecInt8SignBitmask) (operand (Ref 20))))
       (22 (DupVar (var ecx) (src (Ref 4)) (typ Int))) (23 (Const __i32 32767))
       (24 (BiOp (var edx) (op And) (lhs (Ref 4)) (rhs (Ref 23))))
       (25 (Const __i32 32752))
       (26
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 24)) (rhs (Ref 25))))
       (27
        (SetGlobalOp (value (Ref 16))
         (global ((name __xmm3_global__) (typ Vec)))))
       (28
        (SetGlobalOp (value (Ref 20))
         (global ((name __xmm2_global__) (typ Vec)))))
       (29
        (SetGlobalOp (value (Ref 17))
         (global ((name __xmm1_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 10)) (condition (Ref 26))))
     (roots
      ((Ref 0) (Ref 5) (Ref 8) (Ref 12) (Ref 21) (Ref 22) (Ref 24) (Ref 27)
       (Ref 28) (Ref 29))))
    |}]

let%expect_test "xorpd, pinsrw, movlpd, mulsd" =
  test_trans_block 0x00484fd8;
  [%expect {|
    ((id 18)
     (instrs
      ((0 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (1 (Const eax 17392))
       (2
        (VecReplaceLaneOp (var __vec) (dest (Ref 0)) (lane_value (Ref 1))
         (shape I16) (lane 3)))
       (3 (GetGlobalOp (var __vec) (global ((name __xmm7_global__) (typ Vec)))))
       (4 (Const __i32 4803568))
       (5
        (VecLoadLaneOp (var __vec) (dest_vec (Ref 3)) (addr (Ref 4)) (shape I64)
         (lane 0)))
       (6 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
       (7 (Const __i32 4803584))
       (8
        (VecLoadLaneOp (var __vec) (dest_vec (Ref 6)) (addr (Ref 7)) (shape I64)
         (lane 0)))
       (9 (GetGlobalOp (var __vec) (global ((name __xmm4_global__) (typ Vec)))))
       (10
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 2))
         (rhs (Ref 9))))
       (11 (VecExtractLaneOp (var __i64) (src (Ref 10)) (shape I64) (lane 0)))
       (12
        (VecReplaceLaneOp (var __vec) (dest (Ref 2)) (lane_value (Ref 11))
         (shape I64) (lane 0)))
       (13 (VecExtractLaneOp (var edx) (src (Ref 9)) (shape I32) (lane 0)))
       (14 (Const __i32 32))
       (15
        (VecShiftRightOp (var __vec) (operand (Ref 9)) (count (Ref 14))
         (shape I64) (signed false)))
       (16 (VecExtractLaneOp (var eax) (src (Ref 15)) (shape I32) (lane 0)))
       (17 (Const __i32 0))
       (18 (BiOp (var __i32) (op Equal) (lhs (Ref 13)) (rhs (Ref 17))))
       (19
        (SetGlobalOp (value (Ref 15))
         (global ((name __xmm4_global__) (typ Vec)))))
       (20
        (SetGlobalOp (value (Ref 12))
         (global ((name __xmm0_global__) (typ Vec)))))
       (21
        (SetGlobalOp (value (Ref 8)) (global ((name __xmm2_global__) (typ Vec)))))
       (22
        (SetGlobalOp (value (Ref 5)) (global ((name __xmm7_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 18))))
     (roots
      ((Ref 3) (Ref 6) (Ref 9) (Ref 13) (Ref 16) (Ref 19) (Ref 20) (Ref 21)
       (Ref 22))))
    |}]

let%expect_test "pextrw, mulsd" =
  test_trans_block 0x00485561;
  [%expect {|
    ((id 69)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var esi) (op Load32) (addr (Ref 0)))) (2 (Const __i32 4))
       (3 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 2))))
       (4 (GetGlobalOp (var __vec) (global ((name __xmm0_global__) (typ Vec)))))
       (5 (GetGlobalOp (var __vec) (global ((name __xmm1_global__) (typ Vec)))))
       (6
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 4))
         (rhs (Ref 5))))
       (7 (VecExtractLaneOp (var __i64) (src (Ref 6)) (shape I64) (lane 0)))
       (8
        (VecReplaceLaneOp (var __vec) (dest (Ref 4)) (lane_value (Ref 7))
         (shape I64) (lane 0)))
       (9 (GetGlobalOp (var __vec) (global ((name __xmm7_global__) (typ Vec)))))
       (10
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 8))
         (rhs (Ref 9))))
       (11 (VecExtractLaneOp (var __i64) (src (Ref 10)) (shape I64) (lane 0)))
       (12
        (VecReplaceLaneOp (var __vec) (dest (Ref 8)) (lane_value (Ref 11))
         (shape I64) (lane 0)))
       (13 (GetGlobalOp (var __vec) (global ((name __xmm6_global__) (typ Vec)))))
       (14
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 13))
         (rhs (Ref 12))))
       (15 (VecExtractLaneOp (var __i64) (src (Ref 14)) (shape I64) (lane 0)))
       (16
        (VecReplaceLaneOp (var __vec) (dest (Ref 13)) (lane_value (Ref 15))
         (shape I64) (lane 0)))
       (17
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 12))
         (rhs (Ref 16))))
       (18 (VecExtractLaneOp (var __i64) (src (Ref 17)) (shape I64) (lane 0)))
       (19
        (VecReplaceLaneOp (var __vec) (dest (Ref 12)) (lane_value (Ref 18))
         (shape I64) (lane 0)))
       (20 (VecExtractLaneOp (var eax) (src (Ref 19)) (shape I16) (lane 3)))
       (21 (Const __i32 32752))
       (22 (BiOp (var eax) (op And) (lhs (Ref 20)) (rhs (Ref 21))))
       (23 (Const edx 24)) (24 (Const __i32 32752))
       (25 (BiOp (var __i32) (op Equal) (lhs (Ref 22)) (rhs (Ref 24))))
       (26
        (SetGlobalOp (value (Ref 19))
         (global ((name __xmm0_global__) (typ Vec)))))
       (27
        (SetGlobalOp (value (Ref 16))
         (global ((name __xmm6_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 59)) (fail (Block 70)) (condition (Ref 25))))
     (roots
      ((Ref 0) (Ref 1) (Ref 3) (Ref 4) (Ref 5) (Ref 9) (Ref 13) (Ref 22)
       (Ref 23) (Ref 26) (Ref 27))))
    |}]

let%expect_test "movsd" =
  test_trans_block 0x004852c9;
  [%expect {|
    ((id 52)
     (instrs
      ((0 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
       (1
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 0))
         (rhs (Ref 0))))
       (2 (VecExtractLaneOp (var __i64) (src (Ref 1)) (shape I64) (lane 0)))
       (3
        (VecReplaceLaneOp (var __vec) (dest (Ref 0)) (lane_value (Ref 2))
         (shape I64) (lane 0)))
       (4 (GetGlobalOp (var __vec) (global ((name __xmm0_global__) (typ Vec)))))
       (5 (VecExtractLaneOp (var __i64) (src (Ref 3)) (shape I64) (lane 0)))
       (6
        (VecReplaceLaneOp (var __vec) (dest (Ref 4)) (lane_value (Ref 5))
         (shape I64) (lane 0)))
       (7 (Const edx 1006))
       (8
        (SetGlobalOp (value (Ref 3)) (global ((name __xmm2_global__) (typ Vec)))))
       (9
        (SetGlobalOp (value (Ref 6)) (global ((name __xmm0_global__) (typ Vec)))))))
     (terminator (Goto (Block 59)))
     (roots ((Ref 0) (Ref 4) (Ref 7) (Ref 8) (Ref 9))))
    |}]

let%expect_test "backward direction" =
  test_trans_block 0x0047d9d7;
  [%expect
    {|
   ((id 23)
    (instrs
     ((0 (OutsideContext (var ecx) (typ Int))) (1 (Const __i32 2))
      (2 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
      (3 (Const __i32 1))
      (4 (BiOp (var __i32) (op Subtract) (lhs (Ref 2)) (rhs (Ref 3))))
      (5 (OutsideContext (var esi) (typ Int)))
      (6 (BiOp (var esi) (op Subtract) (lhs (Ref 5)) (rhs (Ref 4))))
      (7 (OutsideContext (var edi) (typ Int)))
      (8 (BiOp (var edi) (op Subtract) (lhs (Ref 7)) (rhs (Ref 4))))
      (9 (Memcopy (count (Ref 2)) (src (Ref 6)) (dest (Ref 8))))
      (10 (Const __i32 1))
      (11 (BiOp (var esi) (op Subtract) (lhs (Ref 6)) (rhs (Ref 10))))
      (12 (Const __i32 1))
      (13 (BiOp (var edi) (op Subtract) (lhs (Ref 8)) (rhs (Ref 12))))
      (14 (Const ecx 0)) (15 (OutsideContext (var edx) (typ Int)))))
    (terminator
     (Switch (cases ((Block 29) (Block 30) (Block 31) (Block 32)))
      (default (Block 33)) (switch_on (Ref 15))))
    (roots
     ((Ref 0) (Ref 5) (Ref 7) (Ref 9) (Ref 11) (Ref 13) (Ref 14) (Ref 15))))
   |}]

let%expect_test "int3" =
  test_trans_block 0x00487ff1;
  [%expect
    {|
   ((id 10)
    (instrs
     ((0 (Const __i32 3)) (1 (OutsideContext (var esp) (typ Int)))
      (2 (Const __i32 4))
      (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
      (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
      (5 (Const __i32 4))
      (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
      (7 (Const __i32 4751340))
      (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
      (9 (OutsideContext (var ecx) (typ Int)))
      (10 (OutsideContext (var edx) (typ Int)))
      (11 (CallOp (func __func47f9da__) (args ((Ref 9) (Ref 6) (Ref 10)))))
      (12 (ReturnedOp (var eax) (typ Int)))
      (13 (ReturnedOp (var esp) (typ Int)))
      (14 (ReturnedOp (var edx) (typ Int)))))
    (terminator Return)
    (roots
     ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
      (Ref 14))))
   |}]

(* check this *)
let%expect_test "mmx stuff" =
  test_trans_block 0x00476bce;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (2 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (3 (VecSplatOp (var __vec) (value (Ref 2)) (shape I64)))
       (4 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (5 (OutsideContext (var ebx) (typ Int)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 5))))
       (7 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 6))))
       (8 (VecSplatOp (var __vec) (value (Ref 7)) (shape I64)))
       (9
        (VecExtend (var __vec) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 3))))
       (10 (OutsideContext (var ecx) (typ Int)))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (12 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 11))))
       (13 (VecSplatOp (var __vec) (value (Ref 12)) (shape I64)))
       (14
        (VecExtend (var __vec) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 8))))
       (15 (Const __i32 4835064))
       (16 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 15))))
       (17
        (SignedVecLaneBiOp (var __vec) (op VecSubSaturating) (signed true)
         (shape I16) (lhs (Ref 14)) (rhs (Ref 16))))
       (18
        (VecExtend (var __vec) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 13))))
       (19 (Const __i32 4835064))
       (20 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 19))))
       (21
        (SignedVecLaneBiOp (var __vec) (op VecSubSaturating) (signed true)
         (shape I16) (lhs (Ref 18)) (rhs (Ref 20))))
       (22
        (VecShuffleOp (var __vec) (arg1 (Ref 17)) (arg2 (Ref 21))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (23 (Const __i32 4835072))
       (24 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 23))))
       (25 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 22)) (rhs (Ref 24))))
       (26 (Const __i32 8))
       (27
        (VecShiftLeftOp (var __vec) (operand (Ref 9)) (count (Ref 26))
         (shape I16)))
       (28
        (VecExtend (var __vec) (signed false) (shape I16) (half_used LowOrder)
         (operand (Ref 27))))
       (29
        (VecShuffleOp (var __vec) (arg1 (Ref 27)) (arg2 (Ref 4))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (30
        (VecShuffleOp (var __vec) (arg1 (Ref 17)) (arg2 (Ref 21))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (31
        (VecLaneBiOp (var __vec) (op VecAdd) (shape I32) (lhs (Ref 25))
         (rhs (Ref 28))))
       (32 (Const __i32 4835072))
       (33 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 32))))
       (34 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 30)) (rhs (Ref 33))))
       (35
        (VecShuffleOp (var __vec) (arg1 (Ref 21)) (arg2 (Ref 9))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (36 (Const __i32 4835080))
       (37 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 36))))
       (38 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 35)) (rhs (Ref 37))))
       (39 (Const __i32 8))
       (40
        (VecShiftRightOp (var __vec) (operand (Ref 31)) (count (Ref 39))
         (shape I64) (signed true)))
       (41
        (VecLaneBiOp (var __vec) (op VecAdd) (shape I32) (lhs (Ref 34))
         (rhs (Ref 29))))
       (42
        (VecShuffleOp (var __vec) (arg1 (Ref 17)) (arg2 (Ref 9))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (43 (Const __i32 4835088))
       (44 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 43))))
       (45 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 42)) (rhs (Ref 44))))
       (46 (Const __i32 8))
       (47
        (VecShiftRightOp (var __vec) (operand (Ref 41)) (count (Ref 46))
         (shape I64) (signed true)))
       (48 (Const __i32 8))
       (49
        (VecShiftRightOp (var __vec) (operand (Ref 38)) (count (Ref 48))
         (shape I64) (signed true)))
       (50
        (VecShuffleOp (var __vec) (arg1 (Ref 17)) (arg2 (Ref 9))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (51 (Const __i32 8))
       (52
        (VecShiftRightOp (var __vec) (operand (Ref 45)) (count (Ref 51))
         (shape I64) (signed true)))
       (53 (Const __i32 4835088))
       (54 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 53))))
       (55 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 50)) (rhs (Ref 54))))
       (56
        (VecShuffleOp (var __vec) (arg1 (Ref 21)) (arg2 (Ref 9))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (57 (Const __i32 4835080))
       (58 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 57))))
       (59 (BiOp (var __vec) (op VecMulAdd16Bit) (lhs (Ref 56)) (rhs (Ref 58))))
       (60
        (VecShuffleOp (var __vec) (arg1 (Ref 49)) (arg2 (Ref 40))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (61
        (VecShuffleOp (var __vec) (arg1 (Ref 49)) (arg2 (Ref 40))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (62 (Const __i32 8))
       (63
        (VecShiftRightOp (var __vec) (operand (Ref 55)) (count (Ref 62))
         (shape I64) (signed true)))
       (64 (Const __i32 4847464))
       (65 (LoadOp (var __i32) (op Load32) (addr (Ref 64))))
       (66 (VecSplatOp (var __vec) (value (Ref 65)) (shape I32)))
       (67
        (VecShuffleOp (var __vec) (arg1 (Ref 52)) (arg2 (Ref 66))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (68 (Const __i32 4847464))
       (69 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 68))))
       (70
        (VecShuffleOp (var __vec) (arg1 (Ref 52)) (arg2 (Ref 69))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (71 (Const __i32 8))
       (72
        (VecShiftRightOp (var __vec) (operand (Ref 59)) (count (Ref 71))
         (shape I64) (signed true)))
       (73
        (SignedBiOp (var __i64) (op VecNarrow32Bit) (signed true) (lhs (Ref 60))
         (rhs (Ref 67))))
       (74
        (SignedBiOp (var __i64) (op VecNarrow32Bit) (signed true) (lhs (Ref 61))
         (rhs (Ref 70))))
       (75
        (SignedBiOp (var __i64) (op VecNarrow16Bit) (signed false) (lhs (Ref 73))
         (rhs (Ref 74))))
       (76
        (VecShuffleOp (var __vec) (arg1 (Ref 72)) (arg2 (Ref 47))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (77
        (VecShuffleOp (var __vec) (arg1 (Ref 72)) (arg2 (Ref 47))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (78 (Const __i32 4847464))
       (79 (LoadOp (var __i32) (op Load32) (addr (Ref 78))))
       (80 (VecSplatOp (var __vec) (value (Ref 79)) (shape I32)))
       (81
        (VecShuffleOp (var __vec) (arg1 (Ref 63)) (arg2 (Ref 80))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (82 (Const __i32 4847464))
       (83 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 82))))
       (84
        (VecShuffleOp (var __vec) (arg1 (Ref 63)) (arg2 (Ref 83))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (85
        (SignedBiOp (var __i64) (op VecNarrow32Bit) (signed true) (lhs (Ref 76))
         (rhs (Ref 81))))
       (86 (Const __i32 4835096))
       (87 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 86))))
       (88 (BiOp (var __vec) (op VecAnd) (lhs (Ref 75)) (rhs (Ref 87))))
       (89
        (SignedBiOp (var __i64) (op VecNarrow32Bit) (signed true) (lhs (Ref 77))
         (rhs (Ref 84))))
       (90 (Const __i32 8))
       (91
        (VecShiftRightOp (var __vec) (operand (Ref 88)) (count (Ref 90))
         (shape I64) (signed false)))
       (92 (OutsideContext (var edx) (typ Int))) (93 (Const __i32 12))
       (94 (BiOp (var edx) (op Add) (lhs (Ref 92)) (rhs (Ref 93))))
       (95 (BiOp (var __vec) (op VecOr) (lhs (Ref 75)) (rhs (Ref 91))))
       (96
        (SignedBiOp (var __i64) (op VecNarrow16Bit) (signed false) (lhs (Ref 85))
         (rhs (Ref 89))))
       (97 (Const __i32 32))
       (98
        (VecShiftRightOp (var __vec) (operand (Ref 91)) (count (Ref 97))
         (shape I64) (signed false)))
       (99 (Const __i32 4))
       (100 (BiOp (var eax) (op Add) (lhs (Ref 0)) (rhs (Ref 99))))
       (101 (VecExtractLaneOp (var __i32) (src (Ref 95)) (shape I32) (lane 0)))
       (102
        (StoreOp (op Store32) (addr (Ref 94)) (value (Ref 101)) (offset -12)))
       (103
        (VecShuffleOp (var __vec) (arg1 (Ref 98)) (arg2 (Ref 96))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (104 (Const __i32 24))
       (105
        (VecShiftRightOp (var __vec) (operand (Ref 96)) (count (Ref 104))
         (shape I64) (signed false)))
       (106 (Const __i32 4))
       (107 (BiOp (var ecx) (op Add) (lhs (Ref 10)) (rhs (Ref 106))))
       (108 (VecExtractLaneOp (var __i32) (src (Ref 103)) (shape I32) (lane 0)))
       (109 (StoreOp (op Store32) (addr (Ref 94)) (value (Ref 108)) (offset -8)))
       (110 (Const __i32 48))
       (111
        (VecShiftRightOp (var __vec) (operand (Ref 103)) (count (Ref 110))
         (shape I64) (signed false)))
       (112 (BiOp (var __vec) (op VecOr) (lhs (Ref 105)) (rhs (Ref 111))))
       (113 (Const __i32 4))
       (114 (BiOp (var ebx) (op Add) (lhs (Ref 5)) (rhs (Ref 113))))
       (115 (VecExtractLaneOp (var __i32) (src (Ref 112)) (shape I32) (lane 0)))
       (116 (StoreOp (op Store32) (addr (Ref 94)) (value (Ref 115)) (offset -4)))
       (117 (Const __i32 1)) (118 (OutsideContext (var edi) (typ Int)))
       (119 (BiOp (var edi) (op Subtract) (lhs (Ref 118)) (rhs (Ref 117))))
       (120
        (SetGlobalOp (value (Ref 89)) (global ((name __mm1_global__) (typ Vec)))))
       (121
        (SetGlobalOp (value (Ref 84)) (global ((name __mm4_global__) (typ Vec)))))
       (122
        (SetGlobalOp (value (Ref 47)) (global ((name __mm5_global__) (typ Vec)))))
       (123
        (SetGlobalOp (value (Ref 111))
         (global ((name __mm3_global__) (typ Vec)))))
       (124
        (SetGlobalOp (value (Ref 81)) (global ((name __mm7_global__) (typ Vec)))))
       (125
        (SetGlobalOp (value (Ref 95)) (global ((name __mm0_global__) (typ Vec)))))
       (126
        (SetGlobalOp (value (Ref 74)) (global ((name __mm6_global__) (typ Vec)))))
       (127
        (SetGlobalOp (value (Ref 112))
         (global ((name __mm2_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 119))))
     (roots
      ((Ref 0) (Ref 5) (Ref 10) (Ref 92) (Ref 94) (Ref 100) (Ref 102) (Ref 107)
       (Ref 109) (Ref 114) (Ref 116) (Ref 118) (Ref 119) (Ref 120) (Ref 121)
       (Ref 122) (Ref 123) (Ref 124) (Ref 125) (Ref 126) (Ref 127))))
    |}]

let%expect_test "xor je" =
  test_trans_block 0x0046fcb3;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int)))
       (1 (OutsideContext (var esi) (typ Int)))
       (2 (BiOp (var eax) (op Xor) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (UniOp (var __i32) (op EqualsZero) (operand (Ref 2))))))
     (terminator
      (Branch (succeed (Block 11)) (fail (Block 7)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 1) (Ref 2))))
    |}]

let%expect_test "imul byte" =
  test_trans_block 0x0046e208;
  [%expect
    {|
    ((id 32)
     (instrs
      ((0 (OutsideContext (var ecx) (typ Int)))
       (1
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed false)
         (offset 24)))
       (2 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 1))))
       (3 (OutsideContext (var eax) (typ Int)))
       (4 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 2)) (rhs (Ref 3))))
       (5
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed false)
         (offset 29)))
       (6 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 5))))
       (7 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 4))))
       (8 (BiOp (var __i32) (op Multiply) (lhs (Ref 7)) (rhs (Ref 6))))
       (9 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 8)) (rhs (Ref 4))))
       (10 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 9))))
       (11 (StoreOp (op Store8) (addr (Ref 0)) (value (Ref 10)) (offset 30)))
       (12 (UniOp (var eax) (op ZeroExtendLow8) (operand (Ref 9))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (14 (BiOp (var eax) (op Multiply) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (Const __i32 7))
       (16 (BiOp (var eax) (op Add) (lhs (Ref 14)) (rhs (Ref 15))))
       (17 (Const __i32 3))
       (18 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 17))))
       (19
        (SignedBiOp (var eax) (op ShiftRight) (signed false) (lhs (Ref 16))
         (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 19)) (offset 12)))
       (21 (OutsideContext (var esp) (typ Int)))
       (22 (LoadOp (var ebx) (op Load32) (addr (Ref 21)))) (23 (Const __i32 4))
       (24 (BiOp (var esp) (op Add) (lhs (Ref 21)) (rhs (Ref 23))))
       (25 (LoadOp (var __i32) (op Load32) (addr (Ref 24))))
       (26 (OutsideContext (var __ret_addr__) (typ Int)))
       (27 (BiOp (var __i32) (op Equal) (lhs (Ref 25)) (rhs (Ref 26))))
       (28 (AssertOp (condition (Ref 27))))))
     (terminator Return)
     (roots
      ((Ref 0) (Ref 3) (Ref 11) (Ref 19) (Ref 20) (Ref 21) (Ref 22) (Ref 24)
       (Ref 26) (Ref 28))))
    |}]

let%expect_test "tail indirect call" =
  test_trans_block 0x0047efac;
  [%expect
    {|
    ((id 8)
     (instrs
      ((0 (Const __i32 0)) (1 (OutsideContext (var esp) (typ Int)))
       (2 (Const __i32 4))
       (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
       (5 (OutsideContext (var ebx) (typ Int)))
       (6 (LoadOp (var eax) (op Load32) (addr (Ref 5)) (offset 20)))
       (7 (Const __i32 4))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
       (9 (Const __i32 4714417))
       (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
       (11 (OutsideContext (var ecx) (typ Int)))
       (12 (OutsideContext (var edx) (typ Int)))
       (13 (CallOp (func __func47f816__) (args ((Ref 11) (Ref 8) (Ref 12)))))
       (14 (ReturnedOp (var eax) (typ Int)))
       (15 (ReturnedOp (var esp) (typ Int)))
       (16 (ReturnedOp (var edx) (typ Int)))
       (17 (DupVar (var edx) (src (Ref 5)) (typ Int)))
       (18 (LoadOp (var ebx) (op Load32) (addr (Ref 17)) (offset 4)))
       (19 (LoadOp (var edi) (op Load32) (addr (Ref 17)) (offset 8)))
       (20 (LoadOp (var esi) (op Load32) (addr (Ref 17)) (offset 12)))
       (21 (LoadOp (var eax) (op Load32) (addr (Ref 15)) (offset 8)))
       (22 (Const __i32 1)) (23 (Const __i32 0))
       (24
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 21))
         (rhs (Ref 22))))
       (25 (BiOp (var __i32) (op Add) (lhs (Ref 24)) (rhs (Ref 23))))
       (26 (BiOp (var eax) (op Add) (lhs (Ref 25)) (rhs (Ref 21))))
       (27 (LoadOp (var esp) (op Load32) (addr (Ref 17)) (offset 16)))
       (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Add) (lhs (Ref 27)) (rhs (Ref 28))))
       (30 (LoadOp (var __i32) (op Load32) (addr (Ref 17)) (offset 20)))
       (31
        (CallIndirectOp (table_index (Ref 30))
         (args ((Ref 11) (Ref 29) (Ref 17)))))
       (32 (ReturnedOp (var eax) (typ Int)))
       (33 (ReturnedOp (var esp) (typ Int)))
       (34 (ReturnedOp (var edx) (typ Int)))))
     (terminator Return)
     (roots
      ((Ref 1) (Ref 4) (Ref 5) (Ref 10) (Ref 11) (Ref 12) (Ref 13) (Ref 14)
       (Ref 15) (Ref 16) (Ref 18) (Ref 19) (Ref 20) (Ref 31) (Ref 32) (Ref 33)
       (Ref 34))))
    |}]

let%expect_test "nonzero switch" =
  test_trans_block 0x0046af8f;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 20))
       (2 (BiOp (var __i32) (op Subtract) (lhs (Ref 0)) (rhs (Ref 1))))))
     (terminator
      (Switch
       (cases
        ((Block 6) (Block 8) (Block 10) (Block 12) (Block 14) (Block 16)
         (Block 18) (Block 20) (Block 22) (Block 24) (Block 26)))
       (default (Block 84)) (switch_on (Ref 2))))
     (roots ((Ref 0))))
    |}]

let%expect_test "fistp dword" =
  test_trans_block 0x00465929;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -32)))
       (3 (Const __i32 0))
       (4 (BiOp (var __i32) (op And) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 4)) (offset -32)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset -36)))
       (7 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 0)) (offset -36)))
       (8 (UniOp (var __fl) (op Int64ToFloatSigned) (operand (Ref 7))))
       (9 (OutsideContext (var ecx) (typ Int)))
       (10 (OutsideContext (var esp) (typ Int))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 9))))
       (14 (Const __i32 4))
       (15 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 9))))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (18 (BiOp (var __fl) (op FloatMult) (lhs (Ref 8)) (rhs (Ref 17))))
       (19 (Const __i32 4819536))
       (20 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 19))))
       (21 (BiOp (var __fl) (op FloatSub) (lhs (Ref 18)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 21)) (offset -8)))
       (23 (StoreOp (op FloatStore64) (addr (Ref 15)) (value (Ref 21))))
       (24 (Const __i32 4))
       (25 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 24))))
       (26 (Const __i32 4610332))
       (27 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 26))))
       (28 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (29 (OutsideContext (var edx) (typ Int)))
       (30 (CallOp (func __func47ee10__) (args ((Ref 9) (Ref 25) (Ref 29)))))
       (31 (ReturnedOp (var eax) (typ Int)))
       (32 (ReturnedOp (var esp) (typ Int)))
       (33 (ReturnedOp (var edx) (typ Int)))
       (34 (LoadOp (var ecx) (op Load32) (addr (Ref 32)))) (35 (Const __i32 4))
       (36 (BiOp (var esp) (op Add) (lhs (Ref 32)) (rhs (Ref 35))))
       (37 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (38 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 38)) (offset -12)))
       (40 (LoadOp (var ecx) (op Load32) (addr (Ref 36)))) (41 (Const __i32 4))
       (42 (BiOp (var esp) (op Add) (lhs (Ref 36)) (rhs (Ref 41))))
       (43 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (44 (UniOp (var __i32) (op FloatToInt32) (operand (Ref 43))))
       (45 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 44)) (offset -20)))
       (46 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -20)))
       (47 (Const __i32 1))
       (48 (BiOp (var ecx) (op Add) (lhs (Ref 46)) (rhs (Ref 47))))
       (49 (Const __i32 0))
       (50
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
         (lhs (Ref 46)) (rhs (Ref 49))))
       (51 (Const __i32 -8))
       (52 (BiOp (var __i32) (op Add) (lhs (Ref 37)) (rhs (Ref 51))))
       (53
        (SetGlobalOp (value (Ref 52)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 4)) (condition (Ref 50))))
     (roots
      ((Ref 0) (Ref 5) (Ref 6) (Ref 9) (Ref 10) (Ref 13) (Ref 16) (Ref 22)
       (Ref 23) (Ref 27) (Ref 28) (Ref 29) (Ref 30) (Ref 31) (Ref 32) (Ref 33)
       (Ref 37) (Ref 39) (Ref 42) (Ref 45) (Ref 46) (Ref 48) (Ref 53))))
    |}]

let%expect_test "fsubrp" =
  test_trans_block 0x0046336d;
  [%expect
    {|
    ((id 37)
     (instrs
      ((0 (FloatConst __fl 1))
       (1 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (2 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 1))))
       (3 (BiOp (var __fl) (op FloatSub) (lhs (Ref 0)) (rhs (Ref 2))))
       (4 (StoreOp (op FloatStore64) (addr (Ref 1)) (value (Ref 3))))))
     (terminator (Goto (Block 38))) (roots ((Ref 1) (Ref 4))))
    |}]

let%expect_test "psrlq, andpd, psubd" =
  test_trans_block 0x0047ee50;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (1 (OutsideContext (var esp) (typ Int)))
       (2 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 1)) (offset 4)))
       (3
        (VecReplaceLaneOp (var __vec) (dest (Ref 0)) (lane_value (Ref 2))
         (shape I64) (lane 0)))
       (4 (Const __i32 4785856))
       (5 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 4))))
       (6 (Const __i32 52))
       (7
        (VecShiftRightOp (var __vec) (operand (Ref 3)) (count (Ref 6))
         (shape I64) (signed false)))
       (8 (VecExtractLaneOp (var eax) (src (Ref 7)) (shape I32) (lane 0)))
       (9 (Const __i32 4785904))
       (10 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 9))))
       (11 (BiOp (var __vec) (op VecAnd) (lhs (Ref 7)) (rhs (Ref 10))))
       (12
        (VecLaneBiOp (var __vec) (op VecSub) (shape I32) (lhs (Ref 5))
         (rhs (Ref 11))))
       (13 (VecExtractLaneOp (var __i32) (src (Ref 12)) (shape I32) (lane 0)))
       (14
        (VecShiftRightOp (var __vec) (operand (Ref 3)) (count (Ref 13))
         (shape I64) (signed false)))
       (15 (Const __i32 2048))
       (16 (BiOp (var __i32) (op And) (lhs (Ref 8)) (rhs (Ref 15))))
       (17
        (SetGlobalOp (value (Ref 11))
         (global ((name __xmm0_global__) (typ Vec)))))
       (18
        (SetGlobalOp (value (Ref 12))
         (global ((name __xmm2_global__) (typ Vec)))))
       (19
        (SetGlobalOp (value (Ref 3)) (global ((name __xmm7_global__) (typ Vec)))))
       (20
        (SetGlobalOp (value (Ref 14))
         (global ((name __xmm1_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 12)) (fail (Block 6)) (condition (Ref 16))))
     (roots ((Ref 1) (Ref 8) (Ref 17) (Ref 18) (Ref 19) (Ref 20))))
    |}]

let%expect_test "movq, psllq, cmpltpd" =
  test_trans_block 0x0047eed2;
  [%expect
    {|
    ((id 12)
     (instrs
      ((0 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (1 (OutsideContext (var esp) (typ Int)))
       (2 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 1)) (offset 4)))
       (3
        (VecReplaceLaneOp (var __vec) (dest (Ref 0)) (lane_value (Ref 2))
         (shape I64) (lane 0)))
       (4 (GetGlobalOp (var __vec) (global ((name __xmm1_global__) (typ Vec)))))
       (5 (GetGlobalOp (var __vec) (global ((name __xmm2_global__) (typ Vec)))))
       (6 (VecExtractLaneOp (var __i32) (src (Ref 5)) (shape I32) (lane 0)))
       (7
        (VecShiftLeftOp (var __vec) (operand (Ref 4)) (count (Ref 6))
         (shape I64)))
       (8
        (SignedVecLaneBiOp (var __vec) (op VecLessThan) (signed true) (shape F64)
         (lhs (Ref 3)) (rhs (Ref 7))))
       (9 (OutsideContext (var eax) (typ Int))) (10 (Const __i32 3071))
       (11
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 9))
         (rhs (Ref 10))))
       (12
        (SetGlobalOp (value (Ref 3)) (global ((name __xmm3_global__) (typ Vec)))))
       (13
        (SetGlobalOp (value (Ref 8)) (global ((name __xmm0_global__) (typ Vec)))))
       (14
        (SetGlobalOp (value (Ref 7)) (global ((name __xmm1_global__) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 16)) (fail (Block 13)) (condition (Ref 11))))
     (roots ((Ref 1) (Ref 4) (Ref 5) (Ref 9) (Ref 12) (Ref 13) (Ref 14))))
    |}]

let%expect_test "branch return" =
  test_trans_block 0x0047ee10;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (Const __i32 20336900))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)))) (4 (Const __i32 0))
       (5 (BiOp (var __i32) (op Equal) (lhs (Ref 3)) (rhs (Ref 4))))))
     (terminator
      (Branch (succeed (Block 17)) (fail (Block 1)) (condition (Ref 5))))
     (roots ((Ref 0) (Ref 1))))
    |}]

let%expect_test "and jbe" =
  test_trans_block 0x0046632e;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
       (3 (OutsideContext (var esi) (typ Int))) (4 (Const __i32 4))
       (5 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 4))))
       (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 3))))
       (7 (OutsideContext (var ecx) (typ Int)))
       (8 (DupVar (var esi) (src (Ref 7)) (typ Int)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 8)) (offset 4)))
       (10 (Const __i32 0))
       (11 (BiOp (var __i32) (op And) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 11)) (offset 4)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 8)))) (14 (Const __i32 0))
       (15 (BiOp (var __i32) (op And) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 15))))
       (17 (DupVar (var ecx) (src (Ref 2)) (typ Int))) (18 (Const __i32 65535))
       (19 (BiOp (var ecx) (op And) (lhs (Ref 17)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 2)) (offset 8)))
       (21 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))))
     (terminator
      (Branch (succeed (Block 23)) (fail (Block 1)) (condition (Ref 21))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 12)
       (Ref 16) (Ref 19) (Ref 20))))
    |}]

(*let%expect_est "fprem1 / sahf jp" =*)
(*  test_trans_block 0x0048b987;*)
(*  [%expect {||}]*)
(**)
(*let%expect_test "fptan" =*)
(*  test_trans_block 0x0048b952;*)
(*  [%expect {||}]*)
(**)
(*let%expect_test "fnstcw" =*)
(*  test_trans_block 0x0048b93f;*)
(*  [%expect {||}]*)

let%expect_test "frndint" =
  test_trans_block 0x0044f059;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const __i32 4956072))
       (1 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0))))
       (2 (UniOp (var __fl) (op FloatRound) (operand (Ref 1))))
       (3 (Const __i32 4819536))
       (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3))))
       (5 (BiOp (var __fl) (op FloatSub) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (Const __i32 4956100))
       (7 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 6))))
       (8 (UniOp (var __fl) (op FloatRound) (operand (Ref 7))))
       (9 (Const __i32 4819536))
       (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9))))
       (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (Const __i32 4956076))
       (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12))))
       (14 (UniOp (var __fl) (op FloatRound) (operand (Ref 13))))
       (15 (Const __i32 4819536))
       (16 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 15))))
       (17 (BiOp (var __fl) (op FloatSub) (lhs (Ref 14)) (rhs (Ref 16))))
       (18 (Const __i32 4956132))
       (19 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 18))))
       (20 (UniOp (var __fl) (op FloatRound) (operand (Ref 19))))
       (21 (Const __i32 4819536))
       (22 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 21))))
       (23 (BiOp (var __fl) (op FloatSub) (lhs (Ref 20)) (rhs (Ref 22))))
       (24 (Const __i32 4956132))
       (25 (StoreOp (op Store32) (addr (Ref 24)) (value (Ref 23))))
       (26 (Const __i32 4956160))
       (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 23))))
       (28 (Const __i32 4956076))
       (29 (StoreOp (op Store32) (addr (Ref 28)) (value (Ref 17))))
       (30 (Const __i32 4956104))
       (31 (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 17))))
       (32 (Const __i32 4956100))
       (33 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 11))))
       (34 (Const __i32 4956156))
       (35 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 11))))
       (36 (Const __i32 4956072))
       (37 (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 5))))
       (38 (Const __i32 4956128))
       (39 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 5))))
       (40 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 2)))
     (roots
      ((Ref 25) (Ref 27) (Ref 29) (Ref 31) (Ref 33) (Ref 35) (Ref 37) (Ref 39)
       (Ref 40))))
    |}]

let%expect_test "test eax,eax jae" =
  test_trans_block 0x0045dbcd;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -52)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -44)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 108)))
       (4 (BiOp (var eax) (op Subtract) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 0))))
     (terminator
      (Branch (succeed (Block 8)) (fail (Block 6)) (condition (Ref 5))))
     (roots ((Ref 0) (Ref 1) (Ref 4))))
    |}]

let%expect_test "test eax,eax jbe" =
  test_trans_block 0x0047e73c;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 16)))
       (2 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 28)))
       (3 (UniOp (var __i32) (op EqualsZero) (operand (Ref 1))))))
     (terminator
      (Branch (succeed (Block 16)) (fail (Block 3)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 1) (Ref 2))))
    |}]

let%expect_test "shld" =
  test_trans_block 0x00481fa8;
  [%expect
    {|
    ((id 211)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -92)))
       (2 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -88)))
       (3 (Const __i32 3))
       (4 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 29))
       (6
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 1))
         (rhs (Ref 5))))
       (7 (BiOp (var ecx) (op Or) (lhs (Ref 4)) (rhs (Ref 6))))
       (8 (Const __i32 3))
       (9 (BiOp (var eax) (op ShiftLeft) (lhs (Ref 1)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 9)) (offset -92)))
       (11 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -88)))))
     (terminator (Goto (Block 217)))
     (roots ((Ref 0) (Ref 7) (Ref 9) (Ref 10) (Ref 11))))
    |}]

let%expect_test "repne scasb" =
  test_trans_block 0x0047dcc4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var ecx) (typ Int)))
       (1 (DupVar (var ebx) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var ebp) (typ Int)))
       (3 (LoadOp (var edi) (op Load32) (addr (Ref 2)) (offset 8)))
       (4 (DupVar (var esi) (src (Ref 3)) (typ Int))) (5 (Const eax 0))
       (6 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 5))))
       (7 (CallOp (func __find_byte__) (args ((Ref 6) (Ref 3) (Ref 0)))))
       (8 (ReturnedOp (var ecx) (typ Int))) (9 (Const __i32 0))
       (10 (BiOp (var ecx) (op Subtract) (lhs (Ref 9)) (rhs (Ref 8))))
       (11 (BiOp (var ecx) (op Add) (lhs (Ref 10)) (rhs (Ref 1))))
       (12 (DupVar (var edi) (src (Ref 4)) (typ Int)))
       (13 (LoadOp (var esi) (op Load32) (addr (Ref 2)) (offset 12)))
       (14 (CallOp (func __byte_diff__) (args ((Ref 13) (Ref 12) (Ref 11)))))
       (15 (ReturnedOp (var esi) (typ Int)))
       (16 (ReturnedOp (var edi) (typ Int)))
       (17 (ReturnedOp (var ecx) (typ Int)))
       (18 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 15)) (signed false)))
       (19 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 16)) (signed false)))
       (20
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 15)) (signed false)
         (offset -1)))
       (21 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 20))))
       (22 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 21)) (rhs (Ref 5))))
       (23 (Const ecx 0))
       (24
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 16)) (signed false)
         (offset -1)))
       (25 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 22))))
       (26 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 24))))
       (27
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 25))
         (rhs (Ref 26))))
       (28 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 22))))
       (29 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 24))))
       (30 (BiOp (var __i32) (op Equal) (lhs (Ref 28)) (rhs (Ref 29))))
       (31 (DupVar (var __input_compare_arg__) (src (Ref 30)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 2)) (condition (Ref 27))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 7) (Ref 8) (Ref 14) (Ref 15) (Ref 16)
       (Ref 17) (Ref 22) (Ref 23) (Ref 31))))
    |}]

let%expect_test "jecxz" =
  test_trans_block 0x0047dcb9;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
       (6 (DupVar (var ebp) (src (Ref 4)) (typ Int)))
       (7 (OutsideContext (var edi) (typ Int))) (8 (Const __i32 4))
       (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
       (11 (OutsideContext (var esi) (typ Int))) (12 (Const __i32 4))
       (13 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 11))))
       (15 (OutsideContext (var ebx) (typ Int))) (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15))))
       (19 (LoadOp (var ecx) (op Load32) (addr (Ref 6)) (offset 16)))
       (20 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 1)) (condition (Ref 20))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 7) (Ref 10) (Ref 11)
       (Ref 14) (Ref 15) (Ref 17) (Ref 18) (Ref 19))))
    |}]

let%expect_test "double fadd" =
  test_trans_block 0x00444017;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (Const __i32 6447736))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0))))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1))))
       (3 (OutsideContext (var ebp) (typ Int)))
       (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2)) (offset -156)))
       (5 (Const __i32 5724880))
       (6 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 5))))
       (7 (Const __i32 5724884))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (Const __i32 4819536))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
       (12 (BiOp (var __fl) (op FloatSub) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 12)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 13)) (offset -20)))
       (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3)) (offset -20)))
       (16 (Const __i32 4819532))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 16))))
       (18 (Landmine (var eax) (typ Int))) (19 (Const __i32 5))
       (20 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 15)) (rhs (Ref 17))))
       (21 (UniOp (var __i32) (op EqualsZero) (operand (Ref 20))))
       (22 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 21))))
     (roots ((Ref 1) (Ref 2) (Ref 3) (Ref 4) (Ref 14) (Ref 18) (Ref 22))))
    |}]

let%expect_test "rcr" =
  test_trans_block 0x00482a92;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var ecx) (typ Int))) (1 (Const __i32 1))
       (2
        (SignedBiOp (var ecx) (op ShiftRight) (signed false) (lhs (Ref 0))
         (rhs (Ref 1))))
       (3 (Const __i32 32)) (4 (AssertOp (condition (Ref 1))))
       (5 (BiOp (var __i32) (op Subtract) (lhs (Ref 3)) (rhs (Ref 1))))
       (6 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 5))))
       (7 (OutsideContext (var ebx) (typ Int))) (8 (Const __i32 1))
       (9
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 7))
         (rhs (Ref 8))))
       (10 (BiOp (var ebx) (op Or) (lhs (Ref 6)) (rhs (Ref 9))))
       (11 (OutsideContext (var edx) (typ Int))) (12 (Const __i32 1))
       (13
        (SignedBiOp (var edx) (op ShiftRight) (signed false) (lhs (Ref 11))
         (rhs (Ref 12))))
       (14 (Const __i32 32)) (15 (AssertOp (condition (Ref 12))))
       (16 (BiOp (var __i32) (op Subtract) (lhs (Ref 14)) (rhs (Ref 12))))
       (17 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 11)) (rhs (Ref 16))))
       (18 (OutsideContext (var eax) (typ Int))) (19 (Const __i32 1))
       (20
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 18))
         (rhs (Ref 19))))
       (21 (BiOp (var eax) (op Or) (lhs (Ref 17)) (rhs (Ref 20))))
       (22 (BiOp (var ecx) (op Or) (lhs (Ref 2)) (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 22))))
     (roots
      ((Ref 0) (Ref 4) (Ref 7) (Ref 10) (Ref 11) (Ref 13) (Ref 15) (Ref 18)
       (Ref 21) (Ref 22))))
    |}]

let%expect_test "dumb div" =
  test_trans_block 0x00482a7c;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 20)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 16)))
       (3 (Const edx 0))
       (4
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 2))
         (rhs (Ref 1))))
       (5
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 2))
         (rhs (Ref 1))))
       (6 (DupVar (var ebx) (src (Ref 4)) (typ Int)))
       (7 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
       (8 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 7))))
       (9 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 5))))
       (10 (LongConst __i64 32))
       (11 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (BiOp (var __i64) (op LongOr) (lhs (Ref 8)) (rhs (Ref 11))))
       (13 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (14
        (SignedBiOp (var __i64) (op LongDivide) (signed false) (lhs (Ref 12))
         (rhs (Ref 13))))
       (15 (UniOp (var eax) (op LongToInt32) (operand (Ref 14))))
       (16
        (SignedBiOp (var __i64) (op LongRemainder) (signed false) (lhs (Ref 12))
         (rhs (Ref 13))))
       (17 (UniOp (var edx) (op LongToInt32) (operand (Ref 16))))
       (18 (DupVar (var edx) (src (Ref 6)) (typ Int)))))
     (terminator (Goto (Block 10)))
     (roots ((Ref 0) (Ref 1) (Ref 6) (Ref 15) (Ref 18))))
    |}]

let%expect_test "mul" =
  test_trans_block 0x004816d4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 4)))
       (2 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (3 (OutsideContext (var ecx) (typ Int)))
       (4 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 3))))
       (5 (BiOp (var __i64) (op LongMultiply) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (UniOp (var eax) (op LongToInt32) (operand (Ref 5))))
       (7 (LongConst __i64 32))
       (8
        (SignedBiOp (var __i64) (op LongShiftRight) (signed false) (lhs (Ref 5))
         (rhs (Ref 7))))
       (9 (UniOp (var edx) (op LongToInt32) (operand (Ref 8))))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (11 (OutsideContext (var __ret_addr__) (typ Int)))
       (12 (BiOp (var __i32) (op Equal) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 16))
       (15 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 14))))))
     (terminator Return)
     (roots ((Ref 0) (Ref 3) (Ref 6) (Ref 9) (Ref 11) (Ref 13) (Ref 15))))
    |}]

let%expect_test "stosw" =
  test_trans_block 0x00439633;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
       (6 (DupVar (var ebp) (src (Ref 4)) (typ Int))) (7 (Const __i32 80))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (OutsideContext (var edi) (typ Int))) (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 9))))
       (13 (OutsideContext (var ecx) (typ Int)))
       (14 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 13)) (offset -76)))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -24)))
       (16 (Const __i32 0))
       (17 (BiOp (var __i32) (op And) (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 17)) (offset -24)))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -20)))
       (20 (Const __i32 0))
       (21 (BiOp (var __i32) (op And) (lhs (Ref 19)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 21)) (offset -20)))
       (23 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -28)))
       (24 (Const __i32 0))
       (25 (BiOp (var __i32) (op And) (lhs (Ref 23)) (rhs (Ref 24))))
       (26 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 25)) (offset -28)))
       (27 (Const __i32 -28))
       (28 (BiOp (var eax) (op Add) (lhs (Ref 6)) (rhs (Ref 27))))
       (29 (Const __i32 4))
       (30 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 29))))
       (31 (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 28))))
       (32 (Const __i32 0)) (33 (Const __i32 4))
       (34 (BiOp (var esp) (op Subtract) (lhs (Ref 30)) (rhs (Ref 33))))
       (35 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 32))))
       (36 (Const __i32 0)) (37 (Const __i32 4))
       (38 (BiOp (var esp) (op Subtract) (lhs (Ref 34)) (rhs (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 36))))
       (40 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -76)))
       (41 (LoadOp (var eax) (op Load32) (addr (Ref 40)) (offset 8)))
       (42 (LoadOp (var ecx) (op Load32) (addr (Ref 6)) (offset -76)))
       (43 (LoadOp (var ecx) (op Load32) (addr (Ref 42)) (offset 8)))
       (44 (LoadOp (var eax) (op Load32) (addr (Ref 41)))) (45 (Const __i32 4))
       (46 (BiOp (var esp) (op Subtract) (lhs (Ref 38)) (rhs (Ref 45))))
       (47 (StoreOp (op Store32) (addr (Ref 46)) (value (Ref 43))))
       (48 (Const __i32 64))
       (49 (BiOp (var __i32) (op Add) (lhs (Ref 44)) (rhs (Ref 48))))
       (50 (Const __i32 4))
       (51 (BiOp (var esp) (op Subtract) (lhs (Ref 46)) (rhs (Ref 50))))
       (52 (Const __i32 4429352))
       (53 (StoreOp (op Store32) (addr (Ref 51)) (value (Ref 52))))
       (54 (OutsideContext (var edx) (typ Int)))
       (55
        (CallIndirectOp (table_index (Ref 49))
         (args ((Ref 43) (Ref 51) (Ref 54)))))
       (56 (ReturnedOp (var eax) (typ Int)))
       (57 (ReturnedOp (var esp) (typ Int)))
       (58 (ReturnedOp (var edx) (typ Int))) (59 (Const eax 0))
       (60 (Const __i32 -16))
       (61 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 60))))
       (62 (StoreOp (op Store32) (addr (Ref 61)) (value (Ref 59))))
       (63 (Const __i32 4))
       (64 (BiOp (var edi) (op Add) (lhs (Ref 61)) (rhs (Ref 63))))
       (65 (StoreOp (op Store32) (addr (Ref 64)) (value (Ref 59))))
       (66 (Const __i32 4))
       (67 (BiOp (var edi) (op Add) (lhs (Ref 64)) (rhs (Ref 66))))
       (68 (StoreOp (op Store32) (addr (Ref 67)) (value (Ref 59))))
       (69 (Const __i32 4))
       (70 (BiOp (var edi) (op Add) (lhs (Ref 67)) (rhs (Ref 69))))
       (71 (UniOp (var __i32) (op SignExtend16) (operand (Ref 59))))
       (72 (StoreOp (op Store16) (addr (Ref 70)) (value (Ref 71))))
       (73 (Const __i32 2))
       (74 (BiOp (var edi) (op Add) (lhs (Ref 70)) (rhs (Ref 73))))
       (75 (Const __i32 4812696))
       (76 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 75)) (signed false)))
       (77 (UniOp (var __i32) (op SignExtend16) (operand (Ref 76))))
       (78 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 77)) (rhs (Ref 59))))
       (79 (UniOp (var __i32) (op SignExtend16) (operand (Ref 78))))
       (80 (StoreOp (op Store16) (addr (Ref 6)) (value (Ref 79)) (offset -16)))
       (81 (Const __i32 54))
       (82 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 81)) (offset -6)))
       (83 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -6)))
       (84 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 83)) (offset -14)))
       (85 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -76)))
       (86 (LoadOp (var eax) (op Load32) (addr (Ref 85)) (offset 232)))
       (87 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 86)) (offset -80)))
       (88 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -80)))
       (89 (Const __i32 22))
       (90 (BiOp (var __i32) (op Equal) (lhs (Ref 88)) (rhs (Ref 89))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 90))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 9) (Ref 12) (Ref 13)
       (Ref 14) (Ref 18) (Ref 22) (Ref 26) (Ref 31) (Ref 35) (Ref 39) (Ref 43)
       (Ref 47) (Ref 53) (Ref 54) (Ref 55) (Ref 56) (Ref 57) (Ref 58) (Ref 62)
       (Ref 65) (Ref 68) (Ref 72) (Ref 74) (Ref 80) (Ref 82) (Ref 84) (Ref 86)
       (Ref 87))))
    |}]

let%expect_test "tail intrinsic" =
  test_trans_block 0x0047d136;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (CallOp (func USER32.dll_WINNLSEnableIME) (args ((Ref 0)))))
       (3 (ReturnedOp (var eax) (typ Int))) (4 (ReturnedOp (var esp) (typ Int)))
       (5 (ReturnedOp (var edx) (typ Int)))))
     (terminator Return)
     (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4) (Ref 5)))) |}]

let%expect_test "movsw" =
  test_trans_block 0x004399e1;
  [%expect
    {|
    ((id 9)
     (instrs
      ((0 (Const __i32 1))
       (1 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (2 (Const __i32 5724808))
       (3 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 1))))
       (4 (Const __i32 1))
       (5 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 4))))
       (6 (Const __i32 5724809))
       (7 (StoreOp (op Store8) (addr (Ref 6)) (value (Ref 5))))
       (8 (Const __i32 5724810))
       (9 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 8)) (signed false)))
       (10 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 9))))
       (11 (Const __i32 0))
       (12 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 11))))
       (13 (BiOp (var __i32) (op And) (lhs (Ref 10)) (rhs (Ref 12))))
       (14 (Const __i32 5724810))
       (15 (StoreOp (op Store8) (addr (Ref 14)) (value (Ref 13))))
       (16 (Const __i32 5724811))
       (17 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 16)) (signed false)))
       (18 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 17))))
       (19 (Const __i32 0))
       (20 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 19))))
       (21 (BiOp (var __i32) (op And) (lhs (Ref 18)) (rhs (Ref 20))))
       (22 (Const __i32 5724811))
       (23 (StoreOp (op Store8) (addr (Ref 22)) (value (Ref 21))))
       (24 (Const esi 4845120)) (25 (Const edi 5724776))
       (26 (LoadOp (var __i32) (op Load32) (addr (Ref 24))))
       (27 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 26))))
       (28 (Const __i32 4))
       (29 (BiOp (var edi) (op Add) (lhs (Ref 25)) (rhs (Ref 28))))
       (30 (Const __i32 4))
       (31 (BiOp (var esi) (op Add) (lhs (Ref 24)) (rhs (Ref 30))))
       (32 (LoadOp (var __i32) (op Load32) (addr (Ref 31))))
       (33 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 32))))
       (34 (Const __i32 4))
       (35 (BiOp (var edi) (op Add) (lhs (Ref 29)) (rhs (Ref 34))))
       (36 (Const __i32 4))
       (37 (BiOp (var esi) (op Add) (lhs (Ref 31)) (rhs (Ref 36))))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 38))))
       (40 (Const __i32 4))
       (41 (BiOp (var edi) (op Add) (lhs (Ref 35)) (rhs (Ref 40))))
       (42 (Const __i32 4))
       (43 (BiOp (var esi) (op Add) (lhs (Ref 37)) (rhs (Ref 42))))
       (44 (LoadOp (var __i32) (op Load32) (addr (Ref 43))))
       (45 (StoreOp (op Store32) (addr (Ref 41)) (value (Ref 44))))
       (46 (Const __i32 4))
       (47 (BiOp (var edi) (op Add) (lhs (Ref 41)) (rhs (Ref 46))))
       (48 (Const __i32 4))
       (49 (BiOp (var esi) (op Add) (lhs (Ref 43)) (rhs (Ref 48))))
       (50 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 49)) (signed false)))
       (51 (UniOp (var __i32) (op SignExtend16) (operand (Ref 50))))
       (52 (StoreOp (op Store16) (addr (Ref 47)) (value (Ref 51))))
       (53 (Const __i32 2))
       (54 (BiOp (var edi) (op Add) (lhs (Ref 47)) (rhs (Ref 53))))
       (55 (Const __i32 2))
       (56 (BiOp (var esi) (op Add) (lhs (Ref 49)) (rhs (Ref 55))))
       (57 (Const __i32 2))
       (58 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 57))))
       (59 (Const __i32 5724812))
       (60 (StoreOp (op Store8) (addr (Ref 59)) (value (Ref 58))))
       (61 (Const __i32 5724813))
       (62 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 61)) (signed false)))
       (63 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 62))))
       (64 (Const __i32 0))
       (65 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 64))))
       (66 (BiOp (var __i32) (op And) (lhs (Ref 63)) (rhs (Ref 65))))
       (67 (Const __i32 5724813))
       (68 (StoreOp (op Store8) (addr (Ref 67)) (value (Ref 66))))
       (69 (Const __i32 1))
       (70 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 69))))
       (71 (Const __i32 5724814))
       (72 (StoreOp (op Store8) (addr (Ref 71)) (value (Ref 70))))))
     (terminator (Goto (Block 30)))
     (roots
      ((Ref 3) (Ref 7) (Ref 15) (Ref 23) (Ref 27) (Ref 33) (Ref 39) (Ref 45)
       (Ref 52) (Ref 54) (Ref 56) (Ref 60) (Ref 68) (Ref 72))))
    |}]

let%expect_test "sub jne" =
  test_trans_block 0x0047dc43;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int)))
       (1 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (2 (OutsideContext (var edi) (typ Int)))
       (3 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 1))))
       (4 (Const __i32 1))
       (5 (BiOp (var edi) (op Add) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 1))
       (8 (BiOp (var edx) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))))
     (terminator
      (Branch (succeed (Block 7)) (fail (Block 8)) (condition (Ref 8))))
     (roots ((Ref 0) (Ref 2) (Ref 3) (Ref 5) (Ref 6) (Ref 8))))
    |}]

let%expect_test "rep stosd (nonzero)" =
  test_trans_block 0x0042d5ee;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const ecx 180)) (1 (OutsideContext (var eax) (typ Int)))
       (2 (Const __i32 4294967295))
       (3 (BiOp (var eax) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (Const edi 5724496))
       (5 (CallOp (func __int_memset__) (args ((Ref 4) (Ref 3) (Ref 0)))))
       (6 (Const ecx 0))))
     (terminator (Goto (Block 2)))
     (roots ((Ref 1) (Ref 3) (Ref 4) (Ref 5) (Ref 6))))
    |}]

let%expect_test "rep movsb" =
  test_trans_block 0x004446bb;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (3 (Const __i32 4))
       (4 (BiOp (var __i32) (op Multiply) (lhs (Ref 1)) (rhs (Ref 3))))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 2))))
       (6 (LoadOp (var ecx) (op Load32) (addr (Ref 5)) (offset 36)))
       (7 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 6)) (offset -24)))
       (8 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -24)))
       (9 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -8)))
       (10 (LoadOp (var eax) (op Load32) (addr (Ref 9)) (offset 4)))
       (11 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (12 (Const __i32 4))
       (13 (BiOp (var __i32) (op Multiply) (lhs (Ref 11)) (rhs (Ref 12))))
       (14 (BiOp (var __i32) (op Add) (lhs (Ref 13)) (rhs (Ref 10))))
       (15 (LoadOp (var esi) (op Load32) (addr (Ref 14)) (offset 56)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -272)))
       (17 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -28)))
       (18 (BiOp (var __i32) (op Add) (lhs (Ref 16)) (rhs (Ref 17))))
       (19 (Const __i32 -84))
       (20 (BiOp (var edi) (op Add) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (DupVar (var eax) (src (Ref 8)) (typ Int))) (22 (Const __i32 2))
       (23 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 22))))
       (24
        (SignedBiOp (var ecx) (op ShiftRight) (signed false) (lhs (Ref 8))
         (rhs (Ref 23))))
       (25 (Const __i32 2))
       (26 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (Memcopy (count (Ref 26)) (src (Ref 15)) (dest (Ref 20))))
       (28 (BiOp (var esi) (op Add) (lhs (Ref 15)) (rhs (Ref 26))))
       (29 (BiOp (var edi) (op Add) (lhs (Ref 20)) (rhs (Ref 26))))
       (30 (Const ecx 0)) (31 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
       (32 (Const __i32 3))
       (33 (BiOp (var ecx) (op And) (lhs (Ref 31)) (rhs (Ref 32))))
       (34 (Memcopy (count (Ref 33)) (src (Ref 28)) (dest (Ref 29))))
       (35 (BiOp (var esi) (op Add) (lhs (Ref 28)) (rhs (Ref 33))))
       (36 (BiOp (var edi) (op Add) (lhs (Ref 29)) (rhs (Ref 33))))
       (37 (Const ecx 0))
       (38 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (39 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -272)))
       (40 (Const __i32 4))
       (41 (BiOp (var __i32) (op Multiply) (lhs (Ref 38)) (rhs (Ref 40))))
       (42 (BiOp (var __i32) (op Add) (lhs (Ref 41)) (rhs (Ref 0))))
       (43 (StoreOp (op Store32) (addr (Ref 42)) (value (Ref 39)) (offset -208)))
       (44 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -272)))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (46 (BiOp (var eax) (op Add) (lhs (Ref 44)) (rhs (Ref 45))))
       (47 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 46)) (offset -272)))))
     (terminator (Goto (Block 21)))
     (roots
      ((Ref 0) (Ref 7) (Ref 27) (Ref 34) (Ref 35) (Ref 36) (Ref 38) (Ref 39)
       (Ref 43) (Ref 46) (Ref 47))))
    |}]

let%expect_test "repe cmpsd" =
  test_trans_block 0x00444521;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (Const ecx 14)) (1 (OutsideContext (var ebp) (typ Int)))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1)) (offset -8)))
       (3 (LoadOp (var edi) (op Load32) (addr (Ref 2)) (offset 4)))
       (4 (Const __i32 112))
       (5 (BiOp (var edi) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (Const esi 5724776)) (7 (Const eax 0))
       (8 (CallOp (func __int_diff__) (args ((Ref 6) (Ref 5) (Ref 0)))))
       (9 (ReturnedOp (var esi) (typ Int))) (10 (ReturnedOp (var edi) (typ Int)))
       (11 (ReturnedOp (var ecx) (typ Int)))
       (12 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (14 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 13))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 4)) (condition (Ref 14))))
     (roots ((Ref 1) (Ref 2) (Ref 7) (Ref 8) (Ref 9) (Ref 10) (Ref 11))))
    |}]

let%expect_test "and jns" =
  test_trans_block 0x0043dbb0;
  [%expect
    {|
    ((id 18)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1)) (offset 836)))
       (3 (Const __i32 2147483649))
       (4 (BiOp (var edx) (op And) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 4))
         (rhs (Ref 5))))))
     (terminator
      (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 6))))
     (roots ((Ref 0) (Ref 1) (Ref 4))))
    |}]

let%expect_test "fidivr" =
  test_trans_block 0x00414d50;
  [%expect
    {|
    ((id 818)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 11196)))
       (3 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 2))))
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1656)))
       (5 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 4))))
       (6 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 5)) (rhs (Ref 3))))
       (7 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 6)) (offset -616)))
       (8 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 8)) (offset 11196)))
       (10 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 9))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1660)))
       (12 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 11))))
       (13 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 12)) (rhs (Ref 10))))
       (14 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 13)) (offset -620)))
       (15 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -60)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -620)))
       (17 (Const __i32 4))
       (18 (BiOp (var __i32) (op Multiply) (lhs (Ref 15)) (rhs (Ref 17))))
       (19
        (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 16)) (offset 4848708)))
       (20 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (21 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -616)))
       (22 (Const __i32 4))
       (23 (BiOp (var __i32) (op Multiply) (lhs (Ref 20)) (rhs (Ref 22))))
       (24
        (StoreOp (op Store32) (addr (Ref 23)) (value (Ref 21)) (offset 4848676)))
       (25 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (26
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 25)) (signed false)
         (offset 10)))
       (27 (UniOp (var ecx) (op ZeroExtend16) (operand (Ref 26))))
       (28 (Const __i32 8))
       (29 (BiOp (var ecx) (op And) (lhs (Ref 27)) (rhs (Ref 28))))
       (30 (UniOp (var __i32) (op EqualsZero) (operand (Ref 29))))
       (31 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 820)) (fail (Block 819)) (condition (Ref 30))))
     (roots
      ((Ref 0) (Ref 7) (Ref 14) (Ref 19) (Ref 21) (Ref 24) (Ref 25) (Ref 29)
       (Ref 31))))
    |}]

let%expect_test "tib offset 0" =
  test_trans_block 0x0043009a;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
       (6 (DupVar (var ebp) (src (Ref 4)) (typ Int))) (7 (Const __i32 -1))
       (8 (Const __i32 4))
       (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
       (11 (Const __i32 4768955)) (12 (Const __i32 4))
       (13 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 11))))
       (15 (GetGlobalOp (var eax) (global ((name __seh_frame__) (typ Int)))))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15))))
       (19
        (SetGlobalOp (value (Ref 17)) (global ((name __seh_frame__) (typ Int)))))
       (20 (Const __i32 20))
       (21 (BiOp (var esp) (op Subtract) (lhs (Ref 17)) (rhs (Ref 20))))
       (22 (OutsideContext (var ecx) (typ Int)))
       (23 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 22)) (offset -28)))
       (24 (Const __i32 32)) (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 24))))
       (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 26)) (rhs (Ref 28))))
       (30 (Const __i32 4391088))
       (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
       (32 (OutsideContext (var edx) (typ Int)))
       (33 (CallOp (func __func47d441__) (args ((Ref 22) (Ref 29) (Ref 32)))))
       (34 (ReturnedOp (var eax) (typ Int)))
       (35 (ReturnedOp (var esp) (typ Int)))
       (36 (ReturnedOp (var edx) (typ Int))) (37 (Const __i32 4))
       (38 (BiOp (var esp) (op Add) (lhs (Ref 35)) (rhs (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 34)) (offset -24)))
       (40 (Const __i32 0))
       (41 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 40)) (offset -4)))
       (42 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -24)))
       (43 (Const __i32 0))
       (44 (BiOp (var __i32) (op Equal) (lhs (Ref 42)) (rhs (Ref 43))))))
     (terminator
      (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 44))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 10) (Ref 14) (Ref 15)
       (Ref 18) (Ref 19) (Ref 22) (Ref 23) (Ref 27) (Ref 31) (Ref 32) (Ref 33)
       (Ref 34) (Ref 35) (Ref 36) (Ref 38) (Ref 39) (Ref 41))))
    |}]

let%expect_test "tail call" =
  test_trans 0x0047d43c;
  [%expect
    {|
    ((name func_47d43c)
     (signature
      ((args
        (((name ecx) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))
       (returns
        (((name eax) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (OutsideContext (var ecx) (typ Int)))
          (3 (OutsideContext (var edx) (typ Int)))
          (4 (CallOp (func __func47d285__) (args ((Ref 2) (Ref 0) (Ref 3)))))
          (5 (ReturnedOp (var eax) (typ Int)))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (ReturnedOp (var edx) (typ Int)))))
        (terminator Return)
        (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4) (Ref 5) (Ref 6) (Ref 7))))))
     (locals
      ((__ret_addr__ ((name __ret_addr__) (typ Int)))
       (eax ((name eax) (typ Int))) (ecx ((name ecx) (typ Int)))
       (edx ((name edx) (typ Int))) (esp ((name esp) (typ Int)))))) |}]

let%expect_test "shl reg" =
  test_trans_block 0x0040f3de;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
         (offset 8)))
       (2 (UniOp (var eax) (op ZeroExtend16) (operand (Ref 1))))
       (3 (Const edx 1))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 12)))
       (5 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 4))))
       (6 (BiOp (var edx) (op ShiftLeft) (lhs (Ref 3)) (rhs (Ref 5))))
       (7 (BiOp (var eax) (op And) (lhs (Ref 2)) (rhs (Ref 6))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 7))))
     (roots ((Ref 0) (Ref 4) (Ref 6) (Ref 7))))
    |}]

let%expect_test "cld" =
  test_trans_block 0x0048c237;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int))) (1 (Const __i32 8))
       (2 (BiOp (var esi) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (Const __i32 -134))
       (4 (BiOp (var edi) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
       (7 (Const __i32 4))
       (8 (BiOp (var edi) (op Add) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (Const __i32 4))
       (10 (BiOp (var esi) (op Add) (lhs (Ref 2)) (rhs (Ref 9))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var edi) (op Add) (lhs (Ref 8)) (rhs (Ref 13))))
       (15 (Const __i32 4))
       (16 (BiOp (var esi) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
       (17 (OutsideContext (var ebx) (typ Int)))
       (18
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 17)) (signed false)
         (offset 12)))
       (19 (Const __i32 1))
       (20 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 18))))
       (21 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 19))))
       (22 (BiOp (var __i32) (op Equal) (lhs (Ref 20)) (rhs (Ref 21))))))
     (terminator
      (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 22))))
     (roots ((Ref 0) (Ref 6) (Ref 12) (Ref 14) (Ref 16) (Ref 17))))
    |}]

let%expect_test "fscale/fabs/fcomp sahf jae" =
  test_trans_block 0x0048c1e9;
  [%expect
    {|
    ((id 15)
     (instrs
      ((0 (Const __i32 4)) (1 (OutsideContext (var ebp) (typ Int)))
       (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -142)))
       (3 (Const __i32 4820792))
       (4 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 3))))
       (5 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
       (7 (CallOp (func __float_scale__) (args ((Ref 6) (Ref 4)))))
       (8 (ReturnedOp (var __fl) (typ Float)))
       (9 (UniOp (var __fl) (op FloatAbs) (operand (Ref 8))))
       (10 (Const __i32 4820776))
       (11 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 10))))
       (12 (Landmine (var eax) (typ Int)))
       (13
        (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 9))
         (rhs (Ref 11))))
       (14 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 8))))))
     (terminator
      (Branch (succeed (Block 19)) (fail (Block 16)) (condition (Ref 13))))
     (roots ((Ref 1) (Ref 2) (Ref 5) (Ref 7) (Ref 8) (Ref 12) (Ref 14))))
    |}]

let%expect_test "fscale/fabs/fcomp sahf jbe" =
  test_trans_block 0x0048c217;
  [%expect
    {|
    ((id 17)
     (instrs
      ((0 (Const __i32 3)) (1 (OutsideContext (var ebp) (typ Int)))
       (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -142)))
       (3 (Const __i32 4820784))
       (4 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 3))))
       (5 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
       (7 (CallOp (func __float_scale__) (args ((Ref 6) (Ref 4)))))
       (8 (ReturnedOp (var __fl) (typ Float)))
       (9 (UniOp (var __fl) (op FloatAbs) (operand (Ref 8))))
       (10 (Const __i32 4820768))
       (11 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 10))))
       (12 (Landmine (var eax) (typ Int)))
       (13 (BiOp (var __i32) (op FloatGreaterThan) (lhs (Ref 9)) (rhs (Ref 11))))
       (14 (UniOp (var __i32) (op EqualsZero) (operand (Ref 13))))
       (15 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 8))))))
     (terminator
      (Branch (succeed (Block 19)) (fail (Block 18)) (condition (Ref 14))))
     (roots ((Ref 1) (Ref 2) (Ref 5) (Ref 7) (Ref 8) (Ref 12) (Ref 15))))
    |}]

let%expect_test "and je" =
  test_trans_block 0x0048c178;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
         (offset -164)))
       (2 (UniOp (var __i32) (op SignExtend16) (operand (Ref 1))))
       (3 (OutsideContext (var eax) (typ Int)))
       (4 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (UniOp (var __i32) (op SignExtend16) (operand (Ref 4))))
       (6 (Const __i32 32))
       (7 (UniOp (var __i32) (op SignExtend16) (operand (Ref 6))))
       (8 (BiOp (var __i32) (op And) (lhs (Ref 5)) (rhs (Ref 7))))
       (9 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 8)) (rhs (Ref 4))))
       (10 (UniOp (var __i32) (op SignExtend16) (operand (Ref 8))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 7)) (condition (Ref 10))))
     (roots ((Ref 0) (Ref 3) (Ref 9)))) |}]

let%expect_test "or je" =
  test_trans_block 0x0048c15b;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int)))
       (1 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (2 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (3 (BiOp (var __i32) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 3)) (rhs (Ref 0))))
       (5 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 3))))
       (6 (UniOp (var __i32) (op EqualsZero) (operand (Ref 5))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 6))))
     (roots ((Ref 0) (Ref 4)))) |}]

let%expect_test "xchg" =
  test_trans_block 0x0047f3b0;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 0))
       (2 (BiOp (var eax) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
       (3 (OutsideContext (var esp) (typ Int)))
       (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 4))
       (6 (BiOp (var eax) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 6))))
       (8 (DupVar (var esp) (src (Ref 6)) (typ Int)))
       (9 (DupVar (var eax) (src (Ref 3)) (typ Int)))
       (10 (LoadOp (var eax) (op Load32) (addr (Ref 9)))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
       (14 (LoadOp (var __i32) (op Load32) (addr (Ref 12))))
       (15 (OutsideContext (var __ret_addr__) (typ Int)))
       (16 (BiOp (var __i32) (op Equal) (lhs (Ref 14)) (rhs (Ref 15))))
       (17 (AssertOp (condition (Ref 16))))))
     (terminator Return)
     (roots ((Ref 0) (Ref 3) (Ref 10) (Ref 12) (Ref 13) (Ref 15) (Ref 17))))
    |}]

let%expect_test "fsqrt" =
  test_trans_block 0x00461f7b;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (CallOp (func __float_sqrt__) (args ((Ref 1)))))
       (3 (ReturnedOp (var __fl) (typ Float))) (4 (Const __i32 4819540))
       (5 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 4))))
       (6 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 5)) (rhs (Ref 3))))
       (7 (OutsideContext (var esi) (typ Int)))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (OutsideContext (var ebp) (typ Int)))
       (11 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 9)) (offset -12)))
       (12 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7)) (offset 4)))
       (13 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 13)) (offset -8)))
       (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7)) (offset 8)))
       (16 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 15))))
       (17 (Const __i32 -12))
       (18 (BiOp (var esi) (op Add) (lhs (Ref 10)) (rhs (Ref 17))))
       (19 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 16)) (offset -4)))
       (20 (Const __i32 -8))
       (21 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 20))))
       (22
        (SetGlobalOp (value (Ref 21)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 3)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 7) (Ref 10) (Ref 11) (Ref 14) (Ref 18)
       (Ref 19) (Ref 22))))
    |}]

let%expect_test "movsd" =
  test_trans_block 0x00461f64;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var eax) (typ Int)))
       (1 (DupVar (var edi) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var esi) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 3))))
       (5 (Const __i32 4))
       (6 (BiOp (var edi) (op Add) (lhs (Ref 1)) (rhs (Ref 5))))
       (7 (Const __i32 4))
       (8 (BiOp (var esi) (op Add) (lhs (Ref 2)) (rhs (Ref 7))))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 9))))
       (11 (Const __i32 4))
       (12 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var esi) (op Add) (lhs (Ref 8)) (rhs (Ref 13))))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 15))))
       (17 (Const __i32 4))
       (18 (BiOp (var edi) (op Add) (lhs (Ref 12)) (rhs (Ref 17))))
       (19 (Const __i32 4))
       (20 (BiOp (var esi) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))))
     (terminator (Goto (Block 8)))
     (roots ((Ref 0) (Ref 2) (Ref 4) (Ref 10) (Ref 16) (Ref 18) (Ref 20)))) |}]

let%expect_test "sbb" =
  test_trans_block 0x0048b8f3;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 1))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 2))))
       (5 (Const __i32 2147483647))
       (6 (BiOp (var ecx) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (Const __i32 0))
       (8
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 6))
         (rhs (Ref 4))))
       (9 (BiOp (var __i32) (op Add) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (OutsideContext (var eax) (typ Int)))
       (11 (BiOp (var eax) (op Subtract) (lhs (Ref 10)) (rhs (Ref 9))))
       (12 (LoadOp (var edx) (op Load32) (addr (Ref 2)) (offset 20)))
       (13 (Const __i32 0))
       (14
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 11))
         (rhs (Ref 10))))
       (15 (BiOp (var __i32) (op Add) (lhs (Ref 14)) (rhs (Ref 13))))
       (16 (BiOp (var edx) (op Subtract) (lhs (Ref 12)) (rhs (Ref 15))))
       (17 (Const __i32 -8))
       (18 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 17))))
       (19
        (SetGlobalOp (value (Ref 18)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 6) (Ref 10) (Ref 11) (Ref 16) (Ref 19))))
    |}]

let%expect_test "adc" =
  test_trans_block 0x0048b8db;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 1))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 2))))
       (5 (Const __i32 2147483648))
       (6 (BiOp (var ecx) (op Xor) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (Const __i32 2147483647))
       (8 (BiOp (var ecx) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (Const __i32 0))
       (10
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 8))
         (rhs (Ref 6))))
       (11 (BiOp (var __i32) (op Add) (lhs (Ref 10)) (rhs (Ref 9))))
       (12 (OutsideContext (var eax) (typ Int)))
       (13 (BiOp (var eax) (op Add) (lhs (Ref 11)) (rhs (Ref 12))))
       (14 (LoadOp (var edx) (op Load32) (addr (Ref 2)) (offset 20)))
       (15 (Const __i32 0))
       (16
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 13))
         (rhs (Ref 11))))
       (17 (BiOp (var __i32) (op Add) (lhs (Ref 16)) (rhs (Ref 15))))
       (18 (BiOp (var edx) (op Add) (lhs (Ref 17)) (rhs (Ref 14))))
       (19 (Const __i32 -8))
       (20 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 19))))
       (21
        (SetGlobalOp (value (Ref 20)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 8) (Ref 12) (Ref 13) (Ref 18) (Ref 21))))
    |}]

let%expect_test "test reflexive jns" =
  test_trans_block 0x0048b8c7;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0)) (offset -8)))
       (3 (BiOp (var __fl) (op FloatSub) (lhs (Ref 2)) (rhs (Ref 1))))
       (4 (OutsideContext (var edx) (typ Int))) (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 4))
         (rhs (Ref 5))))
       (7 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 3)) (offset -8)))
       (8 (Const __i32 -8))
       (9 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 8))))
       (10
        (SetGlobalOp (value (Ref 9)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 6))))
     (roots ((Ref 0) (Ref 4) (Ref 7) (Ref 10))))
    |}]

let%expect_test "fistp" =
  test_trans_block 0x0048b8af;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
       (6 (DupVar (var ebp) (src (Ref 4)) (typ Int))) (7 (Const __i32 32))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (Const __i32 4294967280))
       (10 (BiOp (var esp) (op And) (lhs (Ref 8)) (rhs (Ref 9))))
       (11 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (12 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 12)) (offset 24)))
       (14 (UniOp (var __i64) (op FloatToLong) (operand (Ref 12))))
       (15
        (StoreOp (op LongStore64) (addr (Ref 10)) (value (Ref 14)) (offset 16)))
       (16 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 10)) (offset 16)))
       (17 (UniOp (var __fl) (op Int64ToFloatSigned) (operand (Ref 16))))
       (18 (LoadOp (var edx) (op Load32) (addr (Ref 10)) (offset 24)))
       (19 (LoadOp (var eax) (op Load32) (addr (Ref 10)) (offset 16)))
       (20 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))
       (21 (StoreOp (op FloatStore64) (addr (Ref 11)) (value (Ref 12))))
       (22
        (StoreOp (op FloatStore64) (addr (Ref 11)) (value (Ref 17)) (offset 8)))
       (23 (Const __i32 8))
       (24 (BiOp (var __i32) (op Add) (lhs (Ref 11)) (rhs (Ref 23))))
       (25
        (SetGlobalOp (value (Ref 24)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 20))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 13)
       (Ref 15) (Ref 18) (Ref 19) (Ref 21) (Ref 22) (Ref 25))))
    |}]

let%expect_test "dec/dec js" =
  test_trans_block 0x0047d173;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const __i32 1)) (1 (OutsideContext (var ebp) (typ Int)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset -28)))
       (3 (BiOp (var __i32) (op Subtract) (lhs (Ref 2)) (rhs (Ref 0))))
       (4 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 3)) (offset -28)))
       (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 3))
         (rhs (Ref 5))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 6))))
     (roots ((Ref 1) (Ref 4)))) |}]

let%expect_test "jb" =
  test_trans_block 0x0044e4f5;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (Const __i32 50))
       (3
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 1))
         (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 3))))
     (roots ((Ref 0)))) |}]

let%expect_test "fsubr" =
  test_trans_block 0x00404715;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
         (offset 37)))
       (3 (UniOp (var ecx) (op ZeroExtendLow8) (operand (Ref 2))))
       (4 (Const __i32 2))
       (5 (BiOp (var ecx) (op ShiftLeft) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -64)))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -64)))
       (8 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 7))))
       (9 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9)) (offset 8)))
       (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 10)) (rhs (Ref 8))))
       (12 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -60)))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 11)) (offset 1044)))
       (14 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (15 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 15)) (offset 12)))
       (17 (StoreOp (op Store32) (addr (Ref 14)) (value (Ref 16)) (offset 1048)))
       (18 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (19 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (20 (LoadOp (var eax) (op Load32) (addr (Ref 19)) (offset 20)))
       (21 (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 20)) (offset 1028)))
       (22 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (23 (Const __i32 4973576))
       (24 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 23))))
       (25 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 22)) (offset 8)))
       (26 (BiOp (var __fl) (op FloatSub) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 26)) (offset -16)))
       (28 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (29 (Const __i32 4973580))
       (30 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 29))))
       (31 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 28)) (offset 12)))
       (32 (BiOp (var __fl) (op FloatSub) (lhs (Ref 30)) (rhs (Ref 31))))
       (33 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 32)) (offset -12)))
       (34 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (35 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (36 (BiOp (var __fl) (op FloatMult) (lhs (Ref 34)) (rhs (Ref 35))))
       (37 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (38 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (39 (BiOp (var __fl) (op FloatMult) (lhs (Ref 37)) (rhs (Ref 38))))
       (40 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 36)) (rhs (Ref 39))))
       (41 (Const __i32 4)) (42 (OutsideContext (var esp) (typ Int)))
       (43 (BiOp (var esp) (op Subtract) (lhs (Ref 42)) (rhs (Ref 41))))
       (44 (Const __i32 4212587))
       (45 (StoreOp (op Store32) (addr (Ref 43)) (value (Ref 44))))
       (46 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (47
        (StoreOp (op FloatStore64) (addr (Ref 46)) (value (Ref 40)) (offset 8)))
       (48 (Const __i32 8))
       (49 (BiOp (var __i32) (op Add) (lhs (Ref 46)) (rhs (Ref 48))))
       (50
        (SetGlobalOp (value (Ref 49)) (global ((name __fpuStack__) (typ Int)))))
       (51 (CallOp (func __func48b8a0__) (args ((Ref 22) (Ref 43) (Ref 28)))))
       (52 (ReturnedOp (var eax) (typ Int)))
       (53 (ReturnedOp (var esp) (typ Int)))
       (54 (ReturnedOp (var edx) (typ Int)))
       (55 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 52)) (offset -8)))
       (56 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (57 (Const __i32 4096))
       (58
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 56))
         (rhs (Ref 57))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 8)) (condition (Ref 58))))
     (roots
      ((Ref 0) (Ref 6) (Ref 13) (Ref 17) (Ref 21) (Ref 22) (Ref 27) (Ref 33)
       (Ref 42) (Ref 45) (Ref 46) (Ref 47) (Ref 50) (Ref 51) (Ref 52) (Ref 53)
       (Ref 54) (Ref 55))))
    |}]

let%expect_test "rep movsd" =
  test_trans_block 0x00403ad5;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var esi) (op Load32) (addr (Ref 0)) (offset -604)))
       (2 (Const __i32 5888))
       (3 (BiOp (var esi) (op Add) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (Const ecx 147)) (5 (Const __i32 -600))
       (6 (BiOp (var edi) (op Add) (lhs (Ref 0)) (rhs (Ref 5))))
       (7 (Const __i32 2))
       (8 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (Memcopy (count (Ref 8)) (src (Ref 3)) (dest (Ref 6))))
       (10 (BiOp (var esi) (op Add) (lhs (Ref 3)) (rhs (Ref 8))))
       (11 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 8))))
       (12 (Const ecx 0))
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -152)))
       (14 (Const __i32 4096))
       (15 (BiOp (var eax) (op Or) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 15)) (offset -152)))
       (17 (Const __i32 -600))
       (18 (BiOp (var ecx) (op Add) (lhs (Ref 0)) (rhs (Ref 17))))
       (19 (OutsideContext (var esp) (typ Int))) (20 (Const __i32 4))
       (21 (BiOp (var esp) (op Subtract) (lhs (Ref 19)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 18))))
       (23 (Const __i32 4955716))
       (24 (LoadOp (var ecx) (op Load32) (addr (Ref 23)))) (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 25))))
       (27 (Const __i32 4209397))
       (28 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 27))))
       (29 (OutsideContext (var edx) (typ Int)))
       (30 (CallOp (func __func44f770__) (args ((Ref 24) (Ref 26) (Ref 29)))))
       (31 (ReturnedOp (var eax) (typ Int)))
       (32 (ReturnedOp (var esp) (typ Int)))
       (33 (ReturnedOp (var edx) (typ Int)))))
     (terminator (Goto (Block 4)))
     (roots
      ((Ref 0) (Ref 9) (Ref 10) (Ref 11) (Ref 16) (Ref 19) (Ref 22) (Ref 24)
       (Ref 28) (Ref 29) (Ref 30) (Ref 31) (Ref 32) (Ref 33))))
    |}]

let%expect_test "fabs" =
  test_trans_block 0x004023c9;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (2 (Const __i32 588))
       (3 (BiOp (var eax) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -40)))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 5)) (offset 3396)))
       (7 (Const __i32 4819588))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatSub) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (Const __i32 4973576))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
       (12 (BiOp (var __fl) (op FloatSub) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 12)) (offset -36)))
       (14 (UniOp (var __fl) (op FloatAbs) (operand (Ref 12))))
       (15 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 14)) (offset -44)))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 14)) (offset -4)))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -4)))
       (18 (Const __i32 4819720))
       (19 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 18))))
       (20 (Landmine (var eax) (typ Int))) (21 (Const __i32 5))
       (22 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 17)) (rhs (Ref 19))))
       (23 (UniOp (var __i32) (op EqualsZero) (operand (Ref 22))))
       (24 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 23))))
     (roots ((Ref 0) (Ref 4) (Ref 13) (Ref 15) (Ref 16) (Ref 20) (Ref 24))))
    |}]

let%expect_test "jae" =
  test_trans_block 0x004027fc;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (Const __i32 10))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 1)) (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 6)) (condition (Ref 3))))
     (roots ((Ref 0)))) |}]

let%expect_test "rep stosd (zeroed)" =
  test_trans_block 0x0040118c;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
       (6 (DupVar (var ebp) (src (Ref 4)) (typ Int))) (7 (Const __i32 36))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (OutsideContext (var edi) (typ Int))) (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 9))))
       (13 (OutsideContext (var ecx) (typ Int)))
       (14 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 13)) (offset -36)))
       (15 (LoadOp (var ecx) (op Load32) (addr (Ref 6)) (offset -36)))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 16))))
       (18 (Const __i32 4198781))
       (19 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 18))))
       (20 (OutsideContext (var edx) (typ Int)))
       (21 (CallOp (func __func4011b0__) (args ((Ref 15) (Ref 17) (Ref 20)))))
       (22 (ReturnedOp (var eax) (typ Int)))
       (23 (ReturnedOp (var esp) (typ Int)))
       (24 (ReturnedOp (var edx) (typ Int))) (25 (Const ecx 147))
       (26 (Const eax 0))
       (27 (LoadOp (var edi) (op Load32) (addr (Ref 6)) (offset -36)))
       (28 (Const __i32 2))
       (29 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 25)) (rhs (Ref 28))))
       (30 (Const __i32 0))
       (31 (Memset (count (Ref 29)) (value (Ref 30)) (dest (Ref 27))))
       (32 (Const ecx 0))
       (33 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -36)))
       (34 (Const __i32 65535))
       (35 (UniOp (var __i32) (op SignExtend16) (operand (Ref 34))))
       (36 (StoreOp (op Store16) (addr (Ref 33)) (value (Ref 35)) (offset 468)))
       (37 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -36)))
       (38 (LoadOp (var edi) (op Load32) (addr (Ref 23)))) (39 (Const __i32 4))
       (40 (BiOp (var esp) (op Add) (lhs (Ref 23)) (rhs (Ref 39))))
       (41 (DupVar (var esp) (src (Ref 6)) (typ Int)))
       (42 (LoadOp (var ebp) (op Load32) (addr (Ref 41)))) (43 (Const __i32 4))
       (44 (BiOp (var esp) (op Add) (lhs (Ref 41)) (rhs (Ref 43))))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 44))))
       (46 (BiOp (var __i32) (op Equal) (lhs (Ref 45)) (rhs (Ref 1))))
       (47 (AssertOp (condition (Ref 46))))))
     (terminator Return)
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 9) (Ref 12) (Ref 13) (Ref 14)
       (Ref 19) (Ref 20) (Ref 21) (Ref 22) (Ref 23) (Ref 24) (Ref 31) (Ref 32)
       (Ref 36) (Ref 37) (Ref 38) (Ref 42) (Ref 44) (Ref 47))))
    |}]

let%expect_test "fild/fiadd" =
  test_trans_block 0x00453df6;
  [%expect
    {|
    ((id 510)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset 8)))
       (2
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
         (offset 559)))
       (3 (UniOp (var eax) (op ZeroExtendLow8) (operand (Ref 2))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 4)) (signed false)
         (offset 555)))
       (6 (UniOp (var edx) (op ZeroExtendLow8) (operand (Ref 5))))
       (7 (BiOp (var eax) (op Subtract) (lhs (Ref 3)) (rhs (Ref 6))))
       (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -792)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -792)))
       (10 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 9))))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (12 (BiOp (var __fl) (op FloatMult) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (14
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 13)) (signed false)
         (offset 555)))
       (15 (UniOp (var ecx) (op ZeroExtendLow8) (operand (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 15)) (offset -796)))
       (17 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -796)))
       (18 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 17))))
       (19 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 12)) (rhs (Ref 18))))
       (20 (Const __i32 4)) (21 (OutsideContext (var esp) (typ Int)))
       (22 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 20))))
       (23 (Const __i32 4537877))
       (24 (StoreOp (op Store32) (addr (Ref 22)) (value (Ref 23))))
       (25 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (26
        (StoreOp (op FloatStore64) (addr (Ref 25)) (value (Ref 19)) (offset 8)))
       (27 (Const __i32 8))
       (28 (BiOp (var __i32) (op Add) (lhs (Ref 25)) (rhs (Ref 27))))
       (29
        (SetGlobalOp (value (Ref 28)) (global ((name __fpuStack__) (typ Int)))))
       (30 (CallOp (func __func48b8a0__) (args ((Ref 15) (Ref 22) (Ref 6)))))
       (31 (ReturnedOp (var eax) (typ Int)))
       (32 (ReturnedOp (var esp) (typ Int)))
       (33 (ReturnedOp (var edx) (typ Int)))
       (34 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset 8)))
       (35 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 31))))
       (36 (StoreOp (op Store8) (addr (Ref 34)) (value (Ref 35)) (offset 443)))))
     (terminator (Goto (Block 513)))
     (roots
      ((Ref 0) (Ref 8) (Ref 15) (Ref 16) (Ref 21) (Ref 24) (Ref 25) (Ref 26)
       (Ref 29) (Ref 30) (Ref 31) (Ref 32) (Ref 33) (Ref 34) (Ref 36))))
    |}]

let%expect_test "div" =
  test_trans_block 0x00452f62;
  [%expect
    {|
    ((id 341)
     (instrs
      ((0 (Const ecx 4849184)) (1 (Const __i32 4))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (BiOp (var esp) (op Subtract) (lhs (Ref 2)) (rhs (Ref 1))))
       (4 (Const __i32 4534107))
       (5 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 4))))
       (6 (OutsideContext (var edx) (typ Int)))
       (7 (CallOp (func __func4318d0__) (args ((Ref 0) (Ref 3) (Ref 6)))))
       (8 (ReturnedOp (var eax) (typ Int))) (9 (ReturnedOp (var esp) (typ Int)))
       (10 (ReturnedOp (var edx) (typ Int))) (11 (Const edx 0))
       (12 (OutsideContext (var ebp) (typ Int)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 12)) (offset -248)))
       (14
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 8))
         (rhs (Ref 13))))
       (15
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 8))
         (rhs (Ref 13))))
       (16 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 15)) (offset -608)))))
     (terminator (Goto (Block 343)))
     (roots
      ((Ref 0) (Ref 2) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10) (Ref 12)
       (Ref 14) (Ref 15) (Ref 16))))
    |}]

let%expect_test "fdiv" =
  test_trans_block 0x00452949;
  [%expect
    {|
    ((id 279)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -500)))
       (2 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -504)))
       (3 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -508)))
       (5 (Const __i32 0)) (6 (OutsideContext (var esp) (typ Int)))
       (7 (Const __i32 4))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 5))))
       (10 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (11
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 10)) (signed false)
         (offset 6)))
       (12 (UniOp (var __i32) (op SignExtend16) (operand (Ref 11))))
       (13 (OutsideContext (var eax) (typ Int)))
       (14 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (Const __i32 4))
       (16 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 15))))
       (17 (StoreOp (op Store32) (addr (Ref 16)) (value (Ref 14))))
       (18 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (19 (Const __i32 8))
       (20 (BiOp (var ecx) (op Add) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (Const __i32 4))
       (22 (BiOp (var esp) (op Subtract) (lhs (Ref 16)) (rhs (Ref 21))))
       (23 (StoreOp (op Store32) (addr (Ref 22)) (value (Ref 20))))
       (24 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 22)) (rhs (Ref 25))))
       (27 (Const __i32 4532585))
       (28 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 27))))
       (29 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (30 (CallOp (func __func450c10__) (args ((Ref 24) (Ref 26) (Ref 10)))))
       (31 (ReturnedOp (var eax) (typ Int)))
       (32 (ReturnedOp (var esp) (typ Int)))
       (33 (ReturnedOp (var edx) (typ Int)))
       (34 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -508)))
       (35 (StoreOp (op Store32) (addr (Ref 31)) (value (Ref 34))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 4) (Ref 6) (Ref 9) (Ref 13) (Ref 17) (Ref 23) (Ref 24)
       (Ref 28) (Ref 29) (Ref 30) (Ref 31) (Ref 32) (Ref 33) (Ref 34) (Ref 35))))
    |}]

let%expect_test "cdq/idiv" =
  test_trans_block 0x004529e3;
  [%expect
    {|
    ((id 286)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -512)))
       (2 (Const __i32 31))
       (3
        (SignedBiOp (var edx) (op ShiftRight) (signed true) (lhs (Ref 1))
         (rhs (Ref 2))))
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -516)))
       (5
        (SignedBiOp (var eax) (op Divide) (signed true) (lhs (Ref 1))
         (rhs (Ref 4))))
       (6
        (SignedBiOp (var edx) (op Remainder) (signed true) (lhs (Ref 1))
         (rhs (Ref 4))))
       (7 (DupVar (var esi) (src (Ref 6)) (typ Int))) (8 (Const __i32 0))
       (9 (OutsideContext (var esp) (typ Int))) (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 8))))
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (14
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 13)) (signed false)
         (offset 6)))
       (15 (UniOp (var __i32) (op SignExtend16) (operand (Ref 14))))
       (16 (OutsideContext (var ecx) (typ Int)))
       (17 (BiOp (var ecx) (op MergeTrunc16) (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (Const __i32 4))
       (19 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 17))))
       (21 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (22 (Const __i32 8))
       (23 (BiOp (var edx) (op Add) (lhs (Ref 21)) (rhs (Ref 22))))
       (24 (Const __i32 4))
       (25 (BiOp (var esp) (op Subtract) (lhs (Ref 19)) (rhs (Ref 24))))
       (26 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 23))))
       (27 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 25)) (rhs (Ref 28))))
       (30 (Const __i32 4532736))
       (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
       (32 (CallOp (func __func450ca0__) (args ((Ref 27) (Ref 29) (Ref 23)))))
       (33 (ReturnedOp (var eax) (typ Int)))
       (34 (ReturnedOp (var esp) (typ Int)))
       (35 (ReturnedOp (var edx) (typ Int)))
       (36 (StoreOp (op Store32) (addr (Ref 33)) (value (Ref 7))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 7) (Ref 9) (Ref 12) (Ref 16) (Ref 20) (Ref 26) (Ref 27)
       (Ref 31) (Ref 32) (Ref 33) (Ref 34) (Ref 35) (Ref 36))))
    |}]

let%expect_test "imul" =
  test_trans_block 0x004539dd;
  [%expect
    {|
    ((id 491)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -12)))
       (2 (Const __i32 12))
       (3 (BiOp (var eax) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5 (Const edx 0))
       (6 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset 140)))
       (8 (Const __i32 0))
       (9
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 7))
         (rhs (Ref 8))))
       (10 (BiOp (var edx) (op MergeTruncLow8) (lhs (Ref 9)) (rhs (Ref 5))))
       (11 (UniOp (var __i32) (op EqualsZero) (operand (Ref 10))))))
     (terminator
      (Branch (succeed (Block 513)) (fail (Block 492)) (condition (Ref 11))))
     (roots ((Ref 0) (Ref 3) (Ref 4) (Ref 10))))
    |}]

let%expect_test "float jp 0x5" =
  test_trans_block 0x00451bfe;
  [%expect
    {|
    ((id 148)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 1)) (offset 40)))
       (3 (Const __i32 4819532))
       (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3))))
       (5 (Landmine (var eax) (typ Int))) (6 (Const __i32 5))
       (7 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 2)) (rhs (Ref 4))))
       (8 (UniOp (var __i32) (op EqualsZero) (operand (Ref 7))))
       (9 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 150)) (fail (Block 149)) (condition (Ref 8))))
     (roots ((Ref 0) (Ref 1) (Ref 5) (Ref 9))))
    |}]

let%expect_test "jle" =
  test_trans_block 0x004536f0;
  [%expect
    {|
    ((id 450)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -728)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -732)))
       (3
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 1))
         (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 452)) (fail (Block 451)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 1))))
    |}]

let%expect_test "jumptable" =
  test_trans_block 0x00450dec;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -312)))))
     (terminator
      (Switch
       (cases
        ((Block 7) (Block 481) (Block 7) (Block 8) (Block 9) (Block 22)
         (Block 23) (Block 74) (Block 13) (Block 20) (Block 21) (Block 29)
         (Block 31) (Block 32) (Block 42) (Block 52) (Block 69) (Block 73)
         (Block 98) (Block 97) (Block 96) (Block 125) (Block 481) (Block 141)
         (Block 124) (Block 30) (Block 142) (Block 143) (Block 151) (Block 140)
         (Block 59) (Block 167) (Block 168) (Block 169) (Block 185) (Block 189)
         (Block 193) (Block 206) (Block 216) (Block 220) (Block 294) (Block 298)
         (Block 302) (Block 306) (Block 310) (Block 314) (Block 318) (Block 322)
         (Block 326) (Block 330) (Block 224) (Block 231) (Block 238) (Block 245)
         (Block 252) (Block 259) (Block 266) (Block 273) (Block 280) (Block 287)
         (Block 337) (Block 344) (Block 348) (Block 352) (Block 356) (Block 360)
         (Block 364) (Block 368) (Block 372) (Block 381) (Block 390) (Block 399)
         (Block 408) (Block 417) (Block 426) (Block 435) (Block 444) (Block 453)
         (Block 462) (Block 471) (Block 115) (Block 159) (Block 163)))
       (default (Block 528)) (switch_on (Ref 1))))
     (roots ((Ref 0) (Ref 1))))
    |}]

let%expect_test "ja" =
  test_trans_block 0x00450de0;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed false)))
       (3 (UniOp (var ecx) (op SignExtend16) (operand (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -312)))
       (5 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -312)))
       (6 (Const __i32 1))
       (7 (BiOp (var edx) (op Add) (lhs (Ref 5)) (rhs (Ref 6))))
       (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -312)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -312)))
       (10 (Const __i32 82))
       (11
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 9))
         (rhs (Ref 10))))))
     (terminator
      (Branch (succeed (Block 481)) (fail (Block 6)) (condition (Ref 11))))
     (roots ((Ref 0) (Ref 1) (Ref 3) (Ref 4) (Ref 7) (Ref 8))))
    |}]

let%expect_test "jg" =
  test_trans_block 4525496;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var ecx) (op Load32) (addr (Ref 1)) (offset 480)))
       (3 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 2)) (offset -4)))
       (4 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5 (LoadOp (var eax) (op Load32) (addr (Ref 4)) (offset 56)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -56)))
       (7 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (8
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 7)) (signed false)
         (offset 4)))
       (9 (UniOp (var edx) (op SignExtend16) (operand (Ref 8))))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -56)))
       (11
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 9))
         (rhs (Ref 10))))))
     (terminator
      (Branch (succeed (Block 482)) (fail (Block 5)) (condition (Ref 11))))
     (roots ((Ref 0) (Ref 3) (Ref 5) (Ref 6) (Ref 7) (Ref 9))))
    |}]

let%expect_test "jge" =
  test_trans_block 4380548;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -16)))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 1))
         (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 3)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 1))))
    |}]

let%expect_test _ =
  test_trans_block 0x0040127a;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -24)))
       (2 (Const __i32 1))
       (3 (BiOp (var eax) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -24)))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (6 (Const __i32 0))
       (7
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 5))
         (rhs (Ref 6))))))
     (terminator
      (Branch (succeed (Block 6)) (fail (Block 5)) (condition (Ref 7))))
     (roots ((Ref 0) (Ref 3) (Ref 4))))
    |}]

let%expect_test _ =
  test_trans 0x47ea7d;
  [%expect
    {|
    ((name func_47ea7d)
     (signature
      ((args
        (((name ecx) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))
       (returns
        (((name eax) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (Const __i32 96)) (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
          (6 (Const __i32 4785824)) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 6))))
          (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
          (12 (Const __i32 4713092))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (OutsideContext (var ecx) (typ Int)))
          (15 (OutsideContext (var edx) (typ Int)))
          (16 (CallOp (func __func480624__) (args ((Ref 14) (Ref 11) (Ref 15)))))
          (17 (ReturnedOp (var eax) (typ Int)))
          (18 (ReturnedOp (var esp) (typ Int)))
          (19 (ReturnedOp (var edx) (typ Int))) (20 (Const edi 148))
          (21 (DupVar (var eax) (src (Ref 20)) (typ Int))) (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Subtract) (lhs (Ref 18)) (rhs (Ref 22))))
          (24 (Const __i32 4713104))
          (25 (StoreOp (op Store32) (addr (Ref 23)) (value (Ref 24))))
          (26 (CallOp (func __func47f3a0__) (args ((Ref 14) (Ref 23) (Ref 19)))))
          (27 (ReturnedOp (var eax) (typ Int)))
          (28 (ReturnedOp (var esp) (typ Int)))
          (29 (ReturnedOp (var edx) (typ Int)))
          (30 (OutsideContext (var ebp) (typ Int)))
          (31
           (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 28)) (offset -24)))
          (32 (DupVar (var esi) (src (Ref 28)) (typ Int)))
          (33 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 20))))
          (34 (Const __i32 4))
          (35 (BiOp (var esp) (op Subtract) (lhs (Ref 28)) (rhs (Ref 34))))
          (36 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 32))))
          (37 (Const __i32 4))
          (38 (BiOp (var esp) (op Subtract) (lhs (Ref 35)) (rhs (Ref 37))))
          (39 (Const __i32 4713117))
          (40 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 39))))
          (41 (CallOp (func KERNEL32.dll_GetVersionExA) (args ((Ref 38)))))
          (42 (ReturnedOp (var eax) (typ Int)))
          (43 (ReturnedOp (var esp) (typ Int)))
          (44 (ReturnedOp (var edx) (typ Int)))
          (45 (LoadOp (var ecx) (op Load32) (addr (Ref 32)) (offset 16)))
          (46 (Const __i32 4847496))
          (47 (StoreOp (op Store32) (addr (Ref 46)) (value (Ref 45))))
          (48 (LoadOp (var eax) (op Load32) (addr (Ref 32)) (offset 4)))
          (49 (Const __i32 4847508))
          (50 (StoreOp (op Store32) (addr (Ref 49)) (value (Ref 48))))
          (51 (LoadOp (var edx) (op Load32) (addr (Ref 32)) (offset 8)))
          (52 (Const __i32 4847512))
          (53 (StoreOp (op Store32) (addr (Ref 52)) (value (Ref 51))))
          (54 (LoadOp (var esi) (op Load32) (addr (Ref 32)) (offset 12)))
          (55 (Const __i32 32767))
          (56 (BiOp (var esi) (op And) (lhs (Ref 54)) (rhs (Ref 55))))
          (57 (Const __i32 4847500))
          (58 (StoreOp (op Store32) (addr (Ref 57)) (value (Ref 56))))
          (59 (Const __i32 2))
          (60 (BiOp (var __i32) (op Equal) (lhs (Ref 45)) (rhs (Ref 59))))))
        (terminator
         (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 60))))
        (roots
         ((Ref 0) (Ref 1) (Ref 5) (Ref 9) (Ref 13) (Ref 14) (Ref 15) (Ref 16)
          (Ref 17) (Ref 18) (Ref 19) (Ref 20) (Ref 25) (Ref 26) (Ref 27)
          (Ref 28) (Ref 29) (Ref 30) (Ref 31) (Ref 33) (Ref 36) (Ref 40)
          (Ref 41) (Ref 42) (Ref 43) (Ref 44) (Ref 45) (Ref 47) (Ref 48)
          (Ref 50) (Ref 51) (Ref 53) (Ref 56) (Ref 58))))
       ((id 1)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int))) (1 (Const __i32 32768))
          (2 (BiOp (var esi) (op Or) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (Const __i32 4847500))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2))))))
        (terminator (Goto (Block 2))) (roots ((Ref 0) (Ref 2) (Ref 4))))
       ((id 2)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 8))
          (2 (BiOp (var eax) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (OutsideContext (var edx) (typ Int)))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (Const __i32 4847504))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))
          (7 (Const esi 0)) (8 (OutsideContext (var esp) (typ Int)))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 9))))
          (11 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 7))))
          (12 (Const __i32 4772232))
          (13 (LoadOp (var edi) (op Load32) (addr (Ref 12))))
          (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Subtract) (lhs (Ref 10)) (rhs (Ref 14))))
          (16 (Const __i32 4713200))
          (17 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 16))))
          (18 (OutsideContext (var ecx) (typ Int)))
          (19
           (CallIndirectOp (table_index (Ref 13))
            (args ((Ref 18) (Ref 15) (Ref 3)))))
          (20 (ReturnedOp (var eax) (typ Int)))
          (21 (ReturnedOp (var esp) (typ Int)))
          (22 (ReturnedOp (var edx) (typ Int)))
          (23
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 20)) (signed false)))
          (24 (Const __i32 23117))
          (25 (UniOp (var __i32) (op SignExtend16) (operand (Ref 23))))
          (26 (UniOp (var __i32) (op SignExtend16) (operand (Ref 24))))
          (27 (BiOp (var __i32) (op NotEqual) (lhs (Ref 25)) (rhs (Ref 26))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 3)) (condition (Ref 27))))
        (roots
         ((Ref 0) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 11) (Ref 13) (Ref 17)
          (Ref 18) (Ref 19) (Ref 20) (Ref 21) (Ref 22))))
       ((id 3)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 60)))
          (2 (BiOp (var ecx) (op Add) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
          (4 (Const __i32 17744))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 3)) (rhs (Ref 4))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 4)) (condition (Ref 5))))
        (roots ((Ref 0) (Ref 2))))
       ((id 4)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset 24)))
          (2 (UniOp (var eax) (op ZeroExtend16) (operand (Ref 1))))
          (3 (Const __i32 267))
          (4 (BiOp (var __i32) (op Equal) (lhs (Ref 2)) (rhs (Ref 3))))))
        (terminator
         (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 4))))
        (roots ((Ref 0) (Ref 2))))
       ((id 5)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 523))
          (2 (BiOp (var __i32) (op Equal) (lhs (Ref 0)) (rhs (Ref 1))))))
        (terminator
         (Branch (succeed (Block 7)) (fail (Block 6)) (condition (Ref 2))))
        (roots ((Ref 0))))
       ((id 6)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int)))
          (1 (OutsideContext (var ebp) (typ Int)))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -28)))))
        (terminator (Goto (Block 12))) (roots ((Ref 0) (Ref 1) (Ref 2))))
       ((id 7)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 132)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 8)) (condition (Ref 3))))
        (roots ((Ref 0))))
       ((id 8)
        (instrs
         ((0 (Const eax 0)) (1 (OutsideContext (var ecx) (typ Int)))
          (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 248)))
          (3 (OutsideContext (var esi) (typ Int)))
          (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (DupVar (var __input_compare_arg__) (src (Ref 4)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 0) (Ref 1) (Ref 3) (Ref 5))))
       ((id 9)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 116)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 10)) (condition (Ref 3))))
        (roots ((Ref 0))))
       ((id 10)
        (instrs
         ((0 (Const eax 0)) (1 (OutsideContext (var ecx) (typ Int)))
          (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 232)))
          (3 (OutsideContext (var esi) (typ Int)))
          (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (DupVar (var __input_compare_arg__) (src (Ref 4)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 0) (Ref 1) (Ref 3) (Ref 5))))
       ((id 11)
        (instrs
         ((0 (OutsideContext (var __input_compare_arg__) (typ Int)))
          (1 (OutsideContext (var eax) (typ Int)))
          (2 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (OutsideContext (var ebp) (typ Int)))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2)) (offset -28)))))
        (terminator (Goto (Block 12)))
        (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4))))
       ((id 12)
        (instrs
         ((0 (Const __i32 1)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713286))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func48068a__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator
         (Branch (succeed (Block 14)) (fail (Block 13)) (condition (Ref 12))))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 13)
        (instrs
         ((0 (Const __i32 28)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713298))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea59__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 14)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 14)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713304))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int)))
          (7 (CallOp (func __func481649__) (args ((Ref 5) (Ref 2) (Ref 6)))))
          (8 (ReturnedOp (var eax) (typ Int)))
          (9 (ReturnedOp (var esp) (typ Int)))
          (10 (ReturnedOp (var edx) (typ Int)))))
        (terminator
         (Branch (succeed (Block 16)) (fail (Block 15)) (condition (Ref 8))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10))))
       ((id 15)
        (instrs
         ((0 (Const __i32 16)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713315))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea59__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 16)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 16)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713321))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int)))
          (7 (CallOp (func __func483560__) (args ((Ref 5) (Ref 2) (Ref 6)))))
          (8 (ReturnedOp (var eax) (typ Int)))
          (9 (ReturnedOp (var esp) (typ Int)))
          (10 (ReturnedOp (var edx) (typ Int)))
          (11 (OutsideContext (var esi) (typ Int)))
          (12 (OutsideContext (var ebp) (typ Int)))
          (13
           (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 11)) (offset -4)))
          (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 14))))
          (16 (Const __i32 4713329))
          (17 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 16))))
          (18 (CallOp (func __func483362__) (args ((Ref 5) (Ref 15) (Ref 10)))))
          (19 (ReturnedOp (var eax) (typ Int)))
          (20 (ReturnedOp (var esp) (typ Int)))
          (21 (ReturnedOp (var edx) (typ Int))) (22 (Const __i32 0))
          (23
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 19)) (rhs (Ref 22))))))
        (terminator
         (Branch (succeed (Block 18)) (fail (Block 17)) (condition (Ref 23))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 11) (Ref 12) (Ref 13) (Ref 17) (Ref 18) (Ref 19) (Ref 20)
          (Ref 21))))
       ((id 17)
        (instrs
         ((0 (Const __i32 27)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713340))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea34__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 18)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 18)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713346))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func KERNEL32.dll_GetCommandLineA) (args ((Ref 2)))))
          (6 (ReturnedOp (var eax) (typ Int)))
          (7 (ReturnedOp (var esp) (typ Int)))
          (8 (ReturnedOp (var edx) (typ Int))) (9 (Const __i32 20337236))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 6))))
          (11 (Const __i32 4))
          (12 (BiOp (var esp) (op Subtract) (lhs (Ref 7)) (rhs (Ref 11))))
          (13 (Const __i32 4713357))
          (14 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 13))))
          (15 (OutsideContext (var ecx) (typ Int)))
          (16 (CallOp (func __func483240__) (args ((Ref 15) (Ref 12) (Ref 8)))))
          (17 (ReturnedOp (var eax) (typ Int)))
          (18 (ReturnedOp (var esp) (typ Int)))
          (19 (ReturnedOp (var edx) (typ Int))) (20 (Const __i32 4847472))
          (21 (StoreOp (op Store32) (addr (Ref 20)) (value (Ref 17))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Subtract) (lhs (Ref 18)) (rhs (Ref 22))))
          (24 (Const __i32 4713367))
          (25 (StoreOp (op Store32) (addr (Ref 23)) (value (Ref 24))))
          (26 (CallOp (func __func48319e__) (args ((Ref 15) (Ref 23) (Ref 19)))))
          (27 (ReturnedOp (var eax) (typ Int)))
          (28 (ReturnedOp (var esp) (typ Int)))
          (29 (ReturnedOp (var edx) (typ Int))) (30 (Const __i32 0))
          (31
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 27)) (rhs (Ref 30))))))
        (terminator
         (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 31))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 10) (Ref 14)
          (Ref 15) (Ref 16) (Ref 17) (Ref 18) (Ref 19) (Ref 21) (Ref 25)
          (Ref 26) (Ref 27) (Ref 28) (Ref 29))))
       ((id 19)
        (instrs
         ((0 (Const __i32 8)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713378))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea34__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 20)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 20)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713384))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int)))
          (7 (CallOp (func __func482f6b__) (args ((Ref 5) (Ref 2) (Ref 6)))))
          (8 (ReturnedOp (var eax) (typ Int)))
          (9 (ReturnedOp (var esp) (typ Int)))
          (10 (ReturnedOp (var edx) (typ Int))) (11 (Const __i32 0))
          (12
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 8)) (rhs (Ref 11))))))
        (terminator
         (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 12))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10))))
       ((id 21)
        (instrs
         ((0 (Const __i32 9)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713395))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea34__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 22)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 15) (Ref 17))))
       ((id 22)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713401))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int)))
          (7 (CallOp (func __func47f891__) (args ((Ref 5) (Ref 2) (Ref 6)))))
          (8 (ReturnedOp (var eax) (typ Int)))
          (9 (ReturnedOp (var esp) (typ Int)))
          (10 (ReturnedOp (var edx) (typ Int)))
          (11 (OutsideContext (var ebp) (typ Int)))
          (12
           (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 8)) (offset -32)))
          (13 (OutsideContext (var esi) (typ Int)))
          (14 (BiOp (var __i32) (op Equal) (lhs (Ref 8)) (rhs (Ref 13))))))
        (terminator
         (Branch (succeed (Block 24)) (fail (Block 23)) (condition (Ref 14))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 11) (Ref 12) (Ref 13))))
       ((id 23)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713414))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47ea34__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))
          (15 (LoadOp (var ecx) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))))
        (terminator (Goto (Block 24)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 14) (Ref 15) (Ref 17))))
       ((id 24)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int)))
          (1 (OutsideContext (var ebp) (typ Int)))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -56)))
          (3 (Const __i32 -100))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 1)) (rhs (Ref 3))))
          (5 (OutsideContext (var esp) (typ Int))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))
          (8 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 4))))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 7)) (rhs (Ref 9))))
          (11 (Const __i32 4713427))
          (12 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 11))))
          (13 (CallOp (func KERNEL32.dll_GetStartupInfoA) (args ((Ref 10)))))
          (14 (ReturnedOp (var eax) (typ Int)))
          (15 (ReturnedOp (var esp) (typ Int)))
          (16 (ReturnedOp (var edx) (typ Int))) (17 (Const __i32 4))
          (18 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 17))))
          (19 (Const __i32 4713433))
          (20 (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 19))))
          (21 (OutsideContext (var ecx) (typ Int)))
          (22 (CallOp (func __func482f02__) (args ((Ref 21) (Ref 18) (Ref 16)))))
          (23 (ReturnedOp (var eax) (typ Int)))
          (24 (ReturnedOp (var esp) (typ Int)))
          (25 (ReturnedOp (var edx) (typ Int)))
          (26
           (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 23)) (offset -104)))
          (27
           (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
            (offset -56)))
          (28 (Const __i32 1))
          (29 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 27))))
          (30 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 28))))
          (31 (BiOp (var __i32) (op And) (lhs (Ref 29)) (rhs (Ref 30))))
          (32 (UniOp (var __i32) (op EqualsZero) (operand (Ref 31))))))
        (terminator
         (Branch (succeed (Block 26)) (fail (Block 25)) (condition (Ref 32))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 8) (Ref 12) (Ref 13) (Ref 14)
          (Ref 15) (Ref 16) (Ref 20) (Ref 21) (Ref 22) (Ref 23) (Ref 24)
          (Ref 25) (Ref 26))))
       ((id 25)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset -52)))
          (2 (UniOp (var eax) (op ZeroExtend16) (operand (Ref 1))))))
        (terminator (Goto (Block 27))) (roots ((Ref 0) (Ref 2))))
       ((id 26)
        (instrs
         ((0 (Const __i32 10)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (LoadOp (var eax) (op Load32) (addr (Ref 3)))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Add) (lhs (Ref 3)) (rhs (Ref 6))))))
        (terminator (Goto (Block 27))) (roots ((Ref 1) (Ref 4) (Ref 5) (Ref 7))))
       ((id 27)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ebp) (typ Int)))
          (6 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -104)))
          (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 6))))
          (10 (OutsideContext (var esi) (typ Int))) (11 (Const __i32 4))
          (12 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 11))))
          (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
          (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 14))))
          (16 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 10))))
          (17 (OutsideContext (var edi) (typ Int))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 18))))
          (20 (Const __i32 4713462))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22 (OutsideContext (var ecx) (typ Int)))
          (23 (OutsideContext (var edx) (typ Int)))
          (24
           (CallIndirectOp (table_index (Ref 17))
            (args ((Ref 22) (Ref 19) (Ref 23)))))
          (25 (ReturnedOp (var eax) (typ Int)))
          (26 (ReturnedOp (var esp) (typ Int)))
          (27 (ReturnedOp (var edx) (typ Int))) (28 (Const __i32 4))
          (29 (BiOp (var esp) (op Subtract) (lhs (Ref 26)) (rhs (Ref 28))))
          (30 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 25))))
          (31 (Const __i32 4))
          (32 (BiOp (var esp) (op Subtract) (lhs (Ref 29)) (rhs (Ref 31))))
          (33 (Const __i32 4713465))
          (34 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 33))))
          (35 (CallOp (func __func434020__) (args ((Ref 22) (Ref 32) (Ref 27)))))
          (36 (ReturnedOp (var eax) (typ Int)))
          (37 (ReturnedOp (var esp) (typ Int)))
          (38 (ReturnedOp (var edx) (typ Int)))
          (39 (DupVar (var edi) (src (Ref 36)) (typ Int)))
          (40
           (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 39)) (offset -108)))
          (41 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -28)))
          (42 (BiOp (var __i32) (op NotEqual) (lhs (Ref 41)) (rhs (Ref 10))))))
        (terminator
         (Branch (succeed (Block 29)) (fail (Block 28)) (condition (Ref 42))))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 9) (Ref 10) (Ref 13) (Ref 16)
          (Ref 17) (Ref 21) (Ref 22) (Ref 23) (Ref 24) (Ref 25) (Ref 26)
          (Ref 27) (Ref 30) (Ref 34) (Ref 35) (Ref 36) (Ref 37) (Ref 38)
          (Ref 39) (Ref 40))))
       ((id 28)
        (instrs
         ((0 (OutsideContext (var edi) (typ Int)))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713481))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int)))
          (11 (CallOp (func __func47f9c9__) (args ((Ref 9) (Ref 6) (Ref 10)))))
          (12 (ReturnedOp (var eax) (typ Int)))
          (13 (ReturnedOp (var esp) (typ Int)))
          (14 (ReturnedOp (var edx) (typ Int)))))
        (terminator (Goto (Block 29)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 14))))
       ((id 29)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713486))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int)))
          (7 (CallOp (func __func47f9eb__) (args ((Ref 5) (Ref 2) (Ref 6)))))
          (8 (ReturnedOp (var eax) (typ Int)))
          (9 (ReturnedOp (var esp) (typ Int)))
          (10 (ReturnedOp (var edx) (typ Int)))))
        (terminator (Goto (Block 30)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10))))
       ((id 30)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
          (2 (Const __i32 4294967295))
          (3 (BiOp (var __i32) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -4)))
          (5 (OutsideContext (var edi) (typ Int)))
          (6 (DupVar (var eax) (src (Ref 5)) (typ Int))) (7 (Const __i32 -124))
          (8 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 7))))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 9))))
          (11 (Const __i32 4713545))
          (12 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 11))))
          (13 (OutsideContext (var ecx) (typ Int)))
          (14 (OutsideContext (var edx) (typ Int)))
          (15 (CallOp (func __func48065f__) (args ((Ref 13) (Ref 10) (Ref 14)))))
          (16 (ReturnedOp (var eax) (typ Int)))
          (17 (ReturnedOp (var esp) (typ Int)))
          (18 (ReturnedOp (var edx) (typ Int)))
          (19 (LoadOp (var __i32) (op Load32) (addr (Ref 17))))
          (20 (OutsideContext (var __ret_addr__) (typ Int)))
          (21 (BiOp (var __i32) (op Equal) (lhs (Ref 19)) (rhs (Ref 20))))
          (22 (AssertOp (condition (Ref 21))))))
        (terminator Return)
        (roots
         ((Ref 0) (Ref 4) (Ref 5) (Ref 12) (Ref 13) (Ref 14) (Ref 15) (Ref 16)
          (Ref 17) (Ref 18) (Ref 20) (Ref 22))))))
     (locals
      ((__input_compare_arg__ ((name __input_compare_arg__) (typ Int)))
       (__ret_addr__ ((name __ret_addr__) (typ Int)))
       (eax ((name eax) (typ Int))) (ebp ((name ebp) (typ Int)))
       (ecx ((name ecx) (typ Int))) (edi ((name edi) (typ Int)))
       (edx ((name edx) (typ Int))) (esi ((name esi) (typ Int)))
       (esp ((name esp) (typ Int))))))
    |}]

let%expect_test _ =
  test_trans 4429197;
  [%expect
    {|
    ((name func_43958d)
     (signature
      ((args
        (((name ecx) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))
       (returns
        (((name eax) (typ Int)) ((name esp) (typ Int)) ((name edx) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (OutsideContext (var ebp) (typ Int))) (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
          (6 (DupVar (var ebp) (src (Ref 4)) (typ Int)))
          (7 (OutsideContext (var ecx) (typ Int))) (8 (Const __i32 4))
          (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
          (11 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7)) (offset -4)))
          (12 (LoadOp (var eax) (op Load32) (addr (Ref 6)) (offset -4)))
          (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12)) (offset 376)))
          (14 (Const __i32 4819544))
          (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 14))))
          (16 (Landmine (var eax) (typ Int))) (17 (Const __i32 65))
          (18
           (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 13))
            (rhs (Ref 15))))
          (19 (UniOp (var __i32) (op EqualsZero) (operand (Ref 18))))
          (20 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 19))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 7) (Ref 9) (Ref 10)
          (Ref 11) (Ref 16) (Ref 20))))
       ((id 1)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (2 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
          (3 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 1))))
          (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 2)) (offset 376)))
          (5 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (7 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 5))))
          (8 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (9 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 8))))
          (10 (Const __i32 4819540))
          (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
          (12 (Landmine (var eax) (typ Int))) (13 (Const __i32 1))
          (14
           (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 9))
            (rhs (Ref 11))))
          (15 (UniOp (var __i32) (op EqualsZero) (operand (Ref 14))))
          (16 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 15))))
        (roots ((Ref 0) (Ref 2) (Ref 7) (Ref 12) (Ref 16))))
       ((id 2)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (LoadOp (var eax) (op Load32) (addr (Ref 1)))) (3 (Const __i32 1))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))
          (7 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
          (9 (Const __i32 4819540))
          (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9))))
          (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 8)) (rhs (Ref 10))))
          (12 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 11))))
          (14 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator (Goto (Block 3)))
        (roots ((Ref 0) (Ref 5) (Ref 6) (Ref 12) (Ref 13) (Ref 14))))
       ((id 3) (instrs ()) (terminator (Goto (Block 5))) (roots ()))
       ((id 4)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (LoadOp (var eax) (op Load32) (addr (Ref 1)))) (3 (Const __i32 1))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))))
        (terminator (Goto (Block 5))) (roots ((Ref 0) (Ref 4) (Ref 5) (Ref 6))))
       ((id 5)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (DupVar (var esp) (src (Ref 0)) (typ Int)))
          (2 (LoadOp (var ebp) (op Load32) (addr (Ref 1)))) (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Add) (lhs (Ref 1)) (rhs (Ref 3))))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 4))))
          (6 (OutsideContext (var __ret_addr__) (typ Int)))
          (7 (BiOp (var __i32) (op Equal) (lhs (Ref 5)) (rhs (Ref 6))))
          (8 (AssertOp (condition (Ref 7)))) (9 (Const __i32 8))
          (10 (BiOp (var esp) (op Add) (lhs (Ref 4)) (rhs (Ref 9))))))
        (terminator Return) (roots ((Ref 0) (Ref 2) (Ref 6) (Ref 8) (Ref 10))))))
     (locals
      ((__ret_addr__ ((name __ret_addr__) (typ Int)))
       (eax ((name eax) (typ Int))) (ebp ((name ebp) (typ Int)))
       (ecx ((name ecx) (typ Int))) (edx ((name edx) (typ Int)))
       (esp ((name esp) (typ Int))))))
    |}]
