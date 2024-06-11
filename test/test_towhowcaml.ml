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

let%expect_test "psrlq, andpd, psubd" = 
 test_trans_block 
0x0047ee50;
 [%expect {|
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
      (8 (VecExtractLaneOp (var __vec) (src (Ref 7)) (shape I32) (lane 0)))
      (9 (DupVar (var eax) (src (Ref 8)) (typ Int))) (10 (Const __i32 4785904))
      (11 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 10))))
      (12 (BiOp (var __vec) (op VecAnd) (lhs (Ref 7)) (rhs (Ref 11))))
      (13
       (VecLaneBiOp (var __vec) (op VecAdd) (shape I32) (lhs (Ref 5))
        (rhs (Ref 12))))
      (14 (VecExtractLaneOp (var __vec) (src (Ref 13)) (shape I32) (lane 0)))
      (15
       (VecShiftRightOp (var __vec) (operand (Ref 3)) (count (Ref 14))
        (shape I64) (signed false)))
      (16 (Const __i32 2048))
      (17 (BiOp (var __i32) (op And) (lhs (Ref 9)) (rhs (Ref 16))))
      (18
       (SetGlobalOp (value (Ref 12))
        (global ((name __xmm0_global__) (typ Vec)))))
      (19
       (SetGlobalOp (value (Ref 13))
        (global ((name __xmm2_global__) (typ Vec)))))
      (20
       (SetGlobalOp (value (Ref 3)) (global ((name __xmm7_global__) (typ Vec)))))
      (21
       (SetGlobalOp (value (Ref 15))
        (global ((name __xmm1_global__) (typ Vec)))))))
    (terminator
     (Branch (succeed (Block 12)) (fail (Block 6)) (condition (Ref 17))))
    (roots ((Ref 1) (Ref 9) (Ref 18) (Ref 19) (Ref 20) (Ref 21))))
   |}]
let%expect_test "movq, psllq, cmpltpd" =
  test_trans_block 0x0047eed2;
  [%expect {|
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
       (6 (VecExtractLaneOp (var __vec) (src (Ref 5)) (shape I32) (lane 0)))
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
  [%expect {|
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
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
       (3 (DupVar (var eax) (src (Ref 2)) (typ Int)))
       (4 (OutsideContext (var esi) (typ Int))) (5 (Const __i32 4))
       (6 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 5))))
       (7 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 4))))
       (8 (OutsideContext (var ecx) (typ Int)))
       (9 (DupVar (var esi) (src (Ref 8)) (typ Int)))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 9)) (offset 4)))
       (11 (Const __i32 0))
       (12 (BiOp (var __i32) (op And) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 12)) (offset 4)))
       (14 (LoadOp (var __i32) (op Load32) (addr (Ref 9)))) (15 (Const __i32 0))
       (16 (BiOp (var __i32) (op And) (lhs (Ref 14)) (rhs (Ref 15))))
       (17 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 16))))
       (18 (DupVar (var ecx) (src (Ref 3)) (typ Int))) (19 (Const __i32 65535))
       (20 (BiOp (var __i32) (op And) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (DupVar (var ecx) (src (Ref 20)) (typ Int)))
       (22 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 3)) (offset 8)))
       (23 (UniOp (var __i32) (op EqualsZero) (operand (Ref 20))))))
     (terminator
      (Branch (succeed (Block 23)) (fail (Block 1)) (condition (Ref 23))))
     (roots
      ((Ref 0) (Ref 1) (Ref 3) (Ref 4) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
       (Ref 17) (Ref 21) (Ref 22))))
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
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -52)))
       (2 (DupVar (var edx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -44)))
       (4 (DupVar (var eax) (src (Ref 3)) (typ Int)))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 108)))
       (6 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (DupVar (var eax) (src (Ref 6)) (typ Int))) (8 (Const __i32 0))))
     (terminator
      (Branch (succeed (Block 8)) (fail (Block 6)) (condition (Ref 8))))
     (roots ((Ref 0) (Ref 2) (Ref 7)))) |}]

let%expect_test "test eax,eax jbe" =
  test_trans_block 0x0047e73c;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 16)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 2)) (offset 28)))
       (4 (UniOp (var __i32) (op EqualsZero) (operand (Ref 2))))))
     (terminator
      (Branch (succeed (Block 16)) (fail (Block 3)) (condition (Ref 4))))
     (roots ((Ref 0) (Ref 2) (Ref 3)))) |}]

let%expect_test "shld" =
  test_trans_block 0x00481fa8;
  [%expect
    {|
    ((id 211)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -92)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -88)))
       (4 (DupVar (var ecx) (src (Ref 3)) (typ Int))) (5 (Const __i32 3))
       (6 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (Const __i32 29))
       (8
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 2))
         (rhs (Ref 7))))
       (9 (BiOp (var __i32) (op Or) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (DupVar (var ecx) (src (Ref 9)) (typ Int))) (11 (Const __i32 3))
       (12 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 2)) (rhs (Ref 11))))
       (13 (DupVar (var eax) (src (Ref 12)) (typ Int)))
       (14 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 13)) (offset -92)))
       (15 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 10)) (offset -88)))))
     (terminator (Goto (Block 217)))
     (roots ((Ref 0) (Ref 10) (Ref 13) (Ref 14) (Ref 15)))) |}]

let%expect_test "repne scasb" =
  test_trans_block 0x0047dcc4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var ecx) (typ Int)))
       (1 (DupVar (var ebx) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var ebp) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 8)))
       (4 (DupVar (var edi) (src (Ref 3)) (typ Int)))
       (5 (DupVar (var esi) (src (Ref 4)) (typ Int))) (6 (Const __i32 0))
       (7 (DupVar (var eax) (src (Ref 6)) (typ Int)))
       (8 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 7))))
       (9 (CallOp (func __find_byte__) (args ((Ref 8) (Ref 4) (Ref 0)))))
       (10 (ReturnedOp (var ecx) (typ Int))) (11 (Const __i32 0))
       (12 (BiOp (var __i32) (op Subtract) (lhs (Ref 11)) (rhs (Ref 10))))
       (13 (DupVar (var ecx) (src (Ref 12)) (typ Int)))
       (14 (BiOp (var __i32) (op Add) (lhs (Ref 13)) (rhs (Ref 1))))
       (15 (DupVar (var ecx) (src (Ref 14)) (typ Int)))
       (16 (DupVar (var edi) (src (Ref 5)) (typ Int)))
       (17 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 12)))
       (18 (DupVar (var esi) (src (Ref 17)) (typ Int)))
       (19 (CallOp (func __byte_diff__) (args ((Ref 18) (Ref 16) (Ref 15)))))
       (20 (ReturnedOp (var esi) (typ Int)))
       (21 (ReturnedOp (var edi) (typ Int)))
       (22 (ReturnedOp (var ecx) (typ Int)))
       (23 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 20)) (signed false)))
       (24 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 21)) (signed false)))
       (25
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 20)) (signed false)
         (offset -1)))
       (26 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 25))))
       (27 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 26)) (rhs (Ref 7))))
       (28 (Const __i32 0)) (29 (DupVar (var ecx) (src (Ref 28)) (typ Int)))
       (30
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 21)) (signed false)
         (offset -1)))
       (31 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 27))))
       (32 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 30))))
       (33
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 31))
         (rhs (Ref 32))))
       (34 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 27))))
       (35 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 30))))
       (36 (BiOp (var __i32) (op Equal) (lhs (Ref 34)) (rhs (Ref 35))))
       (37 (DupVar (var __input_compare_arg__) (src (Ref 36)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 2)) (condition (Ref 33))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 9) (Ref 10) (Ref 19) (Ref 20) (Ref 21)
       (Ref 22) (Ref 27) (Ref 29) (Ref 37)))) |}]

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
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset 16)))
       (20 (DupVar (var ecx) (src (Ref 19)) (typ Int)))
       (21 (UniOp (var __i32) (op EqualsZero) (operand (Ref 20))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 1)) (condition (Ref 21))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 7) (Ref 10) (Ref 11)
       (Ref 14) (Ref 15) (Ref 17) (Ref 18) (Ref 20)))) |}]

let%expect_test "double fadd" =
  test_trans_block 0x00444017;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (Const __i32 6447736))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (4 (DupVar (var edx) (src (Ref 3)) (typ Int)))
       (5 (OutsideContext (var ebp) (typ Int)))
       (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4)) (offset -156)))
       (7 (Const __i32 5724880))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (Const __i32 5724884))
       (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9))))
       (11 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (Const __i32 4819536))
       (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12))))
       (14 (BiOp (var __fl) (op FloatSub) (lhs (Ref 11)) (rhs (Ref 13))))
       (15 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 14)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 15)) (offset -20)))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 5)) (offset -20)))
       (18 (Const __i32 4819532))
       (19 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 18))))
       (20 (Landmine (var eax) (typ Int))) (21 (Const __i32 5))
       (22 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 17)) (rhs (Ref 19))))
       (23 (UniOp (var __i32) (op EqualsZero) (operand (Ref 22))))
       (24 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 23))))
     (roots ((Ref 2) (Ref 4) (Ref 5) (Ref 6) (Ref 16) (Ref 20) (Ref 24))))
    |}]

let%expect_test "rcr" =
  test_trans_block 0x00482a92;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var ecx) (typ Int))) (1 (Const __i32 1))
       (2
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 0))
         (rhs (Ref 1))))
       (3 (DupVar (var ecx) (src (Ref 2)) (typ Int))) (4 (Const __i32 32))
       (5 (AssertOp (condition (Ref 1))))
       (6 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 1))))
       (7 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 6))))
       (8 (OutsideContext (var ebx) (typ Int))) (9 (Const __i32 1))
       (10
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 8))
         (rhs (Ref 9))))
       (11 (BiOp (var __i32) (op Or) (lhs (Ref 7)) (rhs (Ref 10))))
       (12 (DupVar (var ebx) (src (Ref 11)) (typ Int)))
       (13 (OutsideContext (var edx) (typ Int))) (14 (Const __i32 1))
       (15
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 13))
         (rhs (Ref 14))))
       (16 (DupVar (var edx) (src (Ref 15)) (typ Int))) (17 (Const __i32 32))
       (18 (AssertOp (condition (Ref 14))))
       (19 (BiOp (var __i32) (op Subtract) (lhs (Ref 17)) (rhs (Ref 14))))
       (20 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 13)) (rhs (Ref 19))))
       (21 (OutsideContext (var eax) (typ Int))) (22 (Const __i32 1))
       (23
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 21))
         (rhs (Ref 22))))
       (24 (BiOp (var __i32) (op Or) (lhs (Ref 20)) (rhs (Ref 23))))
       (25 (DupVar (var eax) (src (Ref 24)) (typ Int)))
       (26 (BiOp (var __i32) (op Or) (lhs (Ref 3)) (rhs (Ref 3))))
       (27 (DupVar (var ecx) (src (Ref 26)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 26))))
     (roots
      ((Ref 0) (Ref 5) (Ref 8) (Ref 12) (Ref 13) (Ref 16) (Ref 18) (Ref 21)
       (Ref 25) (Ref 27)))) |}]

let%expect_test "dumb div" =
  test_trans_block 0x00482a7c;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 20)))
       (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 16)))
       (4 (DupVar (var eax) (src (Ref 3)) (typ Int))) (5 (Const __i32 0))
       (6 (DupVar (var edx) (src (Ref 5)) (typ Int)))
       (7
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 4))
         (rhs (Ref 2))))
       (8
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 4))
         (rhs (Ref 2))))
       (9 (DupVar (var ebx) (src (Ref 7)) (typ Int)))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
       (11 (DupVar (var eax) (src (Ref 10)) (typ Int)))
       (12 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 11))))
       (13 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 8))))
       (14 (LongConst __i64 32))
       (15 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (BiOp (var __i64) (op LongOr) (lhs (Ref 12)) (rhs (Ref 15))))
       (17 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 2))))
       (18
        (SignedBiOp (var __i64) (op LongDivide) (signed false) (lhs (Ref 16))
         (rhs (Ref 17))))
       (19 (UniOp (var eax) (op LongToInt32) (operand (Ref 18))))
       (20
        (SignedBiOp (var __i64) (op LongRemainder) (signed false) (lhs (Ref 16))
         (rhs (Ref 17))))
       (21 (UniOp (var edx) (op LongToInt32) (operand (Ref 20))))
       (22 (DupVar (var edx) (src (Ref 9)) (typ Int)))))
     (terminator (Goto (Block 10)))
     (roots ((Ref 0) (Ref 2) (Ref 9) (Ref 19) (Ref 22)))) |}]

let%expect_test "mul" =
  test_trans_block 0x004816d4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 4)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 2))))
       (4 (OutsideContext (var ecx) (typ Int)))
       (5 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 4))))
       (6 (BiOp (var __i64) (op LongMultiply) (lhs (Ref 3)) (rhs (Ref 5))))
       (7 (UniOp (var eax) (op LongToInt32) (operand (Ref 6))))
       (8 (LongConst __i64 32))
       (9
        (SignedBiOp (var __i64) (op LongShiftRight) (signed false) (lhs (Ref 6))
         (rhs (Ref 8))))
       (10 (UniOp (var edx) (op LongToInt32) (operand (Ref 9))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (12 (OutsideContext (var __ret_addr__) (typ Int)))
       (13 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 12))))
       (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 16))
       (16 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 15))))))
     (terminator Return)
     (roots ((Ref 0) (Ref 4) (Ref 7) (Ref 10) (Ref 12) (Ref 14) (Ref 16)))) |}]

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
       (8 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (DupVar (var esp) (src (Ref 8)) (typ Int)))
       (10 (OutsideContext (var edi) (typ Int))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
       (14 (OutsideContext (var ecx) (typ Int)))
       (15 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 14)) (offset -76)))
       (16 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -24)))
       (17 (Const __i32 0))
       (18 (BiOp (var __i32) (op And) (lhs (Ref 16)) (rhs (Ref 17))))
       (19 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 18)) (offset -24)))
       (20 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -20)))
       (21 (Const __i32 0))
       (22 (BiOp (var __i32) (op And) (lhs (Ref 20)) (rhs (Ref 21))))
       (23 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 22)) (offset -20)))
       (24 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -28)))
       (25 (Const __i32 0))
       (26 (BiOp (var __i32) (op And) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 26)) (offset -28)))
       (28 (Const __i32 -28))
       (29 (BiOp (var __i32) (op Add) (lhs (Ref 6)) (rhs (Ref 28))))
       (30 (DupVar (var eax) (src (Ref 29)) (typ Int))) (31 (Const __i32 4))
       (32 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 31))))
       (33 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 30))))
       (34 (Const __i32 0)) (35 (Const __i32 4))
       (36 (BiOp (var esp) (op Subtract) (lhs (Ref 32)) (rhs (Ref 35))))
       (37 (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 34))))
       (38 (Const __i32 0)) (39 (Const __i32 4))
       (40 (BiOp (var esp) (op Subtract) (lhs (Ref 36)) (rhs (Ref 39))))
       (41 (StoreOp (op Store32) (addr (Ref 40)) (value (Ref 38))))
       (42 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -76)))
       (43 (DupVar (var eax) (src (Ref 42)) (typ Int)))
       (44 (LoadOp (var __i32) (op Load32) (addr (Ref 43)) (offset 8)))
       (45 (DupVar (var eax) (src (Ref 44)) (typ Int)))
       (46 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -76)))
       (47 (DupVar (var ecx) (src (Ref 46)) (typ Int)))
       (48 (LoadOp (var __i32) (op Load32) (addr (Ref 47)) (offset 8)))
       (49 (DupVar (var ecx) (src (Ref 48)) (typ Int)))
       (50 (LoadOp (var __i32) (op Load32) (addr (Ref 45))))
       (51 (DupVar (var eax) (src (Ref 50)) (typ Int))) (52 (Const __i32 4))
       (53 (BiOp (var esp) (op Subtract) (lhs (Ref 40)) (rhs (Ref 52))))
       (54 (StoreOp (op Store32) (addr (Ref 53)) (value (Ref 49))))
       (55 (Const __i32 64))
       (56 (BiOp (var __i32) (op Add) (lhs (Ref 51)) (rhs (Ref 55))))
       (57 (Const __i32 4))
       (58 (BiOp (var esp) (op Subtract) (lhs (Ref 53)) (rhs (Ref 57))))
       (59 (Const __i32 4429352))
       (60 (StoreOp (op Store32) (addr (Ref 58)) (value (Ref 59))))
       (61 (OutsideContext (var edx) (typ Int)))
       (62
        (CallIndirectOp (table_index (Ref 56))
         (args ((Ref 49) (Ref 58) (Ref 61)))))
       (63 (ReturnedOp (var eax) (typ Int)))
       (64 (ReturnedOp (var esp) (typ Int)))
       (65 (ReturnedOp (var edx) (typ Int))) (66 (Const __i32 0))
       (67 (DupVar (var eax) (src (Ref 66)) (typ Int))) (68 (Const __i32 -16))
       (69 (BiOp (var __i32) (op Add) (lhs (Ref 6)) (rhs (Ref 68))))
       (70 (DupVar (var edi) (src (Ref 69)) (typ Int)))
       (71 (StoreOp (op Store32) (addr (Ref 70)) (value (Ref 67))))
       (72 (Const __i32 4))
       (73 (BiOp (var edi) (op Add) (lhs (Ref 70)) (rhs (Ref 72))))
       (74 (StoreOp (op Store32) (addr (Ref 73)) (value (Ref 67))))
       (75 (Const __i32 4))
       (76 (BiOp (var edi) (op Add) (lhs (Ref 73)) (rhs (Ref 75))))
       (77 (StoreOp (op Store32) (addr (Ref 76)) (value (Ref 67))))
       (78 (Const __i32 4))
       (79 (BiOp (var edi) (op Add) (lhs (Ref 76)) (rhs (Ref 78))))
       (80 (UniOp (var __i32) (op SignExtend16) (operand (Ref 67))))
       (81 (StoreOp (op Store16) (addr (Ref 79)) (value (Ref 80))))
       (82 (Const __i32 2))
       (83 (BiOp (var edi) (op Add) (lhs (Ref 79)) (rhs (Ref 82))))
       (84 (Const __i32 4812696))
       (85 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 84)) (signed false)))
       (86 (UniOp (var __i32) (op SignExtend16) (operand (Ref 85))))
       (87 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 86)) (rhs (Ref 67))))
       (88 (UniOp (var __i32) (op SignExtend16) (operand (Ref 87))))
       (89 (StoreOp (op Store16) (addr (Ref 6)) (value (Ref 88)) (offset -16)))
       (90 (Const __i32 54))
       (91 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 90)) (offset -6)))
       (92 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -6)))
       (93 (DupVar (var eax) (src (Ref 92)) (typ Int)))
       (94 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 93)) (offset -14)))
       (95 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -76)))
       (96 (DupVar (var eax) (src (Ref 95)) (typ Int)))
       (97 (LoadOp (var __i32) (op Load32) (addr (Ref 96)) (offset 232)))
       (98 (DupVar (var eax) (src (Ref 97)) (typ Int)))
       (99 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 98)) (offset -80)))
       (100 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -80)))
       (101 (Const __i32 22))
       (102 (BiOp (var __i32) (op Equal) (lhs (Ref 100)) (rhs (Ref 101))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 102))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 10) (Ref 13) (Ref 14)
       (Ref 15) (Ref 19) (Ref 23) (Ref 27) (Ref 33) (Ref 37) (Ref 41) (Ref 49)
       (Ref 54) (Ref 60) (Ref 61) (Ref 62) (Ref 63) (Ref 64) (Ref 65) (Ref 71)
       (Ref 74) (Ref 77) (Ref 81) (Ref 83) (Ref 89) (Ref 91) (Ref 94) (Ref 98)
       (Ref 99)))) |}]

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
       (24 (Const __i32 4845120))
       (25 (DupVar (var esi) (src (Ref 24)) (typ Int)))
       (26 (Const __i32 5724776))
       (27 (DupVar (var edi) (src (Ref 26)) (typ Int)))
       (28 (LoadOp (var __i32) (op Load32) (addr (Ref 25))))
       (29 (StoreOp (op Store32) (addr (Ref 27)) (value (Ref 28))))
       (30 (Const __i32 4))
       (31 (BiOp (var edi) (op Add) (lhs (Ref 27)) (rhs (Ref 30))))
       (32 (Const __i32 4))
       (33 (BiOp (var esi) (op Add) (lhs (Ref 25)) (rhs (Ref 32))))
       (34 (LoadOp (var __i32) (op Load32) (addr (Ref 33))))
       (35 (StoreOp (op Store32) (addr (Ref 31)) (value (Ref 34))))
       (36 (Const __i32 4))
       (37 (BiOp (var edi) (op Add) (lhs (Ref 31)) (rhs (Ref 36))))
       (38 (Const __i32 4))
       (39 (BiOp (var esi) (op Add) (lhs (Ref 33)) (rhs (Ref 38))))
       (40 (LoadOp (var __i32) (op Load32) (addr (Ref 39))))
       (41 (StoreOp (op Store32) (addr (Ref 37)) (value (Ref 40))))
       (42 (Const __i32 4))
       (43 (BiOp (var edi) (op Add) (lhs (Ref 37)) (rhs (Ref 42))))
       (44 (Const __i32 4))
       (45 (BiOp (var esi) (op Add) (lhs (Ref 39)) (rhs (Ref 44))))
       (46 (LoadOp (var __i32) (op Load32) (addr (Ref 45))))
       (47 (StoreOp (op Store32) (addr (Ref 43)) (value (Ref 46))))
       (48 (Const __i32 4))
       (49 (BiOp (var edi) (op Add) (lhs (Ref 43)) (rhs (Ref 48))))
       (50 (Const __i32 4))
       (51 (BiOp (var esi) (op Add) (lhs (Ref 45)) (rhs (Ref 50))))
       (52 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 51)) (signed false)))
       (53 (UniOp (var __i32) (op SignExtend16) (operand (Ref 52))))
       (54 (StoreOp (op Store16) (addr (Ref 49)) (value (Ref 53))))
       (55 (Const __i32 2))
       (56 (BiOp (var edi) (op Add) (lhs (Ref 49)) (rhs (Ref 55))))
       (57 (Const __i32 2))
       (58 (BiOp (var esi) (op Add) (lhs (Ref 51)) (rhs (Ref 57))))
       (59 (Const __i32 2))
       (60 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 59))))
       (61 (Const __i32 5724812))
       (62 (StoreOp (op Store8) (addr (Ref 61)) (value (Ref 60))))
       (63 (Const __i32 5724813))
       (64 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 63)) (signed false)))
       (65 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 64))))
       (66 (Const __i32 0))
       (67 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 66))))
       (68 (BiOp (var __i32) (op And) (lhs (Ref 65)) (rhs (Ref 67))))
       (69 (Const __i32 5724813))
       (70 (StoreOp (op Store8) (addr (Ref 69)) (value (Ref 68))))
       (71 (Const __i32 1))
       (72 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 71))))
       (73 (Const __i32 5724814))
       (74 (StoreOp (op Store8) (addr (Ref 73)) (value (Ref 72))))))
     (terminator (Goto (Block 30)))
     (roots
      ((Ref 3) (Ref 7) (Ref 15) (Ref 23) (Ref 29) (Ref 35) (Ref 41) (Ref 47)
       (Ref 54) (Ref 56) (Ref 58) (Ref 62) (Ref 70) (Ref 74)))) |}]

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
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (DupVar (var edi) (src (Ref 5)) (typ Int)))
       (7 (OutsideContext (var edx) (typ Int))) (8 (Const __i32 1))
       (9 (BiOp (var __i32) (op Subtract) (lhs (Ref 7)) (rhs (Ref 8))))
       (10 (DupVar (var edx) (src (Ref 9)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 7)) (fail (Block 8)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 2) (Ref 3) (Ref 6) (Ref 7) (Ref 10)))) |}]

let%expect_test "rep stosd (nonzero)" =
  test_trans_block 0x0042d5ee;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const __i32 180)) (1 (DupVar (var ecx) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var eax) (typ Int))) (3 (Const __i32 4294967295))
       (4 (BiOp (var __i32) (op Or) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (DupVar (var eax) (src (Ref 4)) (typ Int))) (6 (Const __i32 5724496))
       (7 (DupVar (var edi) (src (Ref 6)) (typ Int)))
       (8 (CallOp (func __int_memset__) (args ((Ref 7) (Ref 5) (Ref 1)))))
       (9 (Const ecx 0))))
     (terminator (Goto (Block 2)))
     (roots ((Ref 2) (Ref 5) (Ref 7) (Ref 8) (Ref 9)))) |}]

let%expect_test "rep movsb" =
  test_trans_block 0x004446bb;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (DupVar (var edx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (4 (DupVar (var eax) (src (Ref 3)) (typ Int))) (5 (Const __i32 4))
       (6 (BiOp (var __i32) (op Multiply) (lhs (Ref 2)) (rhs (Ref 5))))
       (7 (BiOp (var __i32) (op Add) (lhs (Ref 6)) (rhs (Ref 4))))
       (8 (LoadOp (var __i32) (op Load32) (addr (Ref 7)) (offset 36)))
       (9 (DupVar (var ecx) (src (Ref 8)) (typ Int)))
       (10 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 9)) (offset -24)))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (12 (DupVar (var ecx) (src (Ref 11)) (typ Int)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (14 (DupVar (var edx) (src (Ref 13)) (typ Int)))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 14)) (offset 4)))
       (16 (DupVar (var eax) (src (Ref 15)) (typ Int)))
       (17 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (18 (DupVar (var edx) (src (Ref 17)) (typ Int))) (19 (Const __i32 4))
       (20 (BiOp (var __i32) (op Multiply) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (BiOp (var __i32) (op Add) (lhs (Ref 20)) (rhs (Ref 16))))
       (22 (LoadOp (var __i32) (op Load32) (addr (Ref 21)) (offset 56)))
       (23 (DupVar (var esi) (src (Ref 22)) (typ Int)))
       (24 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -272)))
       (25 (DupVar (var eax) (src (Ref 24)) (typ Int)))
       (26 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -28)))
       (27 (DupVar (var edx) (src (Ref 26)) (typ Int)))
       (28 (BiOp (var __i32) (op Add) (lhs (Ref 25)) (rhs (Ref 27))))
       (29 (Const __i32 -84))
       (30 (BiOp (var __i32) (op Add) (lhs (Ref 28)) (rhs (Ref 29))))
       (31 (DupVar (var edi) (src (Ref 30)) (typ Int)))
       (32 (DupVar (var eax) (src (Ref 12)) (typ Int))) (33 (Const __i32 2))
       (34 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 33))))
       (35
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 12))
         (rhs (Ref 34))))
       (36 (DupVar (var ecx) (src (Ref 35)) (typ Int))) (37 (Const __i32 2))
       (38 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 36)) (rhs (Ref 37))))
       (39 (Memcopy (count (Ref 38)) (src (Ref 23)) (dest (Ref 31))))
       (40 (Const ecx 0)) (41 (DupVar (var ecx) (src (Ref 32)) (typ Int)))
       (42 (Const __i32 3))
       (43 (BiOp (var __i32) (op And) (lhs (Ref 41)) (rhs (Ref 42))))
       (44 (DupVar (var ecx) (src (Ref 43)) (typ Int)))
       (45 (Memcopy (count (Ref 44)) (src (Ref 23)) (dest (Ref 31))))
       (46 (Const ecx 0))
       (47 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (48 (DupVar (var ecx) (src (Ref 47)) (typ Int)))
       (49 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -272)))
       (50 (DupVar (var edx) (src (Ref 49)) (typ Int))) (51 (Const __i32 4))
       (52 (BiOp (var __i32) (op Multiply) (lhs (Ref 48)) (rhs (Ref 51))))
       (53 (BiOp (var __i32) (op Add) (lhs (Ref 52)) (rhs (Ref 0))))
       (54 (StoreOp (op Store32) (addr (Ref 53)) (value (Ref 50)) (offset -208)))
       (55 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -272)))
       (56 (DupVar (var eax) (src (Ref 55)) (typ Int)))
       (57 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (58 (BiOp (var __i32) (op Add) (lhs (Ref 56)) (rhs (Ref 57))))
       (59 (DupVar (var eax) (src (Ref 58)) (typ Int)))
       (60 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 59)) (offset -272)))))
     (terminator (Goto (Block 21)))
     (roots
      ((Ref 0) (Ref 10) (Ref 23) (Ref 31) (Ref 39) (Ref 45) (Ref 48) (Ref 50)
       (Ref 54) (Ref 59) (Ref 60)))) |}]

let%expect_test "repe cmpsd" =
  test_trans_block 0x00444521;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (Const __i32 14)) (1 (DupVar (var ecx) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var ebp) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset -8)))
       (4 (DupVar (var edx) (src (Ref 3)) (typ Int)))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 4)) (offset 4)))
       (6 (DupVar (var edi) (src (Ref 5)) (typ Int))) (7 (Const __i32 112))
       (8 (BiOp (var __i32) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (DupVar (var edi) (src (Ref 8)) (typ Int))) (10 (Const __i32 5724776))
       (11 (DupVar (var esi) (src (Ref 10)) (typ Int))) (12 (Const __i32 0))
       (13 (DupVar (var eax) (src (Ref 12)) (typ Int)))
       (14 (CallOp (func __int_diff__) (args ((Ref 11) (Ref 9) (Ref 1)))))
       (15 (ReturnedOp (var esi) (typ Int)))
       (16 (ReturnedOp (var edi) (typ Int)))
       (17 (ReturnedOp (var ecx) (typ Int)))
       (18 (LoadOp (var __i32) (op Load32) (addr (Ref 15))))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 16))))
       (20 (BiOp (var __i32) (op Equal) (lhs (Ref 18)) (rhs (Ref 19))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 4)) (condition (Ref 20))))
     (roots ((Ref 2) (Ref 4) (Ref 13) (Ref 14) (Ref 15) (Ref 16) (Ref 17)))) |}]

let%expect_test "and jns" =
  test_trans_block 0x0043dbb0;
  [%expect
    {|
    ((id 18)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 836)))
       (4 (DupVar (var edx) (src (Ref 3)) (typ Int)))
       (5 (Const __i32 2147483649))
       (6 (BiOp (var __i32) (op And) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (DupVar (var edx) (src (Ref 6)) (typ Int))) (8 (Const __i32 0))
       (9
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 6))
         (rhs (Ref 8))))))
     (terminator
      (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 2) (Ref 7)))) |}]

let%expect_test "fidivr" =
  test_trans_block 0x00414d50;
  [%expect
    {|
    ((id 818)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 11196)))
       (4 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 3))))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1656)))
       (6 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 5))))
       (7 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 6)) (rhs (Ref 4))))
       (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -616)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (10 (DupVar (var ecx) (src (Ref 9)) (typ Int)))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10)) (offset 11196)))
       (12 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 11))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1660)))
       (14 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 13))))
       (15 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 14)) (rhs (Ref 12))))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 15)) (offset -620)))
       (17 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -60)))
       (18 (DupVar (var edx) (src (Ref 17)) (typ Int)))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -620)))
       (20 (DupVar (var eax) (src (Ref 19)) (typ Int))) (21 (Const __i32 4))
       (22 (BiOp (var __i32) (op Multiply) (lhs (Ref 18)) (rhs (Ref 21))))
       (23
        (StoreOp (op Store32) (addr (Ref 22)) (value (Ref 20)) (offset 4848708)))
       (24 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -60)))
       (25 (DupVar (var ecx) (src (Ref 24)) (typ Int)))
       (26 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -616)))
       (27 (DupVar (var edx) (src (Ref 26)) (typ Int))) (28 (Const __i32 4))
       (29 (BiOp (var __i32) (op Multiply) (lhs (Ref 25)) (rhs (Ref 28))))
       (30
        (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 27)) (offset 4848676)))
       (31 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (32 (DupVar (var eax) (src (Ref 31)) (typ Int)))
       (33
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 32)) (signed false)
         (offset 10)))
       (34 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 33))))
       (35 (DupVar (var ecx) (src (Ref 34)) (typ Int))) (36 (Const __i32 8))
       (37 (BiOp (var __i32) (op And) (lhs (Ref 35)) (rhs (Ref 36))))
       (38 (DupVar (var ecx) (src (Ref 37)) (typ Int)))
       (39 (UniOp (var __i32) (op EqualsZero) (operand (Ref 38))))
       (40 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 820)) (fail (Block 819)) (condition (Ref 39))))
     (roots
      ((Ref 0) (Ref 8) (Ref 16) (Ref 23) (Ref 27) (Ref 30) (Ref 32) (Ref 38)
       (Ref 40))))
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
       (15 (GetGlobalOp (var __i32) (global ((name __seh_frame__) (typ Int)))))
       (16 (DupVar (var eax) (src (Ref 15)) (typ Int))) (17 (Const __i32 4))
       (18 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 17))))
       (19 (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 16))))
       (20
        (SetGlobalOp (value (Ref 18)) (global ((name __seh_frame__) (typ Int)))))
       (21 (Const __i32 20))
       (22 (BiOp (var __i32) (op Subtract) (lhs (Ref 18)) (rhs (Ref 21))))
       (23 (DupVar (var esp) (src (Ref 22)) (typ Int)))
       (24 (OutsideContext (var ecx) (typ Int)))
       (25 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 24)) (offset -28)))
       (26 (Const __i32 32)) (27 (Const __i32 4))
       (28 (BiOp (var esp) (op Subtract) (lhs (Ref 23)) (rhs (Ref 27))))
       (29 (StoreOp (op Store32) (addr (Ref 28)) (value (Ref 26))))
       (30 (Const __i32 4))
       (31 (BiOp (var esp) (op Subtract) (lhs (Ref 28)) (rhs (Ref 30))))
       (32 (Const __i32 4391088))
       (33 (StoreOp (op Store32) (addr (Ref 31)) (value (Ref 32))))
       (34 (OutsideContext (var edx) (typ Int)))
       (35 (CallOp (func __func47d441__) (args ((Ref 24) (Ref 31) (Ref 34)))))
       (36 (ReturnedOp (var eax) (typ Int)))
       (37 (ReturnedOp (var esp) (typ Int)))
       (38 (ReturnedOp (var edx) (typ Int))) (39 (Const __i32 4))
       (40 (BiOp (var __i32) (op Add) (lhs (Ref 37)) (rhs (Ref 39))))
       (41 (DupVar (var esp) (src (Ref 40)) (typ Int)))
       (42 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 36)) (offset -24)))
       (43 (Const __i32 0))
       (44 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 43)) (offset -4)))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -24)))
       (46 (Const __i32 0))
       (47 (BiOp (var __i32) (op Equal) (lhs (Ref 45)) (rhs (Ref 46))))))
     (terminator
      (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 47))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 10) (Ref 14) (Ref 15)
       (Ref 19) (Ref 20) (Ref 24) (Ref 25) (Ref 29) (Ref 33) (Ref 34) (Ref 35)
       (Ref 36) (Ref 37) (Ref 38) (Ref 41) (Ref 42) (Ref 44))))
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
       (2 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 1))))
       (3 (DupVar (var eax) (src (Ref 2)) (typ Int))) (4 (Const __i32 1))
       (5 (DupVar (var edx) (src (Ref 4)) (typ Int)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
       (7 (DupVar (var ecx) (src (Ref 6)) (typ Int)))
       (8 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 7))))
       (9 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 5)) (rhs (Ref 8))))
       (10 (DupVar (var edx) (src (Ref 9)) (typ Int)))
       (11 (BiOp (var __i32) (op And) (lhs (Ref 3)) (rhs (Ref 10))))
       (12 (DupVar (var eax) (src (Ref 11)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 12))))
     (roots ((Ref 0) (Ref 7) (Ref 10) (Ref 12)))) |}]

let%expect_test "cld" =
  test_trans_block 0x0048c237;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int))) (1 (Const __i32 8))
       (2 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (DupVar (var esi) (src (Ref 2)) (typ Int))) (4 (Const __i32 -134))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 4))))
       (6 (DupVar (var edi) (src (Ref 5)) (typ Int)))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 3))))
       (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
       (9 (Const __i32 4))
       (10 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 9))))
       (11 (Const __i32 4))
       (12 (BiOp (var esi) (op Add) (lhs (Ref 3)) (rhs (Ref 11))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 13))))
       (15 (Const __i32 4))
       (16 (BiOp (var edi) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
       (17 (Const __i32 4))
       (18 (BiOp (var esi) (op Add) (lhs (Ref 12)) (rhs (Ref 17))))
       (19 (OutsideContext (var ebx) (typ Int)))
       (20
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 19)) (signed false)
         (offset 12)))
       (21 (Const __i32 1))
       (22 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 20))))
       (23 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 21))))
       (24 (BiOp (var __i32) (op Equal) (lhs (Ref 22)) (rhs (Ref 23))))))
     (terminator
      (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 24))))
     (roots ((Ref 0) (Ref 8) (Ref 14) (Ref 16) (Ref 18) (Ref 19)))) |}]

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
       (13 (BiOp (var __i32) (op FloatGreaterThan) (lhs (Ref 9)) (rhs (Ref 11))))
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
       (2 (BiOp (var __i32) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
       (3 (DupVar (var eax) (src (Ref 2)) (typ Int)))
       (4 (OutsideContext (var esp) (typ Int)))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (DupVar (var eax) (src (Ref 5)) (typ Int))) (7 (Const __i32 4))
       (8 (BiOp (var __i32) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (DupVar (var eax) (src (Ref 8)) (typ Int)))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
       (11 (DupVar (var esp) (src (Ref 9)) (typ Int)))
       (12 (DupVar (var eax) (src (Ref 4)) (typ Int)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 12))))
       (14 (DupVar (var eax) (src (Ref 13)) (typ Int))) (15 (Const __i32 4))
       (16 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 15))))
       (17 (StoreOp (op Store32) (addr (Ref 16)) (value (Ref 14))))
       (18 (LoadOp (var __i32) (op Load32) (addr (Ref 16))))
       (19 (OutsideContext (var __ret_addr__) (typ Int)))
       (20 (BiOp (var __i32) (op Equal) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (AssertOp (condition (Ref 20))))))
     (terminator Return)
     (roots ((Ref 0) (Ref 4) (Ref 14) (Ref 16) (Ref 17) (Ref 19) (Ref 21)))) |}]

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
       (18 (BiOp (var __i32) (op Add) (lhs (Ref 10)) (rhs (Ref 17))))
       (19 (DupVar (var esi) (src (Ref 18)) (typ Int)))
       (20 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 16)) (offset -4)))
       (21 (Const __i32 -8))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 21))))
       (23
        (SetGlobalOp (value (Ref 22)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 3)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 7) (Ref 10) (Ref 11) (Ref 14) (Ref 19)
       (Ref 20) (Ref 23))))
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
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (5 (DupVar (var ecx) (src (Ref 4)) (typ Int)))
       (6 (Const __i32 2147483647))
       (7 (BiOp (var __i32) (op Add) (lhs (Ref 5)) (rhs (Ref 6))))
       (8 (DupVar (var ecx) (src (Ref 7)) (typ Int))) (9 (Const __i32 0))
       (10
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 7))
         (rhs (Ref 5))))
       (11 (BiOp (var __i32) (op Add) (lhs (Ref 10)) (rhs (Ref 9))))
       (12 (OutsideContext (var eax) (typ Int)))
       (13 (BiOp (var __i32) (op Subtract) (lhs (Ref 12)) (rhs (Ref 11))))
       (14 (DupVar (var eax) (src (Ref 13)) (typ Int)))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 20)))
       (16 (DupVar (var edx) (src (Ref 15)) (typ Int))) (17 (Const __i32 0))
       (18
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 13))
         (rhs (Ref 12))))
       (19 (BiOp (var __i32) (op Add) (lhs (Ref 18)) (rhs (Ref 17))))
       (20 (BiOp (var __i32) (op Subtract) (lhs (Ref 16)) (rhs (Ref 19))))
       (21 (DupVar (var edx) (src (Ref 20)) (typ Int))) (22 (Const __i32 -8))
       (23 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 22))))
       (24
        (SetGlobalOp (value (Ref 23)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 8) (Ref 12) (Ref 14) (Ref 21) (Ref 24))))
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
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (5 (DupVar (var ecx) (src (Ref 4)) (typ Int)))
       (6 (Const __i32 2147483648))
       (7 (BiOp (var __i32) (op Xor) (lhs (Ref 5)) (rhs (Ref 6))))
       (8 (DupVar (var ecx) (src (Ref 7)) (typ Int)))
       (9 (Const __i32 2147483647))
       (10 (BiOp (var __i32) (op Add) (lhs (Ref 8)) (rhs (Ref 9))))
       (11 (DupVar (var ecx) (src (Ref 10)) (typ Int))) (12 (Const __i32 0))
       (13
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 10))
         (rhs (Ref 8))))
       (14 (BiOp (var __i32) (op Add) (lhs (Ref 13)) (rhs (Ref 12))))
       (15 (OutsideContext (var eax) (typ Int)))
       (16 (BiOp (var __i32) (op Add) (lhs (Ref 14)) (rhs (Ref 15))))
       (17 (DupVar (var eax) (src (Ref 16)) (typ Int)))
       (18 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 20)))
       (19 (DupVar (var edx) (src (Ref 18)) (typ Int))) (20 (Const __i32 0))
       (21
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 16))
         (rhs (Ref 14))))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 21)) (rhs (Ref 20))))
       (23 (BiOp (var __i32) (op Add) (lhs (Ref 22)) (rhs (Ref 19))))
       (24 (DupVar (var edx) (src (Ref 23)) (typ Int))) (25 (Const __i32 -8))
       (26 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 25))))
       (27
        (SetGlobalOp (value (Ref 26)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6)))
     (roots
      ((Ref 0) (Ref 2) (Ref 3) (Ref 11) (Ref 15) (Ref 17) (Ref 24) (Ref 27))))
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
       (8 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (DupVar (var esp) (src (Ref 8)) (typ Int)))
       (10 (Const __i32 4294967280))
       (11 (BiOp (var __i32) (op And) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (DupVar (var esp) (src (Ref 11)) (typ Int)))
       (13 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (14 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 13))))
       (15 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 14)) (offset 24)))
       (16 (UniOp (var __i64) (op FloatToLong) (operand (Ref 14))))
       (17
        (StoreOp (op LongStore64) (addr (Ref 12)) (value (Ref 16)) (offset 16)))
       (18 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 12)) (offset 16)))
       (19 (UniOp (var __fl) (op Int64ToFloatSigned) (operand (Ref 18))))
       (20 (LoadOp (var __i32) (op Load32) (addr (Ref 12)) (offset 24)))
       (21 (DupVar (var edx) (src (Ref 20)) (typ Int)))
       (22 (LoadOp (var __i32) (op Load32) (addr (Ref 12)) (offset 16)))
       (23 (DupVar (var eax) (src (Ref 22)) (typ Int)))
       (24 (UniOp (var __i32) (op EqualsZero) (operand (Ref 23))))
       (25 (StoreOp (op FloatStore64) (addr (Ref 13)) (value (Ref 14))))
       (26
        (StoreOp (op FloatStore64) (addr (Ref 13)) (value (Ref 19)) (offset 8)))
       (27 (Const __i32 8))
       (28 (BiOp (var __i32) (op Add) (lhs (Ref 13)) (rhs (Ref 27))))
       (29
        (SetGlobalOp (value (Ref 28)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 24))))
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 12) (Ref 13) (Ref 15)
       (Ref 17) (Ref 21) (Ref 23) (Ref 25) (Ref 26) (Ref 29))))
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
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed false)
         (offset 37)))
       (4 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 3))))
       (5 (DupVar (var ecx) (src (Ref 4)) (typ Int))) (6 (Const __i32 2))
       (7 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 5)) (rhs (Ref 6))))
       (8 (DupVar (var ecx) (src (Ref 7)) (typ Int)))
       (9 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 8)) (offset -64)))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -64)))
       (11 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 10))))
       (12 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (13 (DupVar (var edx) (src (Ref 12)) (typ Int)))
       (14 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 13)) (offset 8)))
       (15 (BiOp (var __fl) (op FloatSub) (lhs (Ref 14)) (rhs (Ref 11))))
       (16 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -60)))
       (17 (DupVar (var eax) (src (Ref 16)) (typ Int)))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15)) (offset 1044)))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -60)))
       (20 (DupVar (var ecx) (src (Ref 19)) (typ Int)))
       (21 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (22 (DupVar (var edx) (src (Ref 21)) (typ Int)))
       (23 (LoadOp (var __i32) (op Load32) (addr (Ref 22)) (offset 12)))
       (24 (DupVar (var eax) (src (Ref 23)) (typ Int)))
       (25 (StoreOp (op Store32) (addr (Ref 20)) (value (Ref 24)) (offset 1048)))
       (26 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -60)))
       (27 (DupVar (var ecx) (src (Ref 26)) (typ Int)))
       (28 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (29 (DupVar (var edx) (src (Ref 28)) (typ Int)))
       (30 (LoadOp (var __i32) (op Load32) (addr (Ref 29)) (offset 20)))
       (31 (DupVar (var eax) (src (Ref 30)) (typ Int)))
       (32 (StoreOp (op Store32) (addr (Ref 27)) (value (Ref 31)) (offset 1028)))
       (33 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (34 (DupVar (var ecx) (src (Ref 33)) (typ Int)))
       (35 (Const __i32 4973576))
       (36 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 35))))
       (37 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 34)) (offset 8)))
       (38 (BiOp (var __fl) (op FloatSub) (lhs (Ref 36)) (rhs (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 38)) (offset -16)))
       (40 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (41 (DupVar (var edx) (src (Ref 40)) (typ Int)))
       (42 (Const __i32 4973580))
       (43 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 42))))
       (44 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 41)) (offset 12)))
       (45 (BiOp (var __fl) (op FloatSub) (lhs (Ref 43)) (rhs (Ref 44))))
       (46 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 45)) (offset -12)))
       (47 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (48 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (49 (BiOp (var __fl) (op FloatMult) (lhs (Ref 47)) (rhs (Ref 48))))
       (50 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (51 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (52 (BiOp (var __fl) (op FloatMult) (lhs (Ref 50)) (rhs (Ref 51))))
       (53 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 49)) (rhs (Ref 52))))
       (54 (Const __i32 4)) (55 (OutsideContext (var esp) (typ Int)))
       (56 (BiOp (var esp) (op Subtract) (lhs (Ref 55)) (rhs (Ref 54))))
       (57 (Const __i32 4212587))
       (58 (StoreOp (op Store32) (addr (Ref 56)) (value (Ref 57))))
       (59 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (60
        (StoreOp (op FloatStore64) (addr (Ref 59)) (value (Ref 53)) (offset 8)))
       (61 (Const __i32 8))
       (62 (BiOp (var __i32) (op Add) (lhs (Ref 59)) (rhs (Ref 61))))
       (63
        (SetGlobalOp (value (Ref 62)) (global ((name __fpuStack__) (typ Int)))))
       (64 (CallOp (func __func48b8a0__) (args ((Ref 34) (Ref 56) (Ref 41)))))
       (65 (ReturnedOp (var eax) (typ Int)))
       (66 (ReturnedOp (var esp) (typ Int)))
       (67 (ReturnedOp (var edx) (typ Int)))
       (68 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 65)) (offset -8)))
       (69 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (70 (Const __i32 4096))
       (71
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 69))
         (rhs (Ref 70))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 8)) (condition (Ref 71))))
     (roots
      ((Ref 0) (Ref 9) (Ref 18) (Ref 25) (Ref 32) (Ref 34) (Ref 39) (Ref 46)
       (Ref 55) (Ref 58) (Ref 59) (Ref 60) (Ref 63) (Ref 64) (Ref 65) (Ref 66)
       (Ref 67) (Ref 68))))
    |}]

let%expect_test "rep movsd" =
  test_trans_block 0x00403ad5;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -604)))
       (2 (DupVar (var esi) (src (Ref 1)) (typ Int))) (3 (Const __i32 5888))
       (4 (BiOp (var __i32) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (DupVar (var esi) (src (Ref 4)) (typ Int))) (6 (Const __i32 147))
       (7 (DupVar (var ecx) (src (Ref 6)) (typ Int))) (8 (Const __i32 -600))
       (9 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 8))))
       (10 (DupVar (var edi) (src (Ref 9)) (typ Int))) (11 (Const __i32 2))
       (12 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 7)) (rhs (Ref 11))))
       (13 (Memcopy (count (Ref 12)) (src (Ref 5)) (dest (Ref 10))))
       (14 (Const ecx 0))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -152)))
       (16 (DupVar (var eax) (src (Ref 15)) (typ Int))) (17 (Const __i32 4096))
       (18 (BiOp (var __i32) (op Or) (lhs (Ref 16)) (rhs (Ref 17))))
       (19 (DupVar (var eax) (src (Ref 18)) (typ Int)))
       (20 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 19)) (offset -152)))
       (21 (Const __i32 -600))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 21))))
       (23 (DupVar (var ecx) (src (Ref 22)) (typ Int)))
       (24 (OutsideContext (var esp) (typ Int))) (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 23))))
       (28 (Const __i32 4955716))
       (29 (LoadOp (var __i32) (op Load32) (addr (Ref 28))))
       (30 (DupVar (var ecx) (src (Ref 29)) (typ Int))) (31 (Const __i32 4))
       (32 (BiOp (var esp) (op Subtract) (lhs (Ref 26)) (rhs (Ref 31))))
       (33 (Const __i32 4209397))
       (34 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 33))))
       (35 (OutsideContext (var edx) (typ Int)))
       (36 (CallOp (func __func44f770__) (args ((Ref 30) (Ref 32) (Ref 35)))))
       (37 (ReturnedOp (var eax) (typ Int)))
       (38 (ReturnedOp (var esp) (typ Int)))
       (39 (ReturnedOp (var edx) (typ Int)))))
     (terminator (Goto (Block 4)))
     (roots
      ((Ref 0) (Ref 5) (Ref 10) (Ref 13) (Ref 20) (Ref 24) (Ref 27) (Ref 30)
       (Ref 34) (Ref 35) (Ref 36) (Ref 37) (Ref 38) (Ref 39)))) |}]

let%expect_test "fabs" =
  test_trans_block 0x004023c9;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -8)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int))) (3 (Const __i32 588))
       (4 (BiOp (var __i32) (op Multiply) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (DupVar (var eax) (src (Ref 4)) (typ Int)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -40)))
       (7 (DupVar (var ecx) (src (Ref 6)) (typ Int)))
       (8 (BiOp (var __i32) (op Add) (lhs (Ref 5)) (rhs (Ref 7))))
       (9 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 8)) (offset 3396)))
       (10 (Const __i32 4819588))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
       (12 (BiOp (var __fl) (op FloatSub) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (Const __i32 4973576))
       (14 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 13))))
       (15 (BiOp (var __fl) (op FloatSub) (lhs (Ref 12)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 15)) (offset -36)))
       (17 (UniOp (var __fl) (op FloatAbs) (operand (Ref 15))))
       (18 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 17)) (offset -44)))
       (19 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 17)) (offset -4)))
       (20 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -4)))
       (21 (Const __i32 4819720))
       (22 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 21))))
       (23 (Landmine (var eax) (typ Int))) (24 (Const __i32 5))
       (25 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 20)) (rhs (Ref 22))))
       (26 (UniOp (var __i32) (op EqualsZero) (operand (Ref 25))))
       (27 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 26))))
     (roots ((Ref 0) (Ref 7) (Ref 16) (Ref 18) (Ref 19) (Ref 23) (Ref 27))))
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
       (8 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (DupVar (var esp) (src (Ref 8)) (typ Int)))
       (10 (OutsideContext (var edi) (typ Int))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
       (14 (OutsideContext (var ecx) (typ Int)))
       (15 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 14)) (offset -36)))
       (16 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -36)))
       (17 (DupVar (var ecx) (src (Ref 16)) (typ Int))) (18 (Const __i32 4))
       (19 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 18))))
       (20 (Const __i32 4198781))
       (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
       (22 (OutsideContext (var edx) (typ Int)))
       (23 (CallOp (func __func4011b0__) (args ((Ref 17) (Ref 19) (Ref 22)))))
       (24 (ReturnedOp (var eax) (typ Int)))
       (25 (ReturnedOp (var esp) (typ Int)))
       (26 (ReturnedOp (var edx) (typ Int))) (27 (Const __i32 147))
       (28 (DupVar (var ecx) (src (Ref 27)) (typ Int))) (29 (Const __i32 0))
       (30 (DupVar (var eax) (src (Ref 29)) (typ Int)))
       (31 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -36)))
       (32 (DupVar (var edi) (src (Ref 31)) (typ Int))) (33 (Const __i32 2))
       (34 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 28)) (rhs (Ref 33))))
       (35 (Const __i32 0))
       (36 (Memset (count (Ref 34)) (value (Ref 35)) (dest (Ref 32))))
       (37 (Const ecx 0))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -36)))
       (39 (DupVar (var eax) (src (Ref 38)) (typ Int))) (40 (Const __i32 65535))
       (41 (UniOp (var __i32) (op SignExtend16) (operand (Ref 40))))
       (42 (StoreOp (op Store16) (addr (Ref 39)) (value (Ref 41)) (offset 468)))
       (43 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -36)))
       (44 (DupVar (var eax) (src (Ref 43)) (typ Int)))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 25)))) (46 (Const __i32 4))
       (47 (BiOp (var esp) (op Add) (lhs (Ref 25)) (rhs (Ref 46))))
       (48 (DupVar (var edi) (src (Ref 45)) (typ Int)))
       (49 (DupVar (var esp) (src (Ref 6)) (typ Int)))
       (50 (LoadOp (var __i32) (op Load32) (addr (Ref 49)))) (51 (Const __i32 4))
       (52 (BiOp (var esp) (op Add) (lhs (Ref 49)) (rhs (Ref 51))))
       (53 (DupVar (var ebp) (src (Ref 50)) (typ Int)))
       (54 (LoadOp (var __i32) (op Load32) (addr (Ref 52))))
       (55 (BiOp (var __i32) (op Equal) (lhs (Ref 54)) (rhs (Ref 1))))
       (56 (AssertOp (condition (Ref 55))))))
     (terminator Return)
     (roots
      ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 10) (Ref 13) (Ref 14) (Ref 15)
       (Ref 21) (Ref 22) (Ref 23) (Ref 24) (Ref 25) (Ref 26) (Ref 36) (Ref 37)
       (Ref 42) (Ref 44) (Ref 48) (Ref 52) (Ref 53) (Ref 56)))) |}]

let%expect_test "fild/fiadd" =
  test_trans_block 0x00453df6;
  [%expect
    {|
    ((id 510)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (DupVar (var edx) (src (Ref 1)) (typ Int)))
       (3
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed false)
         (offset 559)))
       (4 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 3))))
       (5 (DupVar (var eax) (src (Ref 4)) (typ Int)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (7 (DupVar (var ecx) (src (Ref 6)) (typ Int)))
       (8
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 7)) (signed false)
         (offset 555)))
       (9 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 8))))
       (10 (DupVar (var edx) (src (Ref 9)) (typ Int)))
       (11 (BiOp (var __i32) (op Subtract) (lhs (Ref 5)) (rhs (Ref 10))))
       (12 (DupVar (var eax) (src (Ref 11)) (typ Int)))
       (13 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 12)) (offset -792)))
       (14 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -792)))
       (15 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 14))))
       (16 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (17 (BiOp (var __fl) (op FloatMult) (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (19 (DupVar (var eax) (src (Ref 18)) (typ Int)))
       (20
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 19)) (signed false)
         (offset 555)))
       (21 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 20))))
       (22 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
       (23 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 22)) (offset -796)))
       (24 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -796)))
       (25 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 24))))
       (26 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 17)) (rhs (Ref 25))))
       (27 (Const __i32 4)) (28 (OutsideContext (var esp) (typ Int)))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 28)) (rhs (Ref 27))))
       (30 (Const __i32 4537877))
       (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
       (32 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (33
        (StoreOp (op FloatStore64) (addr (Ref 32)) (value (Ref 26)) (offset 8)))
       (34 (Const __i32 8))
       (35 (BiOp (var __i32) (op Add) (lhs (Ref 32)) (rhs (Ref 34))))
       (36
        (SetGlobalOp (value (Ref 35)) (global ((name __fpuStack__) (typ Int)))))
       (37 (CallOp (func __func48b8a0__) (args ((Ref 22) (Ref 29) (Ref 10)))))
       (38 (ReturnedOp (var eax) (typ Int)))
       (39 (ReturnedOp (var esp) (typ Int)))
       (40 (ReturnedOp (var edx) (typ Int)))
       (41 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (42 (DupVar (var edx) (src (Ref 41)) (typ Int)))
       (43 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 38))))
       (44 (StoreOp (op Store8) (addr (Ref 42)) (value (Ref 43)) (offset 443)))))
     (terminator (Goto (Block 513)))
     (roots
      ((Ref 0) (Ref 13) (Ref 22) (Ref 23) (Ref 28) (Ref 31) (Ref 32) (Ref 33)
       (Ref 36) (Ref 37) (Ref 38) (Ref 39) (Ref 40) (Ref 42) (Ref 44))))
    |}]

let%expect_test "div" =
  test_trans_block 0x00452f62;
  [%expect
    {|
    ((id 341)
     (instrs
      ((0 (Const __i32 4849184)) (1 (DupVar (var ecx) (src (Ref 0)) (typ Int)))
       (2 (Const __i32 4)) (3 (OutsideContext (var esp) (typ Int)))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
       (5 (Const __i32 4534107))
       (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
       (7 (OutsideContext (var edx) (typ Int)))
       (8 (CallOp (func __func4318d0__) (args ((Ref 1) (Ref 4) (Ref 7)))))
       (9 (ReturnedOp (var eax) (typ Int))) (10 (ReturnedOp (var esp) (typ Int)))
       (11 (ReturnedOp (var edx) (typ Int))) (12 (Const __i32 0))
       (13 (DupVar (var edx) (src (Ref 12)) (typ Int)))
       (14 (OutsideContext (var ebp) (typ Int)))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 14)) (offset -248)))
       (16
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 9))
         (rhs (Ref 15))))
       (17
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 9))
         (rhs (Ref 15))))
       (18 (StoreOp (op Store32) (addr (Ref 14)) (value (Ref 17)) (offset -608)))))
     (terminator (Goto (Block 343)))
     (roots
      ((Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10) (Ref 11)
       (Ref 14) (Ref 16) (Ref 17) (Ref 18)))) |}]

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
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (11 (DupVar (var edx) (src (Ref 10)) (typ Int)))
       (12
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 11)) (signed false)
         (offset 6)))
       (13 (UniOp (var __i32) (op SignExtend16) (operand (Ref 12))))
       (14 (OutsideContext (var eax) (typ Int)))
       (15 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15))))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (20 (DupVar (var ecx) (src (Ref 19)) (typ Int))) (21 (Const __i32 8))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 20)) (rhs (Ref 21))))
       (23 (DupVar (var ecx) (src (Ref 22)) (typ Int))) (24 (Const __i32 4))
       (25 (BiOp (var esp) (op Subtract) (lhs (Ref 17)) (rhs (Ref 24))))
       (26 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 23))))
       (27 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (28 (DupVar (var ecx) (src (Ref 27)) (typ Int))) (29 (Const __i32 4))
       (30 (BiOp (var esp) (op Subtract) (lhs (Ref 25)) (rhs (Ref 29))))
       (31 (Const __i32 4532585))
       (32 (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 31))))
       (33 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))
       (34 (CallOp (func __func450c10__) (args ((Ref 28) (Ref 30) (Ref 11)))))
       (35 (ReturnedOp (var eax) (typ Int)))
       (36 (ReturnedOp (var esp) (typ Int)))
       (37 (ReturnedOp (var edx) (typ Int)))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -508)))
       (39 (DupVar (var edx) (src (Ref 38)) (typ Int)))
       (40 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 39))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 4) (Ref 6) (Ref 9) (Ref 14) (Ref 18) (Ref 26) (Ref 28)
       (Ref 32) (Ref 33) (Ref 34) (Ref 35) (Ref 36) (Ref 37) (Ref 39) (Ref 40))))
    |}]

let%expect_test "cdq/idiv" =
  test_trans_block 0x004529e3;
  [%expect
    {|
    ((id 286)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -512)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int))) (3 (Const __i32 31))
       (4
        (SignedBiOp (var edx) (op ShiftRight) (signed true) (lhs (Ref 2))
         (rhs (Ref 3))))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -516)))
       (6
        (SignedBiOp (var eax) (op Divide) (signed true) (lhs (Ref 2))
         (rhs (Ref 5))))
       (7
        (SignedBiOp (var edx) (op Remainder) (signed true) (lhs (Ref 2))
         (rhs (Ref 5))))
       (8 (DupVar (var esi) (src (Ref 7)) (typ Int))) (9 (Const __i32 0))
       (10 (OutsideContext (var esp) (typ Int))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 9))))
       (14 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (15 (DupVar (var eax) (src (Ref 14)) (typ Int)))
       (16
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 15)) (signed false)
         (offset 6)))
       (17 (UniOp (var __i32) (op SignExtend16) (operand (Ref 16))))
       (18 (OutsideContext (var ecx) (typ Int)))
       (19 (BiOp (var ecx) (op MergeTrunc16) (lhs (Ref 17)) (rhs (Ref 18))))
       (20 (Const __i32 4))
       (21 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 19))))
       (23 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (24 (DupVar (var edx) (src (Ref 23)) (typ Int))) (25 (Const __i32 8))
       (26 (BiOp (var __i32) (op Add) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (DupVar (var edx) (src (Ref 26)) (typ Int))) (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 28))))
       (30 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 27))))
       (31 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (32 (DupVar (var ecx) (src (Ref 31)) (typ Int))) (33 (Const __i32 4))
       (34 (BiOp (var esp) (op Subtract) (lhs (Ref 29)) (rhs (Ref 33))))
       (35 (Const __i32 4532736))
       (36 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 35))))
       (37 (CallOp (func __func450ca0__) (args ((Ref 32) (Ref 34) (Ref 27)))))
       (38 (ReturnedOp (var eax) (typ Int)))
       (39 (ReturnedOp (var esp) (typ Int)))
       (40 (ReturnedOp (var edx) (typ Int)))
       (41 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 8))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 8) (Ref 10) (Ref 13) (Ref 18) (Ref 22) (Ref 30) (Ref 32)
       (Ref 36) (Ref 37) (Ref 38) (Ref 39) (Ref 40) (Ref 41)))) |}]

let%expect_test "imul" =
  test_trans_block 0x004539dd;
  [%expect
    {|
    ((id 491)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -12)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int))) (3 (Const __i32 12))
       (4 (BiOp (var __i32) (op Multiply) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (DupVar (var eax) (src (Ref 4)) (typ Int)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (7 (DupVar (var ecx) (src (Ref 6)) (typ Int))) (8 (Const __i32 0))
       (9 (DupVar (var edx) (src (Ref 8)) (typ Int)))
       (10 (BiOp (var __i32) (op Add) (lhs (Ref 5)) (rhs (Ref 7))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10)) (offset 140)))
       (12 (Const __i32 0))
       (13
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 11))
         (rhs (Ref 12))))
       (14 (BiOp (var edx) (op MergeTruncLow8) (lhs (Ref 13)) (rhs (Ref 9))))
       (15 (UniOp (var __i32) (op EqualsZero) (operand (Ref 14))))))
     (terminator
      (Branch (succeed (Block 513)) (fail (Block 492)) (condition (Ref 15))))
     (roots ((Ref 0) (Ref 5) (Ref 7) (Ref 14)))) |}]

let%expect_test "float jp 0x5" =
  test_trans_block 0x00451bfe;
  [%expect
    {|
    ((id 148)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 2)) (offset 40)))
       (4 (Const __i32 4819532))
       (5 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 4))))
       (6 (Landmine (var eax) (typ Int))) (7 (Const __i32 5))
       (8 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 3)) (rhs (Ref 5))))
       (9 (UniOp (var __i32) (op EqualsZero) (operand (Ref 8))))
       (10 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 150)) (fail (Block 149)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 2) (Ref 6) (Ref 10))))
    |}]

let%expect_test "jle" =
  test_trans_block 0x004536f0;
  [%expect
    {|
    ((id 450)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -728)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -732)))
       (4
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 2))
         (rhs (Ref 3))))))
     (terminator
      (Branch (succeed (Block 452)) (fail (Block 451)) (condition (Ref 4))))
     (roots ((Ref 0) (Ref 2)))) |}]

let%expect_test "jumptable" =
  test_trans_block 0x00450dec;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -312)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))))
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
       (default (Block 528)) (switch_on (Ref 2))))
     (roots ((Ref 0) (Ref 2)))) |}]

let%expect_test "ja" =
  test_trans_block 0x00450de0;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed false)))
       (4 (UniOp (var __i32) (op SignExtend16) (operand (Ref 3))))
       (5 (DupVar (var ecx) (src (Ref 4)) (typ Int)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -312)))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -312)))
       (8 (DupVar (var edx) (src (Ref 7)) (typ Int))) (9 (Const __i32 1))
       (10 (BiOp (var __i32) (op Add) (lhs (Ref 8)) (rhs (Ref 9))))
       (11 (DupVar (var edx) (src (Ref 10)) (typ Int)))
       (12 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 11)) (offset -312)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -312)))
       (14 (Const __i32 82))
       (15
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 13))
         (rhs (Ref 14))))))
     (terminator
      (Branch (succeed (Block 481)) (fail (Block 6)) (condition (Ref 15))))
     (roots ((Ref 0) (Ref 2) (Ref 5) (Ref 6) (Ref 11) (Ref 12)))) |}]

let%expect_test "jg" =
  test_trans_block 4525496;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 480)))
       (4 (DupVar (var ecx) (src (Ref 3)) (typ Int)))
       (5 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 4)) (offset -4)))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (7 (DupVar (var edx) (src (Ref 6)) (typ Int)))
       (8 (LoadOp (var __i32) (op Load32) (addr (Ref 7)) (offset 56)))
       (9 (DupVar (var eax) (src (Ref 8)) (typ Int)))
       (10 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 9)) (offset -56)))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (12 (DupVar (var ecx) (src (Ref 11)) (typ Int)))
       (13
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 12)) (signed false)
         (offset 4)))
       (14 (UniOp (var __i32) (op SignExtend16) (operand (Ref 13))))
       (15 (DupVar (var edx) (src (Ref 14)) (typ Int)))
       (16 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -56)))
       (17
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 15))
         (rhs (Ref 16))))))
     (terminator
      (Branch (succeed (Block 482)) (fail (Block 5)) (condition (Ref 17))))
     (roots ((Ref 0) (Ref 5) (Ref 9) (Ref 10) (Ref 12) (Ref 15)))) |}]

let%expect_test "jge" =
  test_trans_block 4380548;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -16)))
       (4
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 2))
         (rhs (Ref 3))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 3)) (condition (Ref 4))))
     (roots ((Ref 0) (Ref 2)))) |}]

let%expect_test _ =
  test_trans_block 0x0040127a;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (OutsideContext (var ebp) (typ Int)))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (2 (DupVar (var eax) (src (Ref 1)) (typ Int))) (3 (Const __i32 1))
       (4 (BiOp (var __i32) (op Subtract) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (DupVar (var eax) (src (Ref 4)) (typ Int)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -24)))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (8 (Const __i32 0))
       (9
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 7))
         (rhs (Ref 8))))))
     (terminator
      (Branch (succeed (Block 6)) (fail (Block 5)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 5) (Ref 6)))) |}]

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
          (19 (ReturnedOp (var edx) (typ Int))) (20 (Const __i32 148))
          (21 (DupVar (var edi) (src (Ref 20)) (typ Int)))
          (22 (DupVar (var eax) (src (Ref 21)) (typ Int))) (23 (Const __i32 4))
          (24 (BiOp (var esp) (op Subtract) (lhs (Ref 18)) (rhs (Ref 23))))
          (25 (Const __i32 4713104))
          (26 (StoreOp (op Store32) (addr (Ref 24)) (value (Ref 25))))
          (27 (CallOp (func __func47f3a0__) (args ((Ref 14) (Ref 24) (Ref 19)))))
          (28 (ReturnedOp (var eax) (typ Int)))
          (29 (ReturnedOp (var esp) (typ Int)))
          (30 (ReturnedOp (var edx) (typ Int)))
          (31 (OutsideContext (var ebp) (typ Int)))
          (32
           (StoreOp (op Store32) (addr (Ref 31)) (value (Ref 29)) (offset -24)))
          (33 (DupVar (var esi) (src (Ref 29)) (typ Int)))
          (34 (StoreOp (op Store32) (addr (Ref 33)) (value (Ref 21))))
          (35 (Const __i32 4))
          (36 (BiOp (var esp) (op Subtract) (lhs (Ref 29)) (rhs (Ref 35))))
          (37 (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 33))))
          (38 (Const __i32 4))
          (39 (BiOp (var esp) (op Subtract) (lhs (Ref 36)) (rhs (Ref 38))))
          (40 (Const __i32 4713117))
          (41 (StoreOp (op Store32) (addr (Ref 39)) (value (Ref 40))))
          (42 (CallOp (func KERNEL32.dll_GetVersionExA) (args ((Ref 39)))))
          (43 (ReturnedOp (var eax) (typ Int)))
          (44 (ReturnedOp (var esp) (typ Int)))
          (45 (ReturnedOp (var edx) (typ Int)))
          (46 (LoadOp (var __i32) (op Load32) (addr (Ref 33)) (offset 16)))
          (47 (DupVar (var ecx) (src (Ref 46)) (typ Int)))
          (48 (Const __i32 4847496))
          (49 (StoreOp (op Store32) (addr (Ref 48)) (value (Ref 47))))
          (50 (LoadOp (var __i32) (op Load32) (addr (Ref 33)) (offset 4)))
          (51 (DupVar (var eax) (src (Ref 50)) (typ Int)))
          (52 (Const __i32 4847508))
          (53 (StoreOp (op Store32) (addr (Ref 52)) (value (Ref 51))))
          (54 (LoadOp (var __i32) (op Load32) (addr (Ref 33)) (offset 8)))
          (55 (DupVar (var edx) (src (Ref 54)) (typ Int)))
          (56 (Const __i32 4847512))
          (57 (StoreOp (op Store32) (addr (Ref 56)) (value (Ref 55))))
          (58 (LoadOp (var __i32) (op Load32) (addr (Ref 33)) (offset 12)))
          (59 (DupVar (var esi) (src (Ref 58)) (typ Int)))
          (60 (Const __i32 32767))
          (61 (BiOp (var __i32) (op And) (lhs (Ref 59)) (rhs (Ref 60))))
          (62 (DupVar (var esi) (src (Ref 61)) (typ Int)))
          (63 (Const __i32 4847500))
          (64 (StoreOp (op Store32) (addr (Ref 63)) (value (Ref 62))))
          (65 (Const __i32 2))
          (66 (BiOp (var __i32) (op Equal) (lhs (Ref 47)) (rhs (Ref 65))))))
        (terminator
         (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 66))))
        (roots
         ((Ref 0) (Ref 1) (Ref 5) (Ref 9) (Ref 13) (Ref 14) (Ref 15) (Ref 16)
          (Ref 17) (Ref 18) (Ref 19) (Ref 21) (Ref 26) (Ref 27) (Ref 28)
          (Ref 29) (Ref 30) (Ref 31) (Ref 32) (Ref 34) (Ref 37) (Ref 41)
          (Ref 42) (Ref 43) (Ref 44) (Ref 45) (Ref 47) (Ref 49) (Ref 51)
          (Ref 53) (Ref 55) (Ref 57) (Ref 62) (Ref 64))))
       ((id 1)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int))) (1 (Const __i32 32768))
          (2 (BiOp (var __i32) (op Or) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (DupVar (var esi) (src (Ref 2)) (typ Int)))
          (4 (Const __i32 4847500))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 3))))))
        (terminator (Goto (Block 2))) (roots ((Ref 0) (Ref 3) (Ref 5))))
       ((id 2)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 8))
          (2 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (DupVar (var eax) (src (Ref 2)) (typ Int)))
          (4 (OutsideContext (var edx) (typ Int)))
          (5 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (DupVar (var eax) (src (Ref 5)) (typ Int)))
          (7 (Const __i32 4847504))
          (8 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 6))))
          (9 (Const __i32 0)) (10 (DupVar (var esi) (src (Ref 9)) (typ Int)))
          (11 (OutsideContext (var esp) (typ Int))) (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 12))))
          (14 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 10))))
          (15 (Const __i32 4772232))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 15))))
          (17 (DupVar (var edi) (src (Ref 16)) (typ Int))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (Const __i32 4713200))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22 (OutsideContext (var ecx) (typ Int)))
          (23
           (CallIndirectOp (table_index (Ref 17))
            (args ((Ref 22) (Ref 19) (Ref 4)))))
          (24 (ReturnedOp (var eax) (typ Int)))
          (25 (ReturnedOp (var esp) (typ Int)))
          (26 (ReturnedOp (var edx) (typ Int)))
          (27
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 24)) (signed false)))
          (28 (Const __i32 23117))
          (29 (UniOp (var __i32) (op SignExtend16) (operand (Ref 27))))
          (30 (UniOp (var __i32) (op SignExtend16) (operand (Ref 28))))
          (31 (BiOp (var __i32) (op NotEqual) (lhs (Ref 29)) (rhs (Ref 30))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 3)) (condition (Ref 31))))
        (roots
         ((Ref 0) (Ref 4) (Ref 8) (Ref 10) (Ref 11) (Ref 14) (Ref 17) (Ref 21)
          (Ref 22) (Ref 23) (Ref 24) (Ref 25) (Ref 26))))
       ((id 3)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 60)))
          (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
          (3 (BiOp (var __i32) (op Add) (lhs (Ref 2)) (rhs (Ref 0))))
          (4 (DupVar (var ecx) (src (Ref 3)) (typ Int)))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 4))))
          (6 (Const __i32 17744))
          (7 (BiOp (var __i32) (op NotEqual) (lhs (Ref 5)) (rhs (Ref 6))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 4)) (condition (Ref 7))))
        (roots ((Ref 0) (Ref 4))))
       ((id 4)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset 24)))
          (2 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 1))))
          (3 (DupVar (var eax) (src (Ref 2)) (typ Int))) (4 (Const __i32 267))
          (5 (BiOp (var __i32) (op Equal) (lhs (Ref 3)) (rhs (Ref 4))))))
        (terminator
         (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 5))))
        (roots ((Ref 0) (Ref 3))))
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
         ((0 (Const __i32 0)) (1 (DupVar (var eax) (src (Ref 0)) (typ Int)))
          (2 (OutsideContext (var ecx) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 248)))
          (4 (OutsideContext (var esi) (typ Int)))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (DupVar (var __input_compare_arg__) (src (Ref 5)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 1) (Ref 2) (Ref 4) (Ref 6))))
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
         ((0 (Const __i32 0)) (1 (DupVar (var eax) (src (Ref 0)) (typ Int)))
          (2 (OutsideContext (var ecx) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 232)))
          (4 (OutsideContext (var esi) (typ Int)))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (DupVar (var __input_compare_arg__) (src (Ref 5)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 1) (Ref 2) (Ref 4) (Ref 6))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator
         (Branch (succeed (Block 14)) (fail (Block 13)) (condition (Ref 12))))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 14)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 16)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 18)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 20)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 22)))
        (roots
         ((Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 17) (Ref 18))))
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
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 16))))
          (18 (DupVar (var ecx) (src (Ref 15)) (typ Int)))))
        (terminator (Goto (Block 24)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 14) (Ref 17) (Ref 18))))
       ((id 24)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int)))
          (1 (OutsideContext (var ebp) (typ Int)))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -56)))
          (3 (Const __i32 -100))
          (4 (BiOp (var __i32) (op Add) (lhs (Ref 1)) (rhs (Ref 3))))
          (5 (DupVar (var eax) (src (Ref 4)) (typ Int)))
          (6 (OutsideContext (var esp) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 5))))
          (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
          (12 (Const __i32 4713427))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (CallOp (func KERNEL32.dll_GetStartupInfoA) (args ((Ref 11)))))
          (15 (ReturnedOp (var eax) (typ Int)))
          (16 (ReturnedOp (var esp) (typ Int)))
          (17 (ReturnedOp (var edx) (typ Int))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 16)) (rhs (Ref 18))))
          (20 (Const __i32 4713433))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22 (OutsideContext (var ecx) (typ Int)))
          (23 (CallOp (func __func482f02__) (args ((Ref 22) (Ref 19) (Ref 17)))))
          (24 (ReturnedOp (var eax) (typ Int)))
          (25 (ReturnedOp (var esp) (typ Int)))
          (26 (ReturnedOp (var edx) (typ Int)))
          (27
           (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 24)) (offset -104)))
          (28
           (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
            (offset -56)))
          (29 (Const __i32 1))
          (30 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 28))))
          (31 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 29))))
          (32 (BiOp (var __i32) (op And) (lhs (Ref 30)) (rhs (Ref 31))))
          (33 (UniOp (var __i32) (op EqualsZero) (operand (Ref 32))))))
        (terminator
         (Branch (succeed (Block 26)) (fail (Block 25)) (condition (Ref 33))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 6) (Ref 9) (Ref 13) (Ref 14) (Ref 15)
          (Ref 16) (Ref 17) (Ref 21) (Ref 22) (Ref 23) (Ref 24) (Ref 25)
          (Ref 26) (Ref 27))))
       ((id 25)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset -52)))
          (2 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 1))))
          (3 (DupVar (var eax) (src (Ref 2)) (typ Int)))))
        (terminator (Goto (Block 27))) (roots ((Ref 0) (Ref 3))))
       ((id 26)
        (instrs
         ((0 (Const __i32 10)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 3)))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Add) (lhs (Ref 3)) (rhs (Ref 6))))
          (8 (DupVar (var eax) (src (Ref 5)) (typ Int)))))
        (terminator (Goto (Block 27))) (roots ((Ref 1) (Ref 4) (Ref 7) (Ref 8))))
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
          (8 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 7))))
          (9 (DupVar (var esp) (src (Ref 8)) (typ Int))) (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 10))))
          (12 (Const __i32 4713545))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (OutsideContext (var ecx) (typ Int)))
          (15 (OutsideContext (var edx) (typ Int)))
          (16 (CallOp (func __func48065f__) (args ((Ref 14) (Ref 11) (Ref 15)))))
          (17 (ReturnedOp (var eax) (typ Int)))
          (18 (ReturnedOp (var esp) (typ Int)))
          (19 (ReturnedOp (var edx) (typ Int)))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 18))))
          (21 (OutsideContext (var __ret_addr__) (typ Int)))
          (22 (BiOp (var __i32) (op Equal) (lhs (Ref 20)) (rhs (Ref 21))))
          (23 (AssertOp (condition (Ref 22))))))
        (terminator Return)
        (roots
         ((Ref 0) (Ref 4) (Ref 5) (Ref 13) (Ref 14) (Ref 15) (Ref 16) (Ref 17)
          (Ref 18) (Ref 19) (Ref 21) (Ref 23))))))
     (locals
      ((__input_compare_arg__ ((name __input_compare_arg__) (typ Int)))
       (__ret_addr__ ((name __ret_addr__) (typ Int)))
       (eax ((name eax) (typ Int))) (ebp ((name ebp) (typ Int)))
       (ecx ((name ecx) (typ Int))) (edi ((name edi) (typ Int)))
       (edx ((name edx) (typ Int))) (esi ((name esi) (typ Int)))
       (esp ((name esp) (typ Int)))))) |}]

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
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset -4)))
          (13 (DupVar (var eax) (src (Ref 12)) (typ Int)))
          (14 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 13)) (offset 376)))
          (15 (Const __i32 4819544))
          (16 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 15))))
          (17 (Landmine (var eax) (typ Int))) (18 (Const __i32 65))
          (19
           (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 14))
            (rhs (Ref 16))))
          (20 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))
          (21 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 20))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 5) (Ref 6) (Ref 7) (Ref 9) (Ref 10)
          (Ref 11) (Ref 17) (Ref 21))))
       ((id 1)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
          (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
          (4 (DupVar (var ecx) (src (Ref 3)) (typ Int)))
          (5 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 2))))
          (6 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 4)) (offset 376)))
          (7 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 5)) (rhs (Ref 6))))
          (8 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
          (9 (DupVar (var eax) (src (Ref 8)) (typ Int)))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
          (12 (DupVar (var eax) (src (Ref 11)) (typ Int)))
          (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12))))
          (14 (Const __i32 4819540))
          (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 14))))
          (16 (Landmine (var eax) (typ Int))) (17 (Const __i32 1))
          (18
           (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 13))
            (rhs (Ref 15))))
          (19 (UniOp (var __i32) (op EqualsZero) (operand (Ref 18))))
          (20 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 19))))
        (roots ((Ref 0) (Ref 4) (Ref 10) (Ref 16) (Ref 20))))
       ((id 2)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
          (4 (DupVar (var eax) (src (Ref 3)) (typ Int))) (5 (Const __i32 1))
          (6 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
          (7 (DupVar (var eax) (src (Ref 6)) (typ Int)))
          (8 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
          (9 (DupVar (var ecx) (src (Ref 8)) (typ Int)))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
          (12 (DupVar (var eax) (src (Ref 11)) (typ Int)))
          (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12))))
          (14 (Const __i32 4819540))
          (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 14))))
          (16 (BiOp (var __fl) (op FloatSub) (lhs (Ref 13)) (rhs (Ref 15))))
          (17 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 12)))
          (18 (DupVar (var eax) (src (Ref 17)) (typ Int)))
          (19 (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 16))))
          (20 (GetGlobalOp (var __i32) (global ((name __fpuStack__) (typ Int)))))))
        (terminator (Goto (Block 3)))
        (roots ((Ref 0) (Ref 9) (Ref 10) (Ref 18) (Ref 19) (Ref 20))))
       ((id 3) (instrs ()) (terminator (Goto (Block 5))) (roots ()))
       ((id 4)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (DupVar (var eax) (src (Ref 1)) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
          (4 (DupVar (var eax) (src (Ref 3)) (typ Int))) (5 (Const __i32 1))
          (6 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
          (7 (DupVar (var eax) (src (Ref 6)) (typ Int)))
          (8 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
          (9 (DupVar (var ecx) (src (Ref 8)) (typ Int)))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))))
        (terminator (Goto (Block 5))) (roots ((Ref 0) (Ref 7) (Ref 9) (Ref 10))))
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
