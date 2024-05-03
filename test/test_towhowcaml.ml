open! Core
open Towhowcaml
module C = Radatnet.Commands

let c = Radatnet.Core.create ()

let () =
  C.open_file c "/home/errast/code/Touhou7/th07.exe";
  C.analyze_all c LevelThree

let intrinsics = make_intrinsics c
let translate = translate_func c ~intrinsics
let test_trans addr = print_s @@ Mir.Func.sexp_of_t @@ translate addr

let test_trans_block addr =
  C.seek c addr;
  let name = func_name c in
  let blocks = C.get_func_blocks c |> Array.map ~f:(make_block c) in
  let index =
    Option.value_exn
    @@ Array.binary_search
         ~compare:(fun b addr -> compare_int b.offset addr)
         blocks `Last_less_than_or_equal_to addr
  in
  print_s @@ Mir.Block.sexp_of_t
  @@ (Func_translator.translate ~blocks ~name ~intrinsics).blocks.(index)

let%expect_test "fild/fiadd" =
  test_trans_block 0x00453df6;
  [%expect {|
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
       (32
        (SetGlobalOp (global_name __stack__) (value (Ref 29)) (global_type Int)))
       (33
        (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
       (34
        (StoreOp (op FloatStore64) (addr (Ref 33)) (value (Ref 26)) (offset 8)))
       (35 (Const __i32 1))
       (36 (BiOp (var __i32) (op Add) (lhs (Ref 33)) (rhs (Ref 35))))
       (37
        (SetGlobalOp (global_name __fpuStack__) (value (Ref 36))
         (global_type Int)))
       (38
        (CallOp (var eax) (func __func48b8a0__) (args ((Ref 22) (Ref 10)))
         (return_type Int)))
       (39 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
       (40 (Const __i32 4537877))
       (41 (LoadOp (var __i32) (op Load32) (addr (Ref 39))))
       (42 (BiOp (var __i32) (op Equal) (lhs (Ref 41)) (rhs (Ref 40))))
       (43 (AssertOp (condition (Ref 42)))) (44 (Const __i32 4))
       (45 (BiOp (var esp) (op Add) (lhs (Ref 39)) (rhs (Ref 44))))
       (46 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (47 (DupVar (var edx) (src (Ref 46)) (typ Int)))
       (48 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 38))))
       (49 (StoreOp (op Store8) (addr (Ref 47)) (value (Ref 48)) (offset 443)))))
     (terminator (Goto (Block 513)))
     (roots
      ((Ref 0) (Ref 13) (Ref 22) (Ref 23) (Ref 28) (Ref 31) (Ref 32) (Ref 33)
       (Ref 34) (Ref 37) (Ref 38) (Ref 39) (Ref 43) (Ref 45) (Ref 47) (Ref 49)))) |}]

let%expect_test "div" =
  test_trans_block 0x00452f62;
  [%expect {|
    ((id 341)
     (instrs
      ((0 (Const __i32 4849184)) (1 (DupVar (var ecx) (src (Ref 0)) (typ Int)))
       (2 (OutsideContext (var edx) (typ Int))) (3 (Const __i32 4))
       (4 (OutsideContext (var esp) (typ Int)))
       (5 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 3))))
       (6 (Const __i32 4534107))
       (7 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 6))))
       (8
        (SetGlobalOp (global_name __stack__) (value (Ref 5)) (global_type Int)))
       (9
        (CallOp (var eax) (func __func4318d0__) (args ((Ref 1) (Ref 2)))
         (return_type Int)))
       (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
       (11 (Const __i32 4534107))
       (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
       (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
       (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
       (17 (Const __i32 0)) (18 (DupVar (var edx) (src (Ref 17)) (typ Int)))
       (19 (OutsideContext (var ebp) (typ Int)))
       (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19)) (offset -248)))
       (21
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 9))
         (rhs (Ref 20))))
       (22
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 9))
         (rhs (Ref 20))))
       (23 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 22)) (offset -608)))))
     (terminator (Goto (Block 343)))
     (roots
      ((Ref 1) (Ref 2) (Ref 4) (Ref 7) (Ref 8) (Ref 9) (Ref 10) (Ref 14)
       (Ref 16) (Ref 19) (Ref 21) (Ref 22) (Ref 23)))) |}]

let%expect_test "fdiv" =
  test_trans_block 0x00452949;
  [%expect {|
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
       (33
        (SetGlobalOp (global_name __stack__) (value (Ref 30)) (global_type Int)))
       (34
        (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
       (35
        (CallOp (var eax) (func __func450c10__) (args ((Ref 28) (Ref 11)))
         (return_type Int)))
       (36 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
       (37 (Const __i32 4532585))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 36))))
       (39 (BiOp (var __i32) (op Equal) (lhs (Ref 38)) (rhs (Ref 37))))
       (40 (AssertOp (condition (Ref 39)))) (41 (Const __i32 4))
       (42 (BiOp (var esp) (op Add) (lhs (Ref 36)) (rhs (Ref 41))))
       (43 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -508)))
       (44 (DupVar (var edx) (src (Ref 43)) (typ Int)))
       (45 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 44))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 4) (Ref 6) (Ref 9) (Ref 14) (Ref 18) (Ref 26) (Ref 28)
       (Ref 32) (Ref 33) (Ref 34) (Ref 35) (Ref 36) (Ref 40) (Ref 42) (Ref 44)
       (Ref 45)))) |}]

let%expect_test "cdq/idiv" =
  test_trans_block 0x004529e3;
  [%expect {|
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
       (37
        (SetGlobalOp (global_name __stack__) (value (Ref 34)) (global_type Int)))
       (38
        (CallOp (var eax) (func __func450ca0__) (args ((Ref 32) (Ref 27)))
         (return_type Int)))
       (39 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
       (40 (Const __i32 4532736))
       (41 (LoadOp (var __i32) (op Load32) (addr (Ref 39))))
       (42 (BiOp (var __i32) (op Equal) (lhs (Ref 41)) (rhs (Ref 40))))
       (43 (AssertOp (condition (Ref 42)))) (44 (Const __i32 4))
       (45 (BiOp (var esp) (op Add) (lhs (Ref 39)) (rhs (Ref 44))))
       (46 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 8))))))
     (terminator (Goto (Block 481)))
     (roots
      ((Ref 0) (Ref 8) (Ref 10) (Ref 13) (Ref 18) (Ref 22) (Ref 27) (Ref 30)
       (Ref 32) (Ref 36) (Ref 37) (Ref 38) (Ref 39) (Ref 43) (Ref 45) (Ref 46)))) |}]

let%expect_test "imul" =
  test_trans_block 0x004539dd;
  [%expect {|
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
  [%expect {|
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
       (10
        (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
     (terminator
      (Branch (succeed (Block 150)) (fail (Block 149)) (condition (Ref 9))))
     (roots ((Ref 0) (Ref 2) (Ref 6) (Ref 10)))) |}]

let%expect_test "jle" =
  test_trans_block 0x004536f0;
  [%expect {|
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
  [%expect {|
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
  [%expect {|
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
  [%expect {|
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
      ((args (((name ecx) (typ Int)) ((name edx) (typ Int))))
       (return ((name eax) (typ Int)))))
     (blocks
      (((id 0)
        (instrs
         ((0 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (1 (Const __i32 96)) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 1))))
          (5 (Const __i32 4785824)) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 6))))
          (8 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 5))))
          (9 (OutsideContext (var ecx) (typ Int)))
          (10 (OutsideContext (var edx) (typ Int))) (11 (Const __i32 4))
          (12 (BiOp (var esp) (op Subtract) (lhs (Ref 7)) (rhs (Ref 11))))
          (13 (Const __i32 4713092))
          (14 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 13))))
          (15
           (SetGlobalOp (global_name __stack__) (value (Ref 12))
            (global_type Int)))
          (16
           (CallOp (var eax) (func __func480624__) (args ((Ref 9) (Ref 10)))
            (return_type Int)))
          (17 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (18 (Const __i32 4713092))
          (19 (LoadOp (var __i32) (op Load32) (addr (Ref 17))))
          (20 (BiOp (var __i32) (op Equal) (lhs (Ref 19)) (rhs (Ref 18))))
          (21 (AssertOp (condition (Ref 20)))) (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 17)) (rhs (Ref 22))))
          (24 (Const __i32 148)) (25 (DupVar (var edi) (src (Ref 24)) (typ Int)))
          (26 (DupVar (var eax) (src (Ref 25)) (typ Int))) (27 (Const __i32 4))
          (28 (BiOp (var esp) (op Subtract) (lhs (Ref 23)) (rhs (Ref 27))))
          (29 (Const __i32 4713104))
          (30 (StoreOp (op Store32) (addr (Ref 28)) (value (Ref 29))))
          (31
           (SetGlobalOp (global_name __stack__) (value (Ref 28))
            (global_type Int)))
          (32
           (CallOp (var eax) (func __func47f3a0__) (args ((Ref 9) (Ref 10)))
            (return_type Int)))
          (33 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (34 (Const __i32 4713104))
          (35 (LoadOp (var __i32) (op Load32) (addr (Ref 33))))
          (36 (BiOp (var __i32) (op Equal) (lhs (Ref 35)) (rhs (Ref 34))))
          (37 (AssertOp (condition (Ref 36)))) (38 (Const __i32 4))
          (39 (BiOp (var esp) (op Add) (lhs (Ref 33)) (rhs (Ref 38))))
          (40 (OutsideContext (var ebp) (typ Int)))
          (41
           (StoreOp (op Store32) (addr (Ref 40)) (value (Ref 39)) (offset -24)))
          (42 (DupVar (var esi) (src (Ref 39)) (typ Int)))
          (43 (StoreOp (op Store32) (addr (Ref 42)) (value (Ref 25))))
          (44 (Const __i32 4))
          (45 (BiOp (var esp) (op Subtract) (lhs (Ref 39)) (rhs (Ref 44))))
          (46 (StoreOp (op Store32) (addr (Ref 45)) (value (Ref 42))))
          (47 (Const __i32 4))
          (48 (BiOp (var esp) (op Subtract) (lhs (Ref 45)) (rhs (Ref 47))))
          (49 (Const __i32 4713117))
          (50 (StoreOp (op Store32) (addr (Ref 48)) (value (Ref 49))))
          (51
           (SetGlobalOp (global_name __stack__) (value (Ref 48))
            (global_type Int)))
          (52
           (CallOp (var eax) (func KERNEL32.dll_GetVersionExA) (args ())
            (return_type Int)))
          (53 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (54 (Const __i32 4713117))
          (55 (LoadOp (var __i32) (op Load32) (addr (Ref 53))))
          (56 (BiOp (var __i32) (op Equal) (lhs (Ref 55)) (rhs (Ref 54))))
          (57 (AssertOp (condition (Ref 56)))) (58 (Const __i32 4))
          (59 (BiOp (var esp) (op Add) (lhs (Ref 53)) (rhs (Ref 58))))
          (60 (LoadOp (var __i32) (op Load32) (addr (Ref 42)) (offset 16)))
          (61 (DupVar (var ecx) (src (Ref 60)) (typ Int)))
          (62 (Const __i32 4847496))
          (63 (StoreOp (op Store32) (addr (Ref 62)) (value (Ref 61))))
          (64 (LoadOp (var __i32) (op Load32) (addr (Ref 42)) (offset 4)))
          (65 (DupVar (var eax) (src (Ref 64)) (typ Int)))
          (66 (Const __i32 4847508))
          (67 (StoreOp (op Store32) (addr (Ref 66)) (value (Ref 65))))
          (68 (LoadOp (var __i32) (op Load32) (addr (Ref 42)) (offset 8)))
          (69 (DupVar (var edx) (src (Ref 68)) (typ Int)))
          (70 (Const __i32 4847512))
          (71 (StoreOp (op Store32) (addr (Ref 70)) (value (Ref 69))))
          (72 (LoadOp (var __i32) (op Load32) (addr (Ref 42)) (offset 12)))
          (73 (DupVar (var esi) (src (Ref 72)) (typ Int)))
          (74 (Const __i32 32767))
          (75 (BiOp (var __i32) (op And) (lhs (Ref 73)) (rhs (Ref 74))))
          (76 (DupVar (var esi) (src (Ref 75)) (typ Int)))
          (77 (Const __i32 4847500))
          (78 (StoreOp (op Store32) (addr (Ref 77)) (value (Ref 76))))
          (79 (Const __i32 2))
          (80 (BiOp (var __i32) (op Equal) (lhs (Ref 61)) (rhs (Ref 79))))))
        (terminator
         (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 80))))
        (roots
         ((Ref 0) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 14) (Ref 15) (Ref 16)
          (Ref 17) (Ref 21) (Ref 25) (Ref 30) (Ref 31) (Ref 32) (Ref 33)
          (Ref 37) (Ref 40) (Ref 41) (Ref 43) (Ref 46) (Ref 50) (Ref 51)
          (Ref 52) (Ref 53) (Ref 57) (Ref 59) (Ref 61) (Ref 63) (Ref 65)
          (Ref 67) (Ref 69) (Ref 71) (Ref 76) (Ref 78))))
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
          (2 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 1))))
          (3 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 2))))
          (4 (DupVar (var eax) (src (Ref 3)) (typ Int)))
          (5 (OutsideContext (var edx) (typ Int)))
          (6 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
          (7 (DupVar (var eax) (src (Ref 6)) (typ Int)))
          (8 (Const __i32 4847504))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 7))))
          (10 (Const __i32 0)) (11 (DupVar (var esi) (src (Ref 10)) (typ Int)))
          (12 (OutsideContext (var esp) (typ Int))) (13 (Const __i32 4))
          (14 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 13))))
          (15 (StoreOp (op Store32) (addr (Ref 14)) (value (Ref 11))))
          (16 (Const __i32 4772232))
          (17 (LoadOp (var __i32) (op Load32) (addr (Ref 16))))
          (18 (DupVar (var edi) (src (Ref 17)) (typ Int)))
          (19 (OutsideContext (var ecx) (typ Int))) (20 (Const __i32 4))
          (21 (BiOp (var esp) (op Subtract) (lhs (Ref 14)) (rhs (Ref 20))))
          (22 (Const __i32 4713200))
          (23 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 22))))
          (24
           (SetGlobalOp (global_name __stack__) (value (Ref 21))
            (global_type Int)))
          (25
           (CallIndirectOp (var eax) (table_index (Ref 18))
            (args ((Ref 19) (Ref 5))) (return_type Int)))
          (26 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (27 (Const __i32 4713200))
          (28 (LoadOp (var __i32) (op Load32) (addr (Ref 26))))
          (29 (BiOp (var __i32) (op Equal) (lhs (Ref 28)) (rhs (Ref 27))))
          (30 (AssertOp (condition (Ref 29)))) (31 (Const __i32 4))
          (32 (BiOp (var esp) (op Add) (lhs (Ref 26)) (rhs (Ref 31))))
          (33
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 25)) (signed false)))
          (34 (Const __i32 23117))
          (35 (UniOp (var __i32) (op SignExtend16) (operand (Ref 33))))
          (36 (UniOp (var __i32) (op SignExtend16) (operand (Ref 34))))
          (37 (BiOp (var __i32) (op NotEqual) (lhs (Ref 35)) (rhs (Ref 36))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 3)) (condition (Ref 37))))
        (roots
         ((Ref 0) (Ref 5) (Ref 9) (Ref 11) (Ref 12) (Ref 15) (Ref 18) (Ref 19)
          (Ref 23) (Ref 24) (Ref 25) (Ref 26) (Ref 30) (Ref 32))))
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
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713286))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func48068a__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713286))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator
         (Branch (succeed (Block 14)) (fail (Block 13)) (condition (Ref 12))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 13)
        (instrs
         ((0 (Const __i32 28)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713298))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea59__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713298))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 14)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 14)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (OutsideContext (var edx) (typ Int))) (2 (Const __i32 4))
          (3 (OutsideContext (var esp) (typ Int)))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
          (5 (Const __i32 4713304))
          (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 4))
            (global_type Int)))
          (8
           (CallOp (var eax) (func __func481649__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (9 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (10 (Const __i32 4713304))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
          (12 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 10))))
          (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Add) (lhs (Ref 9)) (rhs (Ref 14))))))
        (terminator
         (Branch (succeed (Block 16)) (fail (Block 15)) (condition (Ref 8))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
          (Ref 15))))
       ((id 15)
        (instrs
         ((0 (Const __i32 16)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713315))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea59__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713315))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 16)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 16)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (OutsideContext (var edx) (typ Int))) (2 (Const __i32 4))
          (3 (OutsideContext (var esp) (typ Int)))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
          (5 (Const __i32 4713321))
          (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 4))
            (global_type Int)))
          (8
           (CallOp (var eax) (func __func483560__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (9 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (10 (Const __i32 4713321))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
          (12 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 10))))
          (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Add) (lhs (Ref 9)) (rhs (Ref 14))))
          (16 (OutsideContext (var esi) (typ Int)))
          (17 (OutsideContext (var ebp) (typ Int)))
          (18
           (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 16)) (offset -4)))
          (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 19))))
          (21 (Const __i32 4713329))
          (22 (StoreOp (op Store32) (addr (Ref 20)) (value (Ref 21))))
          (23
           (SetGlobalOp (global_name __stack__) (value (Ref 20))
            (global_type Int)))
          (24
           (CallOp (var eax) (func __func483362__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (25 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (26 (Const __i32 4713329))
          (27 (LoadOp (var __i32) (op Load32) (addr (Ref 25))))
          (28 (BiOp (var __i32) (op Equal) (lhs (Ref 27)) (rhs (Ref 26))))
          (29 (AssertOp (condition (Ref 28)))) (30 (Const __i32 4))
          (31 (BiOp (var esp) (op Add) (lhs (Ref 25)) (rhs (Ref 30))))
          (32 (Const __i32 0))
          (33
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 24)) (rhs (Ref 32))))))
        (terminator
         (Branch (succeed (Block 18)) (fail (Block 17)) (condition (Ref 33))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
          (Ref 16) (Ref 17) (Ref 18) (Ref 22) (Ref 23) (Ref 24) (Ref 25)
          (Ref 29) (Ref 31))))
       ((id 17)
        (instrs
         ((0 (Const __i32 27)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713340))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713340))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 18)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 18)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713346))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5
           (SetGlobalOp (global_name __stack__) (value (Ref 2))
            (global_type Int)))
          (6
           (CallOp (var eax) (func KERNEL32.dll_GetCommandLineA) (args ())
            (return_type Int)))
          (7 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (8 (Const __i32 4713346))
          (9 (LoadOp (var __i32) (op Load32) (addr (Ref 7))))
          (10 (BiOp (var __i32) (op Equal) (lhs (Ref 9)) (rhs (Ref 8))))
          (11 (AssertOp (condition (Ref 10)))) (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 7)) (rhs (Ref 12))))
          (14 (Const __i32 20337236))
          (15 (StoreOp (op Store32) (addr (Ref 14)) (value (Ref 6))))
          (16 (OutsideContext (var ecx) (typ Int)))
          (17 (OutsideContext (var edx) (typ Int))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (Const __i32 4713357))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22
           (SetGlobalOp (global_name __stack__) (value (Ref 19))
            (global_type Int)))
          (23
           (CallOp (var eax) (func __func483240__) (args ((Ref 16) (Ref 17)))
            (return_type Int)))
          (24 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (25 (Const __i32 4713357))
          (26 (LoadOp (var __i32) (op Load32) (addr (Ref 24))))
          (27 (BiOp (var __i32) (op Equal) (lhs (Ref 26)) (rhs (Ref 25))))
          (28 (AssertOp (condition (Ref 27)))) (29 (Const __i32 4))
          (30 (BiOp (var esp) (op Add) (lhs (Ref 24)) (rhs (Ref 29))))
          (31 (Const __i32 4847472))
          (32 (StoreOp (op Store32) (addr (Ref 31)) (value (Ref 23))))
          (33 (Const __i32 4))
          (34 (BiOp (var esp) (op Subtract) (lhs (Ref 30)) (rhs (Ref 33))))
          (35 (Const __i32 4713367))
          (36 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 35))))
          (37
           (SetGlobalOp (global_name __stack__) (value (Ref 34))
            (global_type Int)))
          (38
           (CallOp (var eax) (func __func48319e__) (args ((Ref 16) (Ref 17)))
            (return_type Int)))
          (39 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (40 (Const __i32 4713367))
          (41 (LoadOp (var __i32) (op Load32) (addr (Ref 39))))
          (42 (BiOp (var __i32) (op Equal) (lhs (Ref 41)) (rhs (Ref 40))))
          (43 (AssertOp (condition (Ref 42)))) (44 (Const __i32 4))
          (45 (BiOp (var esp) (op Add) (lhs (Ref 39)) (rhs (Ref 44))))
          (46 (Const __i32 0))
          (47
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 38)) (rhs (Ref 46))))))
        (terminator
         (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 47))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 11) (Ref 15) (Ref 16)
          (Ref 17) (Ref 21) (Ref 22) (Ref 23) (Ref 24) (Ref 28) (Ref 32)
          (Ref 36) (Ref 37) (Ref 38) (Ref 39) (Ref 43) (Ref 45))))
       ((id 19)
        (instrs
         ((0 (Const __i32 8)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713378))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713378))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 20)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 20)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (OutsideContext (var edx) (typ Int))) (2 (Const __i32 4))
          (3 (OutsideContext (var esp) (typ Int)))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
          (5 (Const __i32 4713384))
          (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 4))
            (global_type Int)))
          (8
           (CallOp (var eax) (func __func482f6b__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (9 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (10 (Const __i32 4713384))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
          (12 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 10))))
          (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Add) (lhs (Ref 9)) (rhs (Ref 14))))
          (16 (Const __i32 0))
          (17
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 8)) (rhs (Ref 16))))))
        (terminator
         (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 17))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
          (Ref 15))))
       ((id 21)
        (instrs
         ((0 (Const __i32 9)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713395))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713395))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 22)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 17) (Ref 22) (Ref 23))))
       ((id 22)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (OutsideContext (var edx) (typ Int))) (2 (Const __i32 4))
          (3 (OutsideContext (var esp) (typ Int)))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
          (5 (Const __i32 4713401))
          (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 4))
            (global_type Int)))
          (8
           (CallOp (var eax) (func __func47f891__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (9 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (10 (Const __i32 4713401))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
          (12 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 10))))
          (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Add) (lhs (Ref 9)) (rhs (Ref 14))))
          (16 (OutsideContext (var ebp) (typ Int)))
          (17
           (StoreOp (op Store32) (addr (Ref 16)) (value (Ref 8)) (offset -32)))
          (18 (OutsideContext (var esi) (typ Int)))
          (19 (BiOp (var __i32) (op Equal) (lhs (Ref 8)) (rhs (Ref 18))))))
        (terminator
         (Branch (succeed (Block 24)) (fail (Block 23)) (condition (Ref 19))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
          (Ref 15) (Ref 16) (Ref 17) (Ref 18))))
       ((id 23)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713414))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713414))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 21))))
          (23 (DupVar (var ecx) (src (Ref 20)) (typ Int)))))
        (terminator (Goto (Block 24)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 17) (Ref 22) (Ref 23))))
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
          (14
           (SetGlobalOp (global_name __stack__) (value (Ref 11))
            (global_type Int)))
          (15
           (CallOp (var eax) (func KERNEL32.dll_GetStartupInfoA) (args ())
            (return_type Int)))
          (16 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (17 (Const __i32 4713427))
          (18 (LoadOp (var __i32) (op Load32) (addr (Ref 16))))
          (19 (BiOp (var __i32) (op Equal) (lhs (Ref 18)) (rhs (Ref 17))))
          (20 (AssertOp (condition (Ref 19)))) (21 (Const __i32 4))
          (22 (BiOp (var esp) (op Add) (lhs (Ref 16)) (rhs (Ref 21))))
          (23 (OutsideContext (var ecx) (typ Int)))
          (24 (OutsideContext (var edx) (typ Int))) (25 (Const __i32 4))
          (26 (BiOp (var esp) (op Subtract) (lhs (Ref 22)) (rhs (Ref 25))))
          (27 (Const __i32 4713433))
          (28 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 27))))
          (29
           (SetGlobalOp (global_name __stack__) (value (Ref 26))
            (global_type Int)))
          (30
           (CallOp (var eax) (func __func482f02__) (args ((Ref 23) (Ref 24)))
            (return_type Int)))
          (31 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (32 (Const __i32 4713433))
          (33 (LoadOp (var __i32) (op Load32) (addr (Ref 31))))
          (34 (BiOp (var __i32) (op Equal) (lhs (Ref 33)) (rhs (Ref 32))))
          (35 (AssertOp (condition (Ref 34)))) (36 (Const __i32 4))
          (37 (BiOp (var esp) (op Add) (lhs (Ref 31)) (rhs (Ref 36))))
          (38
           (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 30)) (offset -104)))
          (39
           (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
            (offset -56)))
          (40 (Const __i32 1))
          (41 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 39))))
          (42 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 40))))
          (43 (BiOp (var __i32) (op And) (lhs (Ref 41)) (rhs (Ref 42))))
          (44 (UniOp (var __i32) (op EqualsZero) (operand (Ref 43))))))
        (terminator
         (Branch (succeed (Block 26)) (fail (Block 25)) (condition (Ref 44))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 6) (Ref 9) (Ref 13) (Ref 14) (Ref 15)
          (Ref 16) (Ref 20) (Ref 23) (Ref 24) (Ref 28) (Ref 29) (Ref 30)
          (Ref 31) (Ref 35) (Ref 37) (Ref 38))))
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
          (17 (OutsideContext (var edi) (typ Int)))
          (18 (OutsideContext (var ecx) (typ Int)))
          (19 (OutsideContext (var edx) (typ Int))) (20 (Const __i32 4))
          (21 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 20))))
          (22 (Const __i32 4713462))
          (23 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 22))))
          (24
           (SetGlobalOp (global_name __stack__) (value (Ref 21))
            (global_type Int)))
          (25
           (CallIndirectOp (var eax) (table_index (Ref 17))
            (args ((Ref 18) (Ref 19))) (return_type Int)))
          (26 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (27 (Const __i32 4713462))
          (28 (LoadOp (var __i32) (op Load32) (addr (Ref 26))))
          (29 (BiOp (var __i32) (op Equal) (lhs (Ref 28)) (rhs (Ref 27))))
          (30 (AssertOp (condition (Ref 29)))) (31 (Const __i32 4))
          (32 (BiOp (var esp) (op Add) (lhs (Ref 26)) (rhs (Ref 31))))
          (33 (Const __i32 4))
          (34 (BiOp (var esp) (op Subtract) (lhs (Ref 32)) (rhs (Ref 33))))
          (35 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 25))))
          (36 (Const __i32 4))
          (37 (BiOp (var esp) (op Subtract) (lhs (Ref 34)) (rhs (Ref 36))))
          (38 (Const __i32 4713465))
          (39 (StoreOp (op Store32) (addr (Ref 37)) (value (Ref 38))))
          (40
           (SetGlobalOp (global_name __stack__) (value (Ref 37))
            (global_type Int)))
          (41
           (CallOp (var eax) (func __func434020__) (args ((Ref 18) (Ref 19)))
            (return_type Int)))
          (42 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (43 (Const __i32 4713465))
          (44 (LoadOp (var __i32) (op Load32) (addr (Ref 42))))
          (45 (BiOp (var __i32) (op Equal) (lhs (Ref 44)) (rhs (Ref 43))))
          (46 (AssertOp (condition (Ref 45)))) (47 (Const __i32 4))
          (48 (BiOp (var esp) (op Add) (lhs (Ref 42)) (rhs (Ref 47))))
          (49 (DupVar (var edi) (src (Ref 41)) (typ Int)))
          (50
           (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 49)) (offset -108)))
          (51 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -28)))
          (52 (BiOp (var __i32) (op NotEqual) (lhs (Ref 51)) (rhs (Ref 10))))))
        (terminator
         (Branch (succeed (Block 29)) (fail (Block 28)) (condition (Ref 52))))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 9) (Ref 10) (Ref 13) (Ref 16)
          (Ref 17) (Ref 18) (Ref 19) (Ref 23) (Ref 24) (Ref 25) (Ref 26)
          (Ref 30) (Ref 35) (Ref 39) (Ref 40) (Ref 41) (Ref 42) (Ref 46)
          (Ref 48) (Ref 49) (Ref 50))))
       ((id 28)
        (instrs
         ((0 (OutsideContext (var edi) (typ Int)))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (OutsideContext (var ecx) (typ Int)))
          (6 (OutsideContext (var edx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (Const __i32 4713481))
          (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
          (11
           (SetGlobalOp (global_name __stack__) (value (Ref 8))
            (global_type Int)))
          (12
           (CallOp (var eax) (func __func47f9c9__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (13 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (14 (Const __i32 4713481))
          (15 (LoadOp (var __i32) (op Load32) (addr (Ref 13))))
          (16 (BiOp (var __i32) (op Equal) (lhs (Ref 15)) (rhs (Ref 14))))
          (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Add) (lhs (Ref 13)) (rhs (Ref 18))))))
        (terminator (Goto (Block 29)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 17) (Ref 19))))
       ((id 29)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (OutsideContext (var edx) (typ Int))) (2 (Const __i32 4))
          (3 (OutsideContext (var esp) (typ Int)))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 2))))
          (5 (Const __i32 4713486))
          (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 4))
            (global_type Int)))
          (8
           (CallOp (var eax) (func __func47f9eb__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (9 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (10 (Const __i32 4713486))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
          (12 (BiOp (var __i32) (op Equal) (lhs (Ref 11)) (rhs (Ref 10))))
          (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Add) (lhs (Ref 9)) (rhs (Ref 14))))))
        (terminator (Goto (Block 30)))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 13)
          (Ref 15))))
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
          (9 (DupVar (var esp) (src (Ref 8)) (typ Int)))
          (10 (OutsideContext (var ecx) (typ Int)))
          (11 (OutsideContext (var edx) (typ Int))) (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 12))))
          (14 (Const __i32 4713545))
          (15 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 14))))
          (16
           (SetGlobalOp (global_name __stack__) (value (Ref 13))
            (global_type Int)))
          (17
           (CallOp (var eax) (func __func48065f__) (args ((Ref 10) (Ref 11)))
            (return_type Int)))
          (18 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (19 (Const __i32 4713545))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 18))))
          (21 (BiOp (var __i32) (op Equal) (lhs (Ref 20)) (rhs (Ref 19))))
          (22 (AssertOp (condition (Ref 21)))) (23 (Const __i32 4))
          (24 (BiOp (var esp) (op Add) (lhs (Ref 18)) (rhs (Ref 23))))
          (25
           (SetGlobalOp (global_name __stack__) (value (Ref 24))
            (global_type Int)))))
        (terminator Return)
        (roots
         ((Ref 0) (Ref 4) (Ref 5) (Ref 10) (Ref 11) (Ref 15) (Ref 16) (Ref 17)
          (Ref 18) (Ref 22) (Ref 24) (Ref 25))))))
     (locals
      ((__input_compare_arg__ ((name __input_compare_arg__) (typ Int)))
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
      ((args (((name ecx) (typ Int)) ((name edx) (typ Int))))
       (return ((name eax) (typ Int)))))
     (blocks
      (((id 0)
        (instrs
         ((0 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (1 (OutsideContext (var ebp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 1))))
          (5 (DupVar (var ebp) (src (Ref 3)) (typ Int)))
          (6 (OutsideContext (var ecx) (typ Int))) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 6))))
          (10 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 6)) (offset -4)))
          (11 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -4)))
          (12 (DupVar (var eax) (src (Ref 11)) (typ Int)))
          (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12)) (offset 376)))
          (14 (Const __i32 4819544))
          (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 14))))
          (16 (Landmine (var eax) (typ Int))) (17 (Const __i32 65))
          (18
           (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 13))
            (rhs (Ref 15))))
          (19 (UniOp (var __i32) (op EqualsZero) (operand (Ref 18))))
          (20
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 19))))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 8) (Ref 9) (Ref 10)
          (Ref 16) (Ref 20))))
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
          (20
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
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
          (20
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
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
          (5 (Const __i32 8))
          (6 (BiOp (var esp) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
          (7
           (SetGlobalOp (global_name __stack__) (value (Ref 6))
            (global_type Int)))))
        (terminator Return) (roots ((Ref 0) (Ref 2) (Ref 6) (Ref 7))))))
     (locals
      ((eax ((name eax) (typ Int))) (ebp ((name ebp) (typ Int)))
       (ecx ((name ecx) (typ Int))) (edx ((name edx) (typ Int)))
       (esp ((name esp) (typ Int)))))) |}]
