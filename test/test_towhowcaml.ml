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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (17
           (CallOp (var eax) (func __func480624__) (args ((Ref 9) (Ref 10)))
            (return_type Int)))
          (18 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (19 (Const __i32 4713092))
          (20 (LoadOp (var __i32) (op Load32) (addr (Ref 18))))
          (21 (BiOp (var __i32) (op Equal) (lhs (Ref 20)) (rhs (Ref 19))))
          (22 (AssertOp (condition (Ref 21)))) (23 (Const __i32 4))
          (24 (BiOp (var esp) (op Add) (lhs (Ref 18)) (rhs (Ref 23))))
          (25 (Const __i32 148)) (26 (DupVar (var edi) (src (Ref 25)) (typ Int)))
          (27 (DupVar (var eax) (src (Ref 26)) (typ Int))) (28 (Const __i32 4))
          (29 (BiOp (var esp) (op Subtract) (lhs (Ref 24)) (rhs (Ref 28))))
          (30 (Const __i32 4713104))
          (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
          (32
           (SetGlobalOp (global_name __stack__) (value (Ref 29))
            (global_type Int)))
          (33
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (34
           (CallOp (var eax) (func __func47f3a0__) (args ((Ref 9) (Ref 10)))
            (return_type Int)))
          (35 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (36 (Const __i32 4713104))
          (37 (LoadOp (var __i32) (op Load32) (addr (Ref 35))))
          (38 (BiOp (var __i32) (op Equal) (lhs (Ref 37)) (rhs (Ref 36))))
          (39 (AssertOp (condition (Ref 38)))) (40 (Const __i32 4))
          (41 (BiOp (var esp) (op Add) (lhs (Ref 35)) (rhs (Ref 40))))
          (42 (OutsideContext (var ebp) (typ Int)))
          (43
           (StoreOp (op Store32) (addr (Ref 42)) (value (Ref 41)) (offset -24)))
          (44 (DupVar (var esi) (src (Ref 41)) (typ Int)))
          (45 (StoreOp (op Store32) (addr (Ref 44)) (value (Ref 26))))
          (46 (Const __i32 4))
          (47 (BiOp (var esp) (op Subtract) (lhs (Ref 41)) (rhs (Ref 46))))
          (48 (StoreOp (op Store32) (addr (Ref 47)) (value (Ref 44))))
          (49 (Const __i32 4))
          (50 (BiOp (var esp) (op Subtract) (lhs (Ref 47)) (rhs (Ref 49))))
          (51 (Const __i32 4713117))
          (52 (StoreOp (op Store32) (addr (Ref 50)) (value (Ref 51))))
          (53
           (SetGlobalOp (global_name __stack__) (value (Ref 50))
            (global_type Int)))
          (54
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (55
           (CallOp (var eax) (func KERNEL32.dll_GetVersionExA) (args ())
            (return_type Int)))
          (56 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (57 (Const __i32 4713117))
          (58 (LoadOp (var __i32) (op Load32) (addr (Ref 56))))
          (59 (BiOp (var __i32) (op Equal) (lhs (Ref 58)) (rhs (Ref 57))))
          (60 (AssertOp (condition (Ref 59)))) (61 (Const __i32 4))
          (62 (BiOp (var esp) (op Add) (lhs (Ref 56)) (rhs (Ref 61))))
          (63 (LoadOp (var __i32) (op Load32) (addr (Ref 44)) (offset 16)))
          (64 (DupVar (var ecx) (src (Ref 63)) (typ Int)))
          (65 (Const __i32 4847496))
          (66 (StoreOp (op Store32) (addr (Ref 65)) (value (Ref 64))))
          (67 (LoadOp (var __i32) (op Load32) (addr (Ref 44)) (offset 4)))
          (68 (DupVar (var eax) (src (Ref 67)) (typ Int)))
          (69 (Const __i32 4847508))
          (70 (StoreOp (op Store32) (addr (Ref 69)) (value (Ref 68))))
          (71 (LoadOp (var __i32) (op Load32) (addr (Ref 44)) (offset 8)))
          (72 (DupVar (var edx) (src (Ref 71)) (typ Int)))
          (73 (Const __i32 4847512))
          (74 (StoreOp (op Store32) (addr (Ref 73)) (value (Ref 72))))
          (75 (LoadOp (var __i32) (op Load32) (addr (Ref 44)) (offset 12)))
          (76 (DupVar (var esi) (src (Ref 75)) (typ Int)))
          (77 (Const __i32 32767))
          (78 (BiOp (var __i32) (op And) (lhs (Ref 76)) (rhs (Ref 77))))
          (79 (DupVar (var esi) (src (Ref 78)) (typ Int)))
          (80 (Const __i32 4847500))
          (81 (StoreOp (op Store32) (addr (Ref 80)) (value (Ref 79))))
          (82 (Const __i32 2))
          (83 (BiOp (var __i32) (op Equal) (lhs (Ref 64)) (rhs (Ref 82))))
          (84
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 83))))
        (roots
         ((Ref 0) (Ref 4) (Ref 8) (Ref 9) (Ref 10) (Ref 14) (Ref 15) (Ref 16)
          (Ref 17) (Ref 18) (Ref 22) (Ref 26) (Ref 31) (Ref 32) (Ref 33)
          (Ref 34) (Ref 35) (Ref 39) (Ref 42) (Ref 43) (Ref 45) (Ref 48)
          (Ref 52) (Ref 53) (Ref 54) (Ref 55) (Ref 56) (Ref 60) (Ref 62)
          (Ref 64) (Ref 66) (Ref 68) (Ref 70) (Ref 72) (Ref 74) (Ref 79)
          (Ref 81) (Ref 84))))
       ((id 1)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int))) (1 (Const __i32 32768))
          (2 (BiOp (var __i32) (op Or) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (DupVar (var esi) (src (Ref 2)) (typ Int)))
          (4 (Const __i32 4847500))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 3))))
          (6
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 2))) (roots ((Ref 0) (Ref 3) (Ref 5) (Ref 6))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (26
           (CallIndirectOp (var eax) (table_index (Ref 18))
            (args ((Ref 19) (Ref 5))) (return_type Int)))
          (27 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (28 (Const __i32 4713200))
          (29 (LoadOp (var __i32) (op Load32) (addr (Ref 27))))
          (30 (BiOp (var __i32) (op Equal) (lhs (Ref 29)) (rhs (Ref 28))))
          (31 (AssertOp (condition (Ref 30)))) (32 (Const __i32 4))
          (33 (BiOp (var esp) (op Add) (lhs (Ref 27)) (rhs (Ref 32))))
          (34
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 26)) (signed false)))
          (35 (Const __i32 23117))
          (36 (UniOp (var __i32) (op SignExtend16) (operand (Ref 34))))
          (37 (UniOp (var __i32) (op SignExtend16) (operand (Ref 35))))
          (38 (BiOp (var __i32) (op NotEqual) (lhs (Ref 36)) (rhs (Ref 37))))
          (39
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 3)) (condition (Ref 38))))
        (roots
         ((Ref 0) (Ref 5) (Ref 9) (Ref 11) (Ref 12) (Ref 15) (Ref 18) (Ref 19)
          (Ref 23) (Ref 24) (Ref 25) (Ref 26) (Ref 27) (Ref 31) (Ref 33)
          (Ref 39))))
       ((id 3)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 60)))
          (2 (DupVar (var ecx) (src (Ref 1)) (typ Int)))
          (3 (BiOp (var __i32) (op Add) (lhs (Ref 2)) (rhs (Ref 0))))
          (4 (DupVar (var ecx) (src (Ref 3)) (typ Int)))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 4))))
          (6 (Const __i32 17744))
          (7 (BiOp (var __i32) (op NotEqual) (lhs (Ref 5)) (rhs (Ref 6))))
          (8
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 4)) (condition (Ref 7))))
        (roots ((Ref 0) (Ref 4) (Ref 8))))
       ((id 4)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset 24)))
          (2 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 1))))
          (3 (DupVar (var eax) (src (Ref 2)) (typ Int))) (4 (Const __i32 267))
          (5 (BiOp (var __i32) (op Equal) (lhs (Ref 3)) (rhs (Ref 4))))
          (6
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 5))))
        (roots ((Ref 0) (Ref 3) (Ref 6))))
       ((id 5)
        (instrs
         ((0 (OutsideContext (var eax) (typ Int))) (1 (Const __i32 523))
          (2 (BiOp (var __i32) (op Equal) (lhs (Ref 0)) (rhs (Ref 1))))
          (3
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 7)) (fail (Block 6)) (condition (Ref 2))))
        (roots ((Ref 0) (Ref 3))))
       ((id 6)
        (instrs
         ((0 (OutsideContext (var esi) (typ Int)))
          (1 (OutsideContext (var ebp) (typ Int)))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -28)))
          (3
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 12))) (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3))))
       ((id 7)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 132)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))
          (4
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 8)) (condition (Ref 3))))
        (roots ((Ref 0) (Ref 4))))
       ((id 8)
        (instrs
         ((0 (Const __i32 0)) (1 (DupVar (var eax) (src (Ref 0)) (typ Int)))
          (2 (OutsideContext (var ecx) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 248)))
          (4 (OutsideContext (var esi) (typ Int)))
          (5
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 1) (Ref 2) (Ref 4) (Ref 5))))
       ((id 9)
        (instrs
         ((0 (OutsideContext (var ecx) (typ Int)))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 116)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))
          (4
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 10)) (condition (Ref 3))))
        (roots ((Ref 0) (Ref 4))))
       ((id 10)
        (instrs
         ((0 (Const __i32 0)) (1 (DupVar (var eax) (src (Ref 0)) (typ Int)))
          (2 (OutsideContext (var ecx) (typ Int)))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)) (offset 232)))
          (4 (OutsideContext (var esi) (typ Int)))
          (5
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 1) (Ref 2) (Ref 4) (Ref 5))))
       ((id 11)
        (instrs
         ((0 (OutsideContext (var __input_compare_arg__) (typ Int)))
          (1 (OutsideContext (var eax) (typ Int)))
          (2 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (OutsideContext (var ebp) (typ Int)))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2)) (offset -28)))
          (5
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 12)))
        (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4) (Ref 5))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func48068a__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713286))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 14)) (fail (Block 13)) (condition (Ref 13))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea59__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713298))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 14)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (9
           (CallOp (var eax) (func __func481649__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (11 (Const __i32 4713304))
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
          (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
          (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
          (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
          (17
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 16)) (fail (Block 15)) (condition (Ref 9))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 14) (Ref 16) (Ref 17))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea59__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713315))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 16)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (9
           (CallOp (var eax) (func __func483560__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (11 (Const __i32 4713321))
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
          (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
          (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
          (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
          (17 (OutsideContext (var esi) (typ Int)))
          (18 (OutsideContext (var ebp) (typ Int)))
          (19
           (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 17)) (offset -4)))
          (20 (Const __i32 4))
          (21 (BiOp (var esp) (op Subtract) (lhs (Ref 16)) (rhs (Ref 20))))
          (22 (Const __i32 4713329))
          (23 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 22))))
          (24
           (SetGlobalOp (global_name __stack__) (value (Ref 21))
            (global_type Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (26
           (CallOp (var eax) (func __func483362__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (27 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (28 (Const __i32 4713329))
          (29 (LoadOp (var __i32) (op Load32) (addr (Ref 27))))
          (30 (BiOp (var __i32) (op Equal) (lhs (Ref 29)) (rhs (Ref 28))))
          (31 (AssertOp (condition (Ref 30)))) (32 (Const __i32 4))
          (33 (BiOp (var esp) (op Add) (lhs (Ref 27)) (rhs (Ref 32))))
          (34 (Const __i32 0))
          (35
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 26)) (rhs (Ref 34))))
          (36
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 18)) (fail (Block 17)) (condition (Ref 35))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 14) (Ref 17) (Ref 18) (Ref 19) (Ref 23) (Ref 24) (Ref 25)
          (Ref 26) (Ref 27) (Ref 31) (Ref 33) (Ref 36))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713340))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 18)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (7
           (CallOp (var eax) (func KERNEL32.dll_GetCommandLineA) (args ())
            (return_type Int)))
          (8 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (9 (Const __i32 4713346))
          (10 (LoadOp (var __i32) (op Load32) (addr (Ref 8))))
          (11 (BiOp (var __i32) (op Equal) (lhs (Ref 10)) (rhs (Ref 9))))
          (12 (AssertOp (condition (Ref 11)))) (13 (Const __i32 4))
          (14 (BiOp (var esp) (op Add) (lhs (Ref 8)) (rhs (Ref 13))))
          (15 (Const __i32 20337236))
          (16 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 7))))
          (17 (OutsideContext (var ecx) (typ Int)))
          (18 (OutsideContext (var edx) (typ Int))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Subtract) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (Const __i32 4713357))
          (22 (StoreOp (op Store32) (addr (Ref 20)) (value (Ref 21))))
          (23
           (SetGlobalOp (global_name __stack__) (value (Ref 20))
            (global_type Int)))
          (24
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (25
           (CallOp (var eax) (func __func483240__) (args ((Ref 17) (Ref 18)))
            (return_type Int)))
          (26 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (27 (Const __i32 4713357))
          (28 (LoadOp (var __i32) (op Load32) (addr (Ref 26))))
          (29 (BiOp (var __i32) (op Equal) (lhs (Ref 28)) (rhs (Ref 27))))
          (30 (AssertOp (condition (Ref 29)))) (31 (Const __i32 4))
          (32 (BiOp (var esp) (op Add) (lhs (Ref 26)) (rhs (Ref 31))))
          (33 (Const __i32 4847472))
          (34 (StoreOp (op Store32) (addr (Ref 33)) (value (Ref 25))))
          (35 (Const __i32 4))
          (36 (BiOp (var esp) (op Subtract) (lhs (Ref 32)) (rhs (Ref 35))))
          (37 (Const __i32 4713367))
          (38 (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 37))))
          (39
           (SetGlobalOp (global_name __stack__) (value (Ref 36))
            (global_type Int)))
          (40
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (41
           (CallOp (var eax) (func __func48319e__) (args ((Ref 17) (Ref 18)))
            (return_type Int)))
          (42 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (43 (Const __i32 4713367))
          (44 (LoadOp (var __i32) (op Load32) (addr (Ref 42))))
          (45 (BiOp (var __i32) (op Equal) (lhs (Ref 44)) (rhs (Ref 43))))
          (46 (AssertOp (condition (Ref 45)))) (47 (Const __i32 4))
          (48 (BiOp (var esp) (op Add) (lhs (Ref 42)) (rhs (Ref 47))))
          (49 (Const __i32 0))
          (50
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 41)) (rhs (Ref 49))))
          (51
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 50))))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 12) (Ref 16)
          (Ref 17) (Ref 18) (Ref 22) (Ref 23) (Ref 24) (Ref 25) (Ref 26)
          (Ref 30) (Ref 34) (Ref 38) (Ref 39) (Ref 40) (Ref 41) (Ref 42)
          (Ref 46) (Ref 48) (Ref 51))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713378))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 20)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (9
           (CallOp (var eax) (func __func482f6b__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (11 (Const __i32 4713384))
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
          (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
          (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
          (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
          (17 (Const __i32 0))
          (18
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 9)) (rhs (Ref 17))))
          (19
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 18))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 14) (Ref 16) (Ref 19))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713395))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 22)))
        (roots
         ((Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12) (Ref 13)
          (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (9
           (CallOp (var eax) (func __func47f891__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (11 (Const __i32 4713401))
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
          (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
          (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
          (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
          (17 (OutsideContext (var ebp) (typ Int)))
          (18
           (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 9)) (offset -32)))
          (19 (OutsideContext (var esi) (typ Int)))
          (20 (BiOp (var __i32) (op Equal) (lhs (Ref 9)) (rhs (Ref 19))))
          (21
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 24)) (fail (Block 23)) (condition (Ref 20))))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 14) (Ref 16) (Ref 17) (Ref 18) (Ref 19) (Ref 21))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47ea34__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713414))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 20))))
          (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 20)) (rhs (Ref 22))))
          (24 (DupVar (var ecx) (src (Ref 21)) (typ Int)))
          (25
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 24)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 14) (Ref 18) (Ref 23) (Ref 24) (Ref 25))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (16
           (CallOp (var eax) (func KERNEL32.dll_GetStartupInfoA) (args ())
            (return_type Int)))
          (17 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (18 (Const __i32 4713427))
          (19 (LoadOp (var __i32) (op Load32) (addr (Ref 17))))
          (20 (BiOp (var __i32) (op Equal) (lhs (Ref 19)) (rhs (Ref 18))))
          (21 (AssertOp (condition (Ref 20)))) (22 (Const __i32 4))
          (23 (BiOp (var esp) (op Add) (lhs (Ref 17)) (rhs (Ref 22))))
          (24 (OutsideContext (var ecx) (typ Int)))
          (25 (OutsideContext (var edx) (typ Int))) (26 (Const __i32 4))
          (27 (BiOp (var esp) (op Subtract) (lhs (Ref 23)) (rhs (Ref 26))))
          (28 (Const __i32 4713433))
          (29 (StoreOp (op Store32) (addr (Ref 27)) (value (Ref 28))))
          (30
           (SetGlobalOp (global_name __stack__) (value (Ref 27))
            (global_type Int)))
          (31
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (32
           (CallOp (var eax) (func __func482f02__) (args ((Ref 24) (Ref 25)))
            (return_type Int)))
          (33 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (34 (Const __i32 4713433))
          (35 (LoadOp (var __i32) (op Load32) (addr (Ref 33))))
          (36 (BiOp (var __i32) (op Equal) (lhs (Ref 35)) (rhs (Ref 34))))
          (37 (AssertOp (condition (Ref 36)))) (38 (Const __i32 4))
          (39 (BiOp (var esp) (op Add) (lhs (Ref 33)) (rhs (Ref 38))))
          (40
           (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 32)) (offset -104)))
          (41
           (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
            (offset -56)))
          (42 (Const __i32 1))
          (43 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 41))))
          (44 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 42))))
          (45 (BiOp (var __i32) (op And) (lhs (Ref 43)) (rhs (Ref 44))))
          (46 (UniOp (var __i32) (op EqualsZero) (operand (Ref 45))))
          (47
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 26)) (fail (Block 25)) (condition (Ref 46))))
        (roots
         ((Ref 0) (Ref 1) (Ref 2) (Ref 6) (Ref 9) (Ref 13) (Ref 14) (Ref 15)
          (Ref 16) (Ref 17) (Ref 21) (Ref 24) (Ref 25) (Ref 29) (Ref 30)
          (Ref 31) (Ref 32) (Ref 33) (Ref 37) (Ref 39) (Ref 40) (Ref 47))))
       ((id 25)
        (instrs
         ((0 (OutsideContext (var ebp) (typ Int)))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset -52)))
          (2 (UniOp (var __i32) (op ZeroExtend16) (operand (Ref 1))))
          (3 (DupVar (var eax) (src (Ref 2)) (typ Int)))
          (4
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 27))) (roots ((Ref 0) (Ref 3) (Ref 4))))
       ((id 26)
        (instrs
         ((0 (Const __i32 10)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 3)))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Add) (lhs (Ref 3)) (rhs (Ref 6))))
          (8 (DupVar (var eax) (src (Ref 5)) (typ Int)))
          (9
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 27)))
        (roots ((Ref 1) (Ref 4) (Ref 7) (Ref 8) (Ref 9))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (26
           (CallIndirectOp (var eax) (table_index (Ref 17))
            (args ((Ref 18) (Ref 19))) (return_type Int)))
          (27 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (28 (Const __i32 4713462))
          (29 (LoadOp (var __i32) (op Load32) (addr (Ref 27))))
          (30 (BiOp (var __i32) (op Equal) (lhs (Ref 29)) (rhs (Ref 28))))
          (31 (AssertOp (condition (Ref 30)))) (32 (Const __i32 4))
          (33 (BiOp (var esp) (op Add) (lhs (Ref 27)) (rhs (Ref 32))))
          (34 (Const __i32 4))
          (35 (BiOp (var esp) (op Subtract) (lhs (Ref 33)) (rhs (Ref 34))))
          (36 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 26))))
          (37 (Const __i32 4))
          (38 (BiOp (var esp) (op Subtract) (lhs (Ref 35)) (rhs (Ref 37))))
          (39 (Const __i32 4713465))
          (40 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 39))))
          (41
           (SetGlobalOp (global_name __stack__) (value (Ref 38))
            (global_type Int)))
          (42
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (43
           (CallOp (var eax) (func __func434020__) (args ((Ref 18) (Ref 19)))
            (return_type Int)))
          (44 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (45 (Const __i32 4713465))
          (46 (LoadOp (var __i32) (op Load32) (addr (Ref 44))))
          (47 (BiOp (var __i32) (op Equal) (lhs (Ref 46)) (rhs (Ref 45))))
          (48 (AssertOp (condition (Ref 47)))) (49 (Const __i32 4))
          (50 (BiOp (var esp) (op Add) (lhs (Ref 44)) (rhs (Ref 49))))
          (51 (DupVar (var edi) (src (Ref 43)) (typ Int)))
          (52
           (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 51)) (offset -108)))
          (53 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -28)))
          (54 (BiOp (var __i32) (op NotEqual) (lhs (Ref 53)) (rhs (Ref 10))))
          (55
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator
         (Branch (succeed (Block 29)) (fail (Block 28)) (condition (Ref 54))))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 9) (Ref 10) (Ref 13) (Ref 16)
          (Ref 17) (Ref 18) (Ref 19) (Ref 23) (Ref 24) (Ref 25) (Ref 26)
          (Ref 27) (Ref 31) (Ref 36) (Ref 40) (Ref 41) (Ref 42) (Ref 43)
          (Ref 44) (Ref 48) (Ref 50) (Ref 51) (Ref 52) (Ref 55))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (13
           (CallOp (var eax) (func __func47f9c9__) (args ((Ref 5) (Ref 6)))
            (return_type Int)))
          (14 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (15 (Const __i32 4713481))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
          (17 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 15))))
          (18 (AssertOp (condition (Ref 17)))) (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
          (21
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 29)))
        (roots
         ((Ref 0) (Ref 1) (Ref 4) (Ref 5) (Ref 6) (Ref 10) (Ref 11) (Ref 12)
          (Ref 13) (Ref 14) (Ref 18) (Ref 20) (Ref 21))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (9
           (CallOp (var eax) (func __func47f9eb__) (args ((Ref 0) (Ref 1)))
            (return_type Int)))
          (10 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (11 (Const __i32 4713486))
          (12 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
          (13 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 11))))
          (14 (AssertOp (condition (Ref 13)))) (15 (Const __i32 4))
          (16 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
          (17
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 30)))
        (roots
         ((Ref 0) (Ref 1) (Ref 3) (Ref 6) (Ref 7) (Ref 8) (Ref 9) (Ref 10)
          (Ref 14) (Ref 16) (Ref 17))))
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
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))
          (18
           (CallOp (var eax) (func __func48065f__) (args ((Ref 10) (Ref 11)))
            (return_type Int)))
          (19 (GetGlobalOp (var esp) (global_name __stack__) (global_type Int)))
          (20 (Const __i32 4713545))
          (21 (LoadOp (var __i32) (op Load32) (addr (Ref 19))))
          (22 (BiOp (var __i32) (op Equal) (lhs (Ref 21)) (rhs (Ref 20))))
          (23 (AssertOp (condition (Ref 22)))) (24 (Const __i32 4))
          (25 (BiOp (var esp) (op Add) (lhs (Ref 19)) (rhs (Ref 24))))
          (26
           (SetGlobalOp (global_name __stack__) (value (Ref 25))
            (global_type Int)))
          (27
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator Return)
        (roots
         ((Ref 0) (Ref 4) (Ref 5) (Ref 10) (Ref 11) (Ref 15) (Ref 16) (Ref 17)
          (Ref 18) (Ref 19) (Ref 23) (Ref 25) (Ref 26) (Ref 27))))))
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
          (7 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 6)) (rhs (Ref 5))))
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
       ((id 3)
        (instrs
         ((0
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 5))) (roots ((Ref 0))))
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
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
          (11
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator (Goto (Block 5)))
        (roots ((Ref 0) (Ref 7) (Ref 9) (Ref 10) (Ref 11))))
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
            (global_type Int)))
          (8
           (GetGlobalOp (var __i32) (global_name __fpuStack__) (global_type Int)))))
        (terminator Return) (roots ((Ref 0) (Ref 2) (Ref 6) (Ref 7) (Ref 8))))))
     (locals
      ((eax ((name eax) (typ Int))) (ebp ((name ebp) (typ Int)))
       (ecx ((name ecx) (typ Int))) (edx ((name edx) (typ Int)))
       (esp ((name esp) (typ Int)))))) |}]
