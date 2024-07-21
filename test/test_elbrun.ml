open! Core

let compile str =
  Sedlexing.Utf8.from_string str
  |> Elbrun.compile
  |> sexp_of_list Mir.Func.sexp_of_t
  |> print_s

 let%expect_test "load_big_float" = compile Towhowcaml.load_big_float;
  [%expect {|
    (((name __load_bit_float__)
      (signature
       ((args (((name float_ptr) (typ Int))))
        (returns (((name res) (typ Float))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var float_ptr) (typ Int)))
           (1 (LoadOp (var lower_bits) (op LongLoad64) (addr (Ref 0))))
           (2
            (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
             (offset 8)))
           (3
            (UniOp (var upper_bits) (op Int32ToLongUnsigned) (operand (Ref 2))))
           (4 (LongConst __i64 9223372036854775807))
           (5 (BiOp (var significand) (op LongAnd) (lhs (Ref 1)) (rhs (Ref 4))))
           (6 (LongConst __i64 65535))
           (7 (BiOp (var upper_bits) (op LongAnd) (lhs (Ref 3)) (rhs (Ref 6))))
           (8 (UniOp (var upper_int) (op LongToInt32) (operand (Ref 7))))
           (9 (Const __i32 32767))
           (10 (BiOp (var exponent) (op And) (lhs (Ref 8)) (rhs (Ref 9))))
           (11 (Const __i32 15361))
           (12 (BiOp (var __i32) (op Subtract) (lhs (Ref 10)) (rhs (Ref 11))))
           (13 (Const __i32 65535))
           (14 (BiOp (var __i32) (op And) (lhs (Ref 12)) (rhs (Ref 13))))
           (15 (Const __i32 17407))
           (16 (BiOp (var __i32) (op Subtract) (lhs (Ref 10)) (rhs (Ref 15))))
           (17 (Const __i32 65535))
           (18 (BiOp (var __i32) (op And) (lhs (Ref 16)) (rhs (Ref 17))))
           (19
            (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 14))
             (rhs (Ref 18))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 3)) (condition (Ref 19))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var upper_bits) (typ Long)))
           (1 (LongConst __i64 52))
           (2 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var significand) (typ Long)))
           (4 (LongConst __i64 11))
           (5
            (SignedBiOp (var __i64) (op LongShiftRight) (signed false)
             (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (BiOp (var result) (op LongOr) (lhs (Ref 2)) (rhs (Ref 5))))
           (7 (OutsideContext (var lower_bits) (typ Long)))
           (8 (LongConst __i64 2047))
           (9 (BiOp (var __i64) (op LongAnd) (lhs (Ref 7)) (rhs (Ref 8))))
           (10 (LongConst __i64 0))
           (11
            (SignedBiOp (var __i32) (op LongGreaterThan) (signed false)
             (lhs (Ref 9)) (rhs (Ref 10))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 3)) (condition (Ref 11))))
         (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var result) (typ Long)))
           (1 (LongConst __i64 4611686018427387905))
           (2 (BiOp (var lower_bits) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator Return) (roots ()))))
      (locals
       ((exponent ((name exponent) (typ Int)))
        (lower_bits ((name lower_bits) (typ Long)))
        (result ((name result) (typ Long)))
        (significand ((name significand) (typ Long)))
        (upper_bits ((name upper_bits) (typ Long)))
        (upper_int ((name upper_int) (typ Int)))))))
    |}]

let%expect_test "memset" = compile Towhowcaml.elbrun_code;
  [%expect {|
    (((name __int_memset__)
      (signature
       ((args
         (((name dest) (typ Int)) ((name val) (typ Int))
          ((name count) (typ Int))))
        (returns ())))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (UniOp (var __i32) (op EqualsZero) (operand (Ref 0))))))
         (terminator (BranchReturn (fail (Block 1)) (condition (Ref 1))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 4))
           (2
            (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 0))
             (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 3)) (condition (Ref 2))))
         (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (DupVar (var back_count) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var back_dest) (src (Ref 2)) (typ Int)))))
         (terminator (Goto (Block 6))) (roots ()))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 3))
           (2 (BiOp (var back_count) (op And) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (Const __i32 4294967292))
           (4 (BiOp (var main_count) (op And) (lhs (Ref 0)) (rhs (Ref 3))))
           (5 (Const __i32 2))
           (6 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (OutsideContext (var dest) (typ Int)))
           (8 (BiOp (var back_dest) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))
           (9 (OutsideContext (var val) (typ Int)))
           (10 (VecSplatOp (var splat) (value (Ref 9)) (shape I32)))
           (11 (DupVar (var main_i) (src (Ref 4)) (typ Int)))))
         (terminator (Goto (Block 4))) (roots ()))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var dest) (typ Int)))
           (1 (OutsideContext (var splat) (typ Vec)))
           (2 (StoreOp (op VecStore128) (addr (Ref 0)) (value (Ref 1))))
           (3 (Const __i32 16))
           (4 (BiOp (var dest) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
           (5 (OutsideContext (var main_i) (typ Int))) (6 (Const __i32 4))
           (7 (BiOp (var main_i) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))))
         (terminator
          (Branch (succeed (Block 4)) (fail (Block 5)) (condition (Ref 7))))
         (roots ((Ref 2))))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var main_count) (typ Int)))
           (1 (OutsideContext (var count) (typ Int)))
           (2 (BiOp (var __i32) (op Equal) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator (BranchReturn (fail (Block 6)) (condition (Ref 2))))
         (roots ()))
        ((id 6)
         (instrs
          ((0 (OutsideContext (var back_dest) (typ Int)))
           (1 (OutsideContext (var val) (typ Int)))
           (2 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1))))
           (3 (Const __i32 4))
           (4 (BiOp (var back_dest) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
           (5 (OutsideContext (var back_count) (typ Int))) (6 (Const __i32 1))
           (7 (BiOp (var back_count) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))))
         (terminator
          (Branch (succeed (Block 6)) (fail (Block 7)) (condition (Ref 7))))
         (roots ((Ref 2))))))
      (locals
       ((back_count ((name back_count) (typ Int)))
        (back_dest ((name back_dest) (typ Int)))
        (main_count ((name main_count) (typ Int)))
        (main_i ((name main_i) (typ Int))) (splat ((name splat) (typ Vec))))))
     ((name __int_diff__)
      (signature
       ((args
         (((name src) (typ Int)) ((name dest) (typ Int))
          ((name count) (typ Int))))
        (returns
         (((name s2) (typ Int)) ((name d2) (typ Int)) ((name c) (typ Int))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (UniOp (var __i32) (op EqualsZero) (operand (Ref 0))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 1))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int))) (4 (Const c 0))))
         (terminator Return) (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 2))
           (2 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var src) (typ Int)))
           (4 (BiOp (var src_end) (op Add) (lhs (Ref 3)) (rhs (Ref 2))))
           (5 (OutsideContext (var dest) (typ Int)))
           (6 (BiOp (var dest_end) (op Add) (lhs (Ref 5)) (rhs (Ref 2))))))
         (terminator (Goto (Block 3))) (roots ()))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
           (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 1)) (rhs (Ref 3))))))
         (terminator
          (Branch (succeed (Block 4)) (fail (Block 5)) (condition (Ref 4))))
         (roots ()))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int)))
           (4 (OutsideContext (var count) (typ Int)))
           (5 (DupVar (var c) (src (Ref 4)) (typ Int)))))
         (terminator Return) (roots ()))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var src) (typ Int))) (1 (Const __i32 4))
           (2 (BiOp (var src) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int))) (4 (Const __i32 4))
           (5 (BiOp (var dest) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (OutsideContext (var count) (typ Int))) (7 (Const __i32 1))
           (8 (BiOp (var count) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 6)) (condition (Ref 8))))
         (roots ()))
        ((id 6)
         (instrs
          ((0 (OutsideContext (var src_end) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest_end) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int))) (4 (Const c 0))))
         (terminator Return) (roots ()))))
      (locals
       ((dest_end ((name dest_end) (typ Int)))
        (src_end ((name src_end) (typ Int))))))
     ((name __byte_diff__)
      (signature
       ((args
         (((name src) (typ Int)) ((name dest) (typ Int))
          ((name count) (typ Int))))
        (returns
         (((name s2) (typ Int)) ((name d2) (typ Int)) ((name c) (typ Int))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (UniOp (var __i32) (op EqualsZero) (operand (Ref 0))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 1))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int))) (4 (Const c 0))))
         (terminator Return) (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (OutsideContext (var count) (typ Int)))
           (2 (BiOp (var src_end) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int)))
           (4 (BiOp (var dest_end) (op Add) (lhs (Ref 3)) (rhs (Ref 1))))))
         (terminator (Goto (Block 3))) (roots ()))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1
            (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed false)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3
            (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed false)))
           (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 1)) (rhs (Ref 3))))))
         (terminator
          (Branch (succeed (Block 4)) (fail (Block 5)) (condition (Ref 4))))
         (roots ()))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int)))
           (4 (OutsideContext (var count) (typ Int)))
           (5 (DupVar (var c) (src (Ref 4)) (typ Int)))))
         (terminator Return) (roots ()))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var src) (typ Int))) (1 (Const __i32 1))
           (2 (BiOp (var src) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int))) (4 (Const __i32 1))
           (5 (BiOp (var dest) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (OutsideContext (var count) (typ Int))) (7 (Const __i32 1))
           (8 (BiOp (var count) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 6)) (condition (Ref 8))))
         (roots ()))
        ((id 6)
         (instrs
          ((0 (OutsideContext (var src_end) (typ Int)))
           (1 (DupVar (var s2) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest_end) (typ Int)))
           (3 (DupVar (var d2) (src (Ref 2)) (typ Int))) (4 (Const c 0))))
         (terminator Return) (roots ()))))
      (locals
       ((dest_end ((name dest_end) (typ Int)))
        (src_end ((name src_end) (typ Int)))))))
    |}]

let%expect_test "basic" =
  compile
    {|
  fn test(int a1 a2 a3) -> (int r1 r2) {
    if a1 - 1 {
      r1 = a2
    } else {
      r1 = a3
      if load a1 {
        store:4 a1, a2 * a3 - 4
      }
    }
    r2 = a1 + (load a1 * 3) - a2 + a3
  }
  |};
  [%expect {|
    (((name test)
      (signature
       ((args
         (((name a1) (typ Int)) ((name a2) (typ Int)) ((name a3) (typ Int))))
        (returns (((name r1) (typ Int)) ((name r2) (typ Int))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var a1) (typ Int))) (1 (Const __i32 1))
           (2 (BiOp (var __i32) (op Subtract) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 2))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var a2) (typ Int)))
           (1 (DupVar (var r1) (src (Ref 0)) (typ Int)))))
         (terminator (Goto (Block 4))) (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var a3) (typ Int)))
           (1 (DupVar (var r1) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var a1) (typ Int)))
           (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 3))))
         (roots ()))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var a1) (typ Int)))
           (1 (OutsideContext (var a2) (typ Int)))
           (2 (OutsideContext (var a3) (typ Int)))
           (3 (BiOp (var __i32) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
           (4 (Const __i32 4))
           (5 (BiOp (var __i32) (op Subtract) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset 4)))))
         (terminator (Goto (Block 4))) (roots ((Ref 6))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var a1) (typ Int)))
           (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
           (2 (Const __i32 3))
           (3 (BiOp (var __i32) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
           (4 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
           (5 (OutsideContext (var a2) (typ Int)))
           (6 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (OutsideContext (var a3) (typ Int)))
           (8 (BiOp (var r2) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))))
         (terminator Return) (roots ()))))
      (locals ())))
    |}]
