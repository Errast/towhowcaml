open! Core

let compile str =
  Sedlexing.Utf8.from_string str
  |> Elbrun.compile
  |> sexp_of_list Mir.Func.sexp_of_t
  |> print_s

 let%expect_test "store_big_float" = compile Towhowcaml.store_big_float;
  [%expect {|
    (((name __store_big_float__)
      (signature
       ((args (((name arg) (typ Float)) ((name dest_ptr) (typ Int))))
        (returns ())))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var arg) (typ Float)))
           (1 (UniOp (var f) (op BitcastFloatToLong) (operand (Ref 0))))
           (2 (LongConst __i64 9223372036854775807))
           (3 (BiOp (var abs_f) (op LongAnd) (lhs (Ref 1)) (rhs (Ref 2))))
           (4 (LongConst __i64 -4503599627370496))
           (5 (BiOp (var __i64) (op LongAdd) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (LongConst __i64 9214364837600034816))
           (7
            (SignedBiOp (var __i32) (op LongLessThan) (signed false)
             (lhs (Ref 5)) (rhs (Ref 6))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 7))))
         (roots ((Ref 0) (Ref 1) (Ref 3))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var f) (typ Long))) (1 (LongConst __i64 11))
           (2 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (LongConst __i64 -9223372036854775808))
           (4 (BiOp (var res_mantissa) (op LongOr) (lhs (Ref 2)) (rhs (Ref 3))))
           (5 (LongConst __i64 52))
           (6
            (SignedBiOp (var __i64) (op LongShiftRight) (signed false)
             (lhs (Ref 0)) (rhs (Ref 5))))
           (7 (UniOp (var __i32) (op LongToInt32) (operand (Ref 6))))
           (8 (Const __i32 15360))
           (9 (BiOp (var res_exp) (op Add) (lhs (Ref 7)) (rhs (Ref 8))))))
         (terminator (Goto (Block 4))) (roots ((Ref 0) (Ref 4) (Ref 9))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var abs_f) (typ Long)))
           (1 (LongConst __i64 9218868437227405312))
           (2
            (SignedBiOp (var __i32) (op LongGreaterThanEqual) (signed false)
             (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var f) (typ Long))) (1 (LongConst __i64 11))
           (2 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (LongConst __i64 -9223372036854775808))
           (4 (BiOp (var res_mantissa) (op LongOr) (lhs (Ref 2)) (rhs (Ref 3))))
           (5 (Const res_exp 32767))))
         (terminator (Goto (Block 4))) (roots ((Ref 0) (Ref 4) (Ref 5))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var abs_f) (typ Long))) (1 (LongConst __i64 0))
           (2 (BiOp (var __i32) (op LongNotEq) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 5)) (fail (Block 6)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var abs_f) (typ Long)))
           (1 (UniOp (var __i64) (op LongCountLeadingZeros) (operand (Ref 0))))
           (2 (UniOp (var __i32) (op LongToInt32) (operand (Ref 1))))
           (3 (Const __i32 117))
           (4 (BiOp (var __i32) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
           (5 (Const __i32 127))
           (6 (BiOp (var __i32) (op And) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (Const __i32 52))
           (8 (BiOp (var __i32) (op Subtract) (lhs (Ref 7)) (rhs (Ref 6))))
           (9 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 8))))
           (10 (LongConst __i64 65535))
           (11 (BiOp (var __i64) (op LongAnd) (lhs (Ref 9)) (rhs (Ref 10))))
           (12
            (SignedBiOp (var __i64) (op LongShiftRight) (signed false)
             (lhs (Ref 0)) (rhs (Ref 11))))
           (13 (UniOp (var res_exp) (op LongToInt32) (operand (Ref 12))))
           (14 (Const __i32 1))
           (15 (BiOp (var res_exp) (op Xor) (lhs (Ref 13)) (rhs (Ref 14))))
           (16 (Const __i32 15361))
           (17 (BiOp (var __i32) (op Subtract) (lhs (Ref 16)) (rhs (Ref 6))))
           (18 (BiOp (var res_exp) (op Or) (lhs (Ref 15)) (rhs (Ref 17))))
           (19 (OutsideContext (var f) (typ Long)))
           (20
            (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 19)) (rhs (Ref 1))))
           (21 (LongConst __i64 -9223372036854775808))
           (22
            (BiOp (var res_mantissa) (op LongOr) (lhs (Ref 20)) (rhs (Ref 21))))))
         (terminator (Goto (Block 7)))
         (roots ((Ref 0) (Ref 18) (Ref 19) (Ref 22))))
        ((id 6) (instrs ((0 (Const res_exp 0)) (1 (LongConst res_mantissa 0))))
         (terminator (Goto (Block 7))) (roots ((Ref 0) (Ref 1))))
        ((id 7)
         (instrs
          ((0 (OutsideContext (var dest_ptr) (typ Int)))
           (1 (OutsideContext (var res_mantissa) (typ Long)))
           (2 (StoreOp (op LongStore64) (addr (Ref 0)) (value (Ref 1))))
           (3 (OutsideContext (var res_exp) (typ Int)))
           (4 (OutsideContext (var f) (typ Long))) (5 (LongConst __i64 48))
           (6
            (SignedBiOp (var __i64) (op LongShiftRight) (signed false)
             (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (UniOp (var __i32) (op LongToInt32) (operand (Ref 6))))
           (8 (Const __i32 32768))
           (9 (BiOp (var __i32) (op And) (lhs (Ref 7)) (rhs (Ref 8))))
           (10 (BiOp (var res_exp) (op Or) (lhs (Ref 3)) (rhs (Ref 9))))
           (11 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 10)) (offset 8)))))
         (terminator Return) (roots ((Ref 0) (Ref 1) (Ref 4) (Ref 10))))))
      (locals
       ((abs_f ((name abs_f) (typ Long))) (f ((name f) (typ Long)))
        (res_exp ((name res_exp) (typ Int)))
        (res_mantissa ((name res_mantissa) (typ Long)))))))
    |}]
 

 let%expect_test "load_big_float" = compile Towhowcaml.load_big_float;
  [%expect {|
    (((name __load_big_float__)
      (signature
       ((args (((name float_ptr) (typ Int))))
        (returns (((name res) (typ Float))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var float_ptr) (typ Int)))
           (1 (LoadOp (var lower_bits) (op LongLoad64) (addr (Ref 0))))
           (2 (LongConst __i64 9223372036854775807))
           (3 (BiOp (var mantissa) (op LongAnd) (lhs (Ref 1)) (rhs (Ref 2))))
           (4
            (SignedLoadOp (var upper_bits) (op Load16) (addr (Ref 0))
             (signed false) (offset 8)))
           (5 (Const __i32 32767))
           (6 (BiOp (var exponent) (op And) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (Const __i32 15361))
           (8 (BiOp (var __i32) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))
           (9 (Const __i32 65535))
           (10 (BiOp (var __i32) (op And) (lhs (Ref 8)) (rhs (Ref 9))))
           (11 (Const __i32 17407))
           (12 (BiOp (var __i32) (op Subtract) (lhs (Ref 6)) (rhs (Ref 11))))
           (13 (Const __i32 65535))
           (14 (BiOp (var __i32) (op And) (lhs (Ref 12)) (rhs (Ref 13))))
           (15
            (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 10))
             (rhs (Ref 14))))))
         (terminator
          (Branch (succeed (Block 1)) (fail (Block 5)) (condition (Ref 15))))
         (roots ((Ref 0) (Ref 1) (Ref 3) (Ref 4) (Ref 6))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var upper_bits) (typ Int)))
           (1 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 0))))
           (2 (LongConst __i64 52))
           (3 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 1)) (rhs (Ref 2))))
           (4 (OutsideContext (var mantissa) (typ Long)))
           (5 (LongConst __i64 11))
           (6
            (SignedBiOp (var res_mantissa) (op LongShiftRight) (signed false)
             (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (BiOp (var result) (op LongOr) (lhs (Ref 3)) (rhs (Ref 6))))
           (8 (OutsideContext (var lower_bits) (typ Long)))
           (9 (LongConst __i64 2047))
           (10
            (BiOp (var rounded_part) (op LongAnd) (lhs (Ref 8)) (rhs (Ref 9))))
           (11 (LongConst __i64 1024))
           (12
            (SignedBiOp (var __i32) (op LongGreaterThan) (signed false)
             (lhs (Ref 10)) (rhs (Ref 11))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 3)) (condition (Ref 12))))
         (roots ((Ref 0) (Ref 4) (Ref 6) (Ref 7) (Ref 8) (Ref 10))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var result) (typ Long)))
           (1 (LongConst __i64 4611686018427387905))
           (2 (BiOp (var result) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator (Goto (Block 15))) (roots ((Ref 2))))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var result) (typ Long)))
           (1 (LongConst __i64 4611686018427387904))
           (2 (BiOp (var result) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var rounded_part) (typ Long)))
           (4 (LongConst __i64 1024))
           (5 (BiOp (var __i64) (op LongEq) (lhs (Ref 3)) (rhs (Ref 4))))))
         (terminator
          (Branch (succeed (Block 4)) (fail (Block 15)) (condition (Ref 5))))
         (roots ((Ref 2) (Ref 3))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var result) (typ Long)))
           (1 (OutsideContext (var res_mantissa) (typ Long)))
           (2 (LongConst __i64 1))
           (3 (BiOp (var __i64) (op LongAnd) (lhs (Ref 1)) (rhs (Ref 2))))
           (4 (BiOp (var result) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 3))))))
         (terminator (Goto (Block 15))) (roots ((Ref 1) (Ref 4))))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var lower_bits) (typ Long)))
           (1 (LongConst __i64 0))
           (2 (BiOp (var __i32) (op LongNotEq) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var exponent) (typ Int))) (4 (Const __i32 32767))
           (5 (BiOp (var __i32) (op Equal) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (BiOp (var __i32) (op And) (lhs (Ref 2)) (rhs (Ref 5))))))
         (terminator
          (Branch (succeed (Block 6)) (fail (Block 7)) (condition (Ref 6))))
         (roots ((Ref 0) (Ref 3))))
        ((id 6)
         (instrs
          ((0 (OutsideContext (var lower_bits) (typ Long)))
           (1 (LongConst __i64 11))
           (2 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (LongConst __i64 9221120237041090560))
           (4 (BiOp (var result) (op LongOr) (lhs (Ref 2)) (rhs (Ref 3))))))
         (terminator (Goto (Block 15))) (roots ((Ref 0) (Ref 4))))
        ((id 7)
         (instrs
          ((0 (OutsideContext (var exponent) (typ Int))) (1 (Const __i32 17406))
           (2
            (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 0))
             (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 8)) (fail (Block 9)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 8) (instrs ((0 (LongConst result 9218868437227405312))))
         (terminator (Goto (Block 15))) (roots ((Ref 0))))
        ((id 9)
         (instrs
          ((0 (OutsideContext (var exponent) (typ Int))) (1 (Const __i32 15297))
           (2
            (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 0))
             (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 10)) (fail (Block 11)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 10) (instrs ((0 (LongConst result 0))))
         (terminator (Goto (Block 15))) (roots ((Ref 0))))
        ((id 11)
         (instrs
          ((0 (Const __i32 0)) (1 (OutsideContext (var upper_bits) (typ Int)))
           (2 (BiOp (var __i32) (op Subtract) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (Const __i32 63))
           (4 (BiOp (var __i32) (op And) (lhs (Ref 2)) (rhs (Ref 3))))
           (5 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 4))))
           (6 (OutsideContext (var mantissa) (typ Long)))
           (7
            (SignedBiOp (var __i64) (op LongShiftRight) (signed false)
             (lhs (Ref 6)) (rhs (Ref 5))))
           (8 (LongConst __i64 11))
           (9
            (SignedBiOp (var result) (op LongShiftRight) (signed false)
             (lhs (Ref 7)) (rhs (Ref 8))))
           (10 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 6)) (rhs (Ref 5))))
           (11 (LongConst __i64 0))
           (12 (BiOp (var __i64) (op LongEq) (lhs (Ref 10)) (rhs (Ref 11))))
           (13 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 12))))
           (14 (LongConst __i64 2047))
           (15 (BiOp (var __i64) (op LongAnd) (lhs (Ref 7)) (rhs (Ref 14))))
           (16
            (BiOp (var rounded_part) (op LongOr) (lhs (Ref 13)) (rhs (Ref 15))))
           (17 (LongConst __i64 1024))
           (18
            (SignedBiOp (var __i32) (op LongGreaterThan) (signed false)
             (lhs (Ref 16)) (rhs (Ref 17))))))
         (terminator
          (Branch (succeed (Block 12)) (fail (Block 13)) (condition (Ref 18))))
         (roots ((Ref 1) (Ref 6) (Ref 9) (Ref 16))))
        ((id 12)
         (instrs
          ((0 (OutsideContext (var result) (typ Long))) (1 (LongConst __i64 1))
           (2 (BiOp (var result) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator (Goto (Block 15))) (roots ((Ref 2))))
        ((id 13)
         (instrs
          ((0 (OutsideContext (var rounded_part) (typ Long)))
           (1 (LongConst __i64 1024))
           (2 (BiOp (var __i64) (op LongEq) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 14)) (fail (Block 15)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 14)
         (instrs
          ((0 (OutsideContext (var result) (typ Long))) (1 (LongConst __i64 1))
           (2 (BiOp (var __i64) (op LongAnd) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (BiOp (var result) (op LongAdd) (lhs (Ref 0)) (rhs (Ref 2))))))
         (terminator (Goto (Block 15))) (roots ((Ref 3))))
        ((id 15)
         (instrs
          ((0 (OutsideContext (var result) (typ Long)))
           (1 (OutsideContext (var upper_bits) (typ Int)))
           (2 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
           (3 (LongConst __i64 32768))
           (4 (BiOp (var __i64) (op LongAnd) (lhs (Ref 2)) (rhs (Ref 3))))
           (5 (LongConst __i64 48))
           (6 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (BiOp (var result) (op LongOr) (lhs (Ref 0)) (rhs (Ref 6))))
           (8 (UniOp (var res) (op BitcastInt64ToFloat) (operand (Ref 7))))))
         (terminator Return) (roots ((Ref 1) (Ref 7) (Ref 8))))))
      (locals
       ((exponent ((name exponent) (typ Int)))
        (lower_bits ((name lower_bits) (typ Long)))
        (mantissa ((name mantissa) (typ Long))) (res ((name res) (typ Float)))
        (res_mantissa ((name res_mantissa) (typ Long)))
        (result ((name result) (typ Long)))
        (rounded_part ((name rounded_part) (typ Long)))
        (upper_bits ((name upper_bits) (typ Int)))))))
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
         (roots ((Ref 0))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 4))
           (2
            (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 0))
             (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 3)) (condition (Ref 2))))
         (roots ((Ref 0))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (DupVar (var back_count) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (DupVar (var back_dest) (src (Ref 2)) (typ Int)))))
         (terminator (Goto (Block 6))) (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3))))
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
         (terminator (Goto (Block 4)))
         (roots
          ((Ref 0) (Ref 2) (Ref 4) (Ref 7) (Ref 8) (Ref 9) (Ref 10) (Ref 11))))
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
         (roots ((Ref 1) (Ref 4) (Ref 7))))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var main_count) (typ Int)))
           (1 (OutsideContext (var count) (typ Int)))
           (2 (BiOp (var __i32) (op Equal) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator (BranchReturn (fail (Block 6)) (condition (Ref 2))))
         (roots ((Ref 0) (Ref 1))))
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
         (roots ((Ref 1) (Ref 4) (Ref 7))))))
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
         (((name src) (typ Int)) ((name dest) (typ Int))
          ((name count) (typ Int))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (UniOp (var __i32) (op EqualsZero) (operand (Ref 0))))))
         (terminator (BranchReturn (fail (Block 1)) (condition (Ref 1))))
         (roots ((Ref 0))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 2))
           (2 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var src) (typ Int)))
           (4 (BiOp (var src_end) (op Add) (lhs (Ref 3)) (rhs (Ref 2))))
           (5 (OutsideContext (var dest) (typ Int)))
           (6 (BiOp (var dest_end) (op Add) (lhs (Ref 5)) (rhs (Ref 2))))))
         (terminator (Goto (Block 2)))
         (roots ((Ref 0) (Ref 3) (Ref 4) (Ref 5) (Ref 6))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
           (2 (OutsideContext (var dest) (typ Int)))
           (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
           (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 1)) (rhs (Ref 3))))))
         (terminator (BranchReturn (fail (Block 3)) (condition (Ref 4))))
         (roots ((Ref 0) (Ref 2))))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var src) (typ Int))) (1 (Const __i32 4))
           (2 (BiOp (var src) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int))) (4 (Const __i32 4))
           (5 (BiOp (var dest) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (OutsideContext (var count) (typ Int))) (7 (Const __i32 1))
           (8 (BiOp (var count) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 4)) (condition (Ref 8))))
         (roots ((Ref 2) (Ref 5) (Ref 8))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var src_end) (typ Int)))
           (1 (DupVar (var src) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest_end) (typ Int)))
           (3 (DupVar (var dest) (src (Ref 2)) (typ Int))) (4 (Const count 0))))
         (terminator Return) (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4))))))
      (locals
       ((dest_end ((name dest_end) (typ Int)))
        (src_end ((name src_end) (typ Int))))))
     ((name __byte_diff__)
      (signature
       ((args
         (((name src) (typ Int)) ((name dest) (typ Int))
          ((name count) (typ Int))))
        (returns
         (((name src) (typ Int)) ((name dest) (typ Int))
          ((name count) (typ Int))))))
      (blocks
       (((id 0)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (UniOp (var __i32) (op EqualsZero) (operand (Ref 0))))))
         (terminator (BranchReturn (fail (Block 1)) (condition (Ref 1))))
         (roots ((Ref 0))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1 (OutsideContext (var count) (typ Int)))
           (2 (BiOp (var src_end) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int)))
           (4 (BiOp (var dest_end) (op Add) (lhs (Ref 3)) (rhs (Ref 1))))))
         (terminator (Goto (Block 2)))
         (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var src) (typ Int)))
           (1
            (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed false)))
           (2 (OutsideContext (var dest) (typ Int)))
           (3
            (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed false)))
           (4 (BiOp (var __i32) (op NotEqual) (lhs (Ref 1)) (rhs (Ref 3))))))
         (terminator (BranchReturn (fail (Block 3)) (condition (Ref 4))))
         (roots ((Ref 0) (Ref 2))))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var src) (typ Int))) (1 (Const __i32 1))
           (2 (BiOp (var src) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var dest) (typ Int))) (4 (Const __i32 1))
           (5 (BiOp (var dest) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (OutsideContext (var count) (typ Int))) (7 (Const __i32 1))
           (8 (BiOp (var count) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 4)) (condition (Ref 8))))
         (roots ((Ref 2) (Ref 5) (Ref 8))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var src_end) (typ Int)))
           (1 (DupVar (var src) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var dest_end) (typ Int)))
           (3 (DupVar (var dest) (src (Ref 2)) (typ Int))) (4 (Const count 0))))
         (terminator Return) (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3) (Ref 4))))))
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
    } else if load a1 {
        r1 = a3
        store:4 a1, a2 * a3 - 4
    } else {
      r1 = a3
   
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
         (roots ((Ref 0))))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var a2) (typ Int)))
           (1 (DupVar (var r1) (src (Ref 0)) (typ Int)))))
         (terminator (Goto (Block 5))) (roots ((Ref 0) (Ref 1))))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var a1) (typ Int)))
           (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 1))))
         (roots ((Ref 0))))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var a3) (typ Int)))
           (1 (DupVar (var r1) (src (Ref 0)) (typ Int)))
           (2 (OutsideContext (var a1) (typ Int)))
           (3 (OutsideContext (var a2) (typ Int)))
           (4 (BiOp (var __i32) (op Multiply) (lhs (Ref 3)) (rhs (Ref 0))))
           (5 (Const __i32 4))
           (6 (BiOp (var __i32) (op Subtract) (lhs (Ref 4)) (rhs (Ref 5))))
           (7 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 6)) (offset 4)))))
         (terminator (Goto (Block 5))) (roots ((Ref 0) (Ref 1) (Ref 2) (Ref 3))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var a3) (typ Int)))
           (1 (DupVar (var r1) (src (Ref 0)) (typ Int)))))
         (terminator (Goto (Block 5))) (roots ((Ref 0) (Ref 1))))
        ((id 5)
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
         (terminator Return) (roots ((Ref 0) (Ref 5) (Ref 7) (Ref 8))))))
      (locals ((r1 ((name r1) (typ Int))) (r2 ((name r2) (typ Int)))))))
    |}]
