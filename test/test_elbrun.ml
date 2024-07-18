open! Core

let compile str =
  Sedlexing.Utf8.from_string str
  |> Elbrun.compile
  |> sexp_of_list Mir.Func.sexp_of_t
  |> print_s

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
         (terminator
          (Branch (succeed (Block 6)) (fail (Block 1)) (condition (Ref 1))))
         (roots ()))
        ((id 1)
         (instrs
          ((0 (OutsideContext (var count) (typ Int))) (1 (Const __i32 7))
           (2 (BiOp (var c_mod8) (op And) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator
          (Branch (succeed (Block 2)) (fail (Block 4)) (condition (Ref 2))))
         (roots ()))
        ((id 2)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (Const __i32 4294967288))
           (2 (BiOp (var i) (op And) (lhs (Ref 0)) (rhs (Ref 1))))))
         (terminator (Goto (Block 3))) (roots ()))
        ((id 3)
         (instrs
          ((0 (OutsideContext (var dest) (typ Int)))
           (1 (OutsideContext (var val) (typ Int)))
           (2 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1))))
           (3 (Const __i32 4))
           (4 (BiOp (var dest) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
           (5 (OutsideContext (var c_mod8) (typ Int))) (6 (Const __i32 1))
           (7 (BiOp (var c_mod8) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))))
         (terminator
          (Branch (succeed (Block 3)) (fail (Block 5)) (condition (Ref 7))))
         (roots ((Ref 2))))
        ((id 4)
         (instrs
          ((0 (OutsideContext (var count) (typ Int)))
           (1 (DupVar (var i) (src (Ref 0)) (typ Int)))))
         (terminator (Goto (Block 5))) (roots ()))
        ((id 5)
         (instrs
          ((0 (OutsideContext (var dest) (typ Int)))
           (1 (OutsideContext (var val) (typ Int)))
           (2 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 28)))
           (3 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 24)))
           (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 20)))
           (5 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 16)))
           (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 12)))
           (7 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 8)))
           (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 4)))
           (9 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1))))
           (10 (Const __i32 32))
           (11 (BiOp (var dest) (op Add) (lhs (Ref 0)) (rhs (Ref 10))))
           (12 (OutsideContext (var i) (typ Int))) (13 (Const __i32 8))
           (14 (BiOp (var i) (op Subtract) (lhs (Ref 12)) (rhs (Ref 13))))))
         (terminator
          (Branch (succeed (Block 5)) (fail (Block 6)) (condition (Ref 14))))
         (roots
          ((Ref 2) (Ref 3) (Ref 4) (Ref 5) (Ref 6) (Ref 7) (Ref 8) (Ref 9))))
        ((id 6) (instrs ()) (terminator Return) (roots ()))))
      (locals ((c_mod8 ((name c_mod8) (typ Int))) (i ((name i) (typ Int)))))))
    |}]

let%expect_test "basic" =
  compile
    {|
  fn test(int a1 a2 a3) -> (int r1 r2) {
    if a1 - 1 {
      r1 = a2
    } else {
      r1 = a3
      if a1! {
        (a1+4)! = a2 * a3 - 4
      }
    }
    r2 = a1 + (a1! * 3) - a2 + a3
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
          ((0 (OutsideContext (var a1) (typ Int))) (1 (Const __i32 4))
           (2 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
           (3 (OutsideContext (var a2) (typ Int)))
           (4 (OutsideContext (var a3) (typ Int)))
           (5 (BiOp (var __i32) (op Multiply) (lhs (Ref 3)) (rhs (Ref 4))))
           (6 (Const __i32 4))
           (7 (BiOp (var __i32) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))
           (8 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 7))))))
         (terminator (Goto (Block 4))) (roots ((Ref 8))))
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
