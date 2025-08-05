open Syntax

type int_ = |
type long_ = |
type float_ = |
type vec_ = |

type _ var_decl =
  | Int : string -> int_ var_decl
  | Long : string -> long_ var_decl
  | Float : string -> float_ var_decl
  | Vec : string -> vec_ var_decl

let var_to_mir : type t. t var_decl -> Mir.variable = function
  | Int name -> { name; typ = Int }
  | Long name -> { name; typ = Long }
  | Float name -> { name; typ = Float }
  | Vec name -> { name; typ = Vec }

type _ var = Var of string [@@unboxed]
type (_, _) s = |
type z = |

module type HKTList = sig
  type 'a elem
  type 'len t = [] : z t | ( :: ) : 'a elem * 'len t -> ('len, 'a) s t
  type 'a func = { f : 'b. 'b elem -> 'a } [@@unboxed]

  val map_list : 'a func -> 'len t -> 'a list
end

module HKTList (T : sig
  type 'a t
end) : HKTList with type 'a elem = 'a T.t = struct
  type 'a elem = 'a T.t
  type 'len t = [] : z t | ( :: ) : 'a T.t * 'len t -> ('len, 'a) s t
  type 'a func = { f : 'b. 'b T.t -> 'a } [@@unboxed]

  let[@tail_mod_cons] rec map_list : type len. 'a func -> len t -> 'a list =
   fun f xs -> match xs with [] -> [] | x :: xs -> f.f x :: map_list f xs
end

module Params = HKTList (struct
  type 'a t = 'a var_decl
end)

module Args = HKTList (struct
  type 'a t = 'a var
end)

module Vars = Params
module Locals = Args
module Returns = Params
module RetVars = Args

type ('size, 'underlying, 'bitcast) tag =
  | I8 : ([> `I8 ], int_, _) tag
  | I16 : ([> `I16 ], int_, _) tag
  | Int : ([> `Int ], int_, _) tag
  | Long : ([> `Long ], long_, [> `Float ]) tag
  | Float : ([> `Float ], float_, [> `Long ]) tag
  | Vec : ([> `Vec ], vec_, _) tag

type ('size, 'underlying) size_tag =
  ('size, 'underlying, [ `Float | `Long ]) tag

type 'underlying lane_tag =
  ([ `I8 | `I16 | `Int | `Long | `Float ], 'underlying) size_tag

type 'a type_tag = ([ `Int | `Long | `Float | `Vec ], 'a) size_tag

type (_, _) uni_op =
  | Not : (int_, int_) uni_op
  | TruncLongToInt : (long_, int_) uni_op
  | SextIntToLong : (int_, long_) uni_op
  | ZextIntToLong : (int_, long_) uni_op
  (* really all these polymorphic variants should be open, even in tag itself, but we can get away with only these few *)
  | BitcastLongToFloat : (long_, [> `Float ]) uni_op
  | BitcastFloatToLong : (float_, [> `Long ]) uni_op
  | CountLeadingZeros : (int_, int_) uni_op
  | LongCountLeadingZeros : (long_, long_) uni_op

type (_, _, _) bi_op =
  | Add : (int_, int_, int_) bi_op
  | Sub : (int_, int_, int_) bi_op
  | Mul : (int_, int_, int_) bi_op
  | BitAnd : (int_, int_, int_) bi_op
  | BitOr : (int_, int_, int_) bi_op
  | BitXor : (int_, int_, int_) bi_op
  | Eq : (int_, int_, int_) bi_op
  | NotEq : (int_, int_, int_) bi_op
  | ShiftLeft : (int_, int_, int_) bi_op
  | LongAdd : (long_, long_, long_) bi_op
  | LongSub : (long_, long_, long_) bi_op
  | LongMul : (long_, long_, long_) bi_op
  | LongAnd : (long_, long_, long_) bi_op
  | LongOr : (long_, long_, long_) bi_op
  | LongXor : (long_, long_, long_) bi_op
  | LongEq : (long_, long_, long_) bi_op
  | LongNotEq : (long_, long_, long_) bi_op
  | LongShiftLeft : (long_, long_, long_) bi_op

type (_, _, _) sign_bi_op =
  | IntGT : (int_, int_, int_) sign_bi_op
  | IntLT : (int_, int_, int_) sign_bi_op
  | IntGTE : (int_, int_, int_) sign_bi_op
  | IntLTE : (int_, int_, int_) sign_bi_op
  | LongGT : (long_, long_, int_) sign_bi_op
  | LongLT : (long_, long_, int_) sign_bi_op
  | LongGTE : (long_, long_, int_) sign_bi_op
  | LongLTE : (long_, long_, int_) sign_bi_op
  | ShiftRight : (int_, int_, int_) sign_bi_op
  | LongShiftRight : (long_, long_, long_) sign_bi_op

type _ expr =
  | Const : string -> int_ expr
  | LongConst : string -> long_ expr
  | Var : 'a var -> 'a expr
  | Use : string -> 'a expr
  | UniOp : ('arg, 'out) uni_op * 'arg expr -> 'out expr
  | BiOp : ('arg1, 'arg2, 'out) bi_op * 'arg1 expr * 'arg2 expr -> 'out expr
  | SignBiOp :
      ('arg1, 'arg2, 'out) sign_bi_op
      * 'arg1 expr
      * 'arg2 expr
      * [ `Signed | `Unsigned ]
      -> 'out expr
  | Deref : int_ expr * int * 'out type_tag -> 'out expr
  | Deref8 : int_ expr * [ `Sext | `Zext ] * int -> int_ expr
  | Deref16 : int_ expr * [ `Sext | `Zext ] * int -> int_ expr
  | Splat : 'arg lane_tag * 'arg expr -> vec_ expr

type statement =
  | Set : 'a var * 'a expr -> statement
  | Alias : string * 'a expr -> statement
  | Store : int_ expr * int * (_, 'value, _) tag * 'value expr -> statement
  | If of int_ expr * statement list * statement list
  | Label of string
  | Goto of string
  | Return
  | Sequence of statement list

let uni_op_to_syntax : type arg out. (arg, out) uni_op -> Syntax.uni_op =
  function
  | Not -> Not
  | TruncLongToInt -> TruncLongToInt
  | SextIntToLong -> SextIntToLong
  | ZextIntToLong -> ZextIntToLong
  | BitcastLongToFloat -> BitcastLongToFloat
  | BitcastFloatToLong -> BitcastFloatToLong
  | CountLeadingZeros -> CountLeadingZeros
  | LongCountLeadingZeros -> LongCountLeadingZeros

let bi_op_to_syntax : type arg1 arg2 out.
    (arg1, arg2, out) bi_op -> Syntax.bi_op =
 fun op ->
  match op with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | BitAnd -> BitAnd
  | BitOr -> BitOr
  | BitXor -> BitXor
  | Eq -> Eq
  | NotEq -> NotEq
  | ShiftLeft -> ShiftLeft
  | LongAdd -> LongAdd
  | LongSub -> LongSub
  | LongMul -> LongMul
  | LongAnd -> LongAnd
  | LongOr -> LongOr
  | LongXor -> LongXor
  | LongEq -> LongEq
  | LongNotEq -> LongNotEq
  | LongShiftLeft -> LongShiftLeft

let sign_bi_op_to_syntax : type arg1 arg2 out.
    (arg1, arg2, out) sign_bi_op -> Syntax.sign_bi_op =
 fun op ->
  match op with
  | IntGT -> IntGT
  | IntLT -> IntLT
  | IntGTE -> IntGTE
  | IntLTE -> IntLTE
  | LongGT -> LongGT
  | LongLT -> LongLT
  | LongGTE -> LongGTE
  | LongLTE -> LongLTE
  | ShiftRight -> ShiftRight
  | LongShiftRight -> LongShiftRight

let tag_to_syntax : type t. t type_tag -> Mir.local_type = function
  | Int -> Int
  | Long -> Long
  | Float -> Float
  | Vec -> Vec

let tag_to_lane_shape : type t. t lane_tag -> Mir.vec_lane_shape = function
  | Int -> `I32
  | Long -> `I64
  | Float -> `F64
  | I8 -> `I8
  | I16 -> `I16

let rec expr_to_syntax : type t. t expr -> Syntax.expr = function
  | Const num -> Const num
  | Var (Var name) -> Var name
  | Use alias -> Use alias
  | UniOp (op, arg) -> UniOp (uni_op_to_syntax op, expr_to_syntax arg)
  | BiOp (op, lhs, rhs) ->
      BiOp (bi_op_to_syntax op, expr_to_syntax lhs, expr_to_syntax rhs)
  | SignBiOp (op, lhs, rhs, signed) ->
      SignBiOp
        {
          op = sign_bi_op_to_syntax op;
          lhs = expr_to_syntax lhs;
          rhs = expr_to_syntax rhs;
          signed = (match signed with `Signed -> true | `Unsigned -> false);
        }
  | Deref (addr, offset, size) ->
      Deref { addr = expr_to_syntax addr; offset; size = tag_to_syntax size }
  | Deref8 (addr, signed, offset) ->
      Deref8
        ( expr_to_syntax addr,
          (match signed with `Sext -> true | `Zext -> false),
          offset )
  | Deref16 (addr, signed, offset) ->
      Deref16
        ( expr_to_syntax addr,
          (match signed with `Sext -> true | `Zext -> false),
          offset )
  | LongConst num -> LongConst num
  | Splat (lane_shape, arg) ->
      Splat (tag_to_lane_shape lane_shape, expr_to_syntax arg)

let statements_to_syntax : statement list -> Syntax.statement list =
  let rec go : statement list -> Syntax.statement list -> Syntax.statement list
      =
   fun stmts acc ->
    match stmts with
    | [] -> acc
    | Set (Var lhs, rhs) :: stmts ->
        go stmts (Let { lhs; rhs = expr_to_syntax rhs } :: acc)
    | Return :: stmts -> go stmts (Return :: acc)
    | Store (addr, offset, I8, value) :: stmts ->
        go stmts
        @@ Store8
             {
               addr = expr_to_syntax addr;
               offset;
               value = expr_to_syntax value;
             }
           :: acc
    | Store (addr, offset, I16, value) :: stmts ->
        go stmts
        @@ Store16
             {
               addr = expr_to_syntax addr;
               offset;
               value = expr_to_syntax value;
             }
           :: acc
    | Store (addr, offset, (Int | Long | Float | Vec), value) :: stmts ->
        go stmts
        @@ Store
             {
               addr = expr_to_syntax addr;
               offset;
               value = expr_to_syntax value;
               size = Int;
             }
           :: acc
    | If (cond, t, f) :: stmts ->
        go stmts
          (If { cond = expr_to_syntax cond; t = go t []; f = go f [] } :: acc)
    | Label str :: stmts -> go stmts (Label str :: acc)
    | Goto str :: stmts -> go stmts (Goto str :: acc)
    | Alias (lhs, rhs) :: stmts ->
        go stmts (Alias { lhs; rhs = expr_to_syntax rhs } :: acc)
    | Sequence seq :: stmts -> go (seq @ stmts) acc
  in
  fun stmts -> go stmts []

let fn : type arg_len ret_len local_len.
    string ->
    arg_len Params.t ->
    ret_len Returns.t ->
    local_len Vars.t ->
    (arg_len Args.t ->
    ret_len RetVars.t ->
    local_len Locals.t ->
    statement list) ->
    Mir.Func.t =
 fun name params returns locals body ->
  let rec erase : type t. t Params.t -> t Args.t = function
    | [] -> []
    | (Int s | Long s | Float s | Vec s) :: vs -> Var s :: erase vs
  in
  let statements = body (erase params) (erase returns) (erase locals) in
  let body = statements_to_syntax statements in
  let signature =
    Mir.
      {
        args = Params.map_list { f = var_to_mir } params;
        returns = Params.map_list { f = var_to_mir } returns;
      }
  in
  let locals =
    Params.map_list
      {
        f =
          (fun v ->
            let Mir.{ name; typ } = var_to_mir v in
            Mir.Builder.{ name; typ; scope = `Local });
      }
      locals
  in
  Lowering.lower { name; signature; locals; body }

let ( := ) : type t. t var -> t expr -> statement = fun lhs rhs -> Set (lhs, rhs)
let ( ! ) : type t. t var -> t expr = fun var -> Var var

let ( %= ) : type t. string -> t expr -> statement =
 fun lhs rhs -> Alias (lhs, rhs)

let ( !% ) : type t. string -> t expr = fun alias -> Use alias

type if_acc = (int_ expr * statement list) list

type _ if_cont =
  | End : statement if_cont
  | Else : (statement list -> statement if_cont -> statement) if_cont
  | Elif : (int_ expr -> statement list -> 'a if_cont -> 'a) if_cont

let rec if_ : type cont.
    if_acc -> int_ expr -> statement list -> cont if_cont -> cont =
 fun acc cond if_true cont ->
  let fold if_false =
    List.fold_left
      (fun if_false (cond, if_true) -> If (cond, if_true, [ if_false ]))
      (If (cond, if_true, if_false))
      acc
  in
  match cont with
  | End -> fold []
  | Else -> fun if_false End -> fold if_false
  | Elif -> if_ ((cond, if_true) :: acc)

let if_ : type cont. int_ expr -> statement list -> cont if_cont -> cont =
 fun cond if_true cont -> if_ [] cond if_true cont

let else_ = Else
let end_ = End
let elif_ = Elif

type while_ = While

let while_ = While

let loop_ : statement list -> while_ -> int_ expr -> statement =
 fun body While cond ->
  let label = fresh () in
  Sequence (Label label :: (body @ [ If (cond, [ Goto label ], []) ]))

let mkbiop : type arg1 arg2 out.
    (arg1, arg2, out) bi_op -> arg1 expr -> arg2 expr -> out expr =
 fun op lhs rhs -> BiOp (op, lhs, rhs)

let mkuniop : type arg out. (arg, out) uni_op -> arg expr -> out expr =
 fun op arg -> UniOp (op, arg)

let mksbiop : type arg1 arg2 out.
    (arg1, arg2, out) sign_bi_op ->
    arg1 expr ->
    [< `Signed | `Unsigned ] * arg2 expr ->
    out expr =
 fun op lhs (signed, rhs) -> SignBiOp (op, lhs, rhs, signed)

let i8 : (_, _) size_tag = I8
let i16 : (_, _) size_tag = I16
let int : (_, _) size_tag = Int
let long : (_, _) size_tag = Long
let float : (_, _) size_tag = Float
let vec : (_, _) size_tag = Vec
let i num = Const (string_of_int num)
let l num = LongConst (string_of_int num)
let ll num = LongConst (Int64.to_string num)

type 'a signed_arg = [ `Signed | `Unsigned ] * 'a expr

let s rhs = (`Signed, rhs)
let u rhs = (`Unsigned, rhs)

type 'a bin_op = 'a expr -> 'a expr -> 'a expr
type 'a cmp_op = 'a expr -> 'a signed_arg -> int_ expr

let ( == ) = mkbiop Eq
let ( != ) = mkbiop NotEq
let ( ==: ) = mkbiop LongEq
let ( !=: ) = mkbiop LongNotEq
let ( < ) = mksbiop IntLT
let ( <= ) = mksbiop IntLTE
let ( > ) = mksbiop IntGT
let ( >= ) = mksbiop IntGTE
let ( <: ) = mksbiop LongLT
let ( <=: ) = mksbiop LongLTE
let ( >: ) = mksbiop LongGT
let ( >=: ) = mksbiop LongGTE
let ( + ) = mkbiop Add
let ( - ) = mkbiop Sub
let ( * ) = mkbiop Mul
let ( +: ) = mkbiop LongAdd
let ( -: ) = mkbiop LongSub
let ( *: ) = mkbiop LongMul
let ( lor ) = mkbiop BitOr
let ( |: ) = mkbiop LongOr
let ( & ) = mkbiop BitAnd
let ( &: ) = mkbiop LongAnd
let ( ^ ) = mkbiop BitXor
let ( ^: ) = mkbiop LongXor
let ( << ) = mkbiop ShiftLeft
let ( <<: ) = mkbiop LongShiftLeft
let ( >> ) = mksbiop ShiftRight
let ( >>: ) = mksbiop LongShiftRight
let load ?(offset = 0) size addr = Deref (addr, offset, size)

let load_s ?(offset = 0) (size : ([< `I8 | `I16 ], _) size_tag) addr =
  match size with
  | I8 -> Deref8 (addr, `Sext, offset)
  | I16 -> Deref16 (addr, `Sext, offset)

let load_z ?(offset = 0) (size : ([< `I8 | `I16 ], _) size_tag) addr =
  match size with
  | I8 -> Deref8 (addr, `Zext, offset)
  | I16 -> Deref16 (addr, `Zext, offset)

let splat lane_tag arg = Splat (lane_tag, arg)
let not = mkuniop Not
let trunc = mkuniop TruncLongToInt
let extend_z = mkuniop ZextIntToLong
let extend_s = mkuniop SextIntToLong

let bitcast (type arg out) (tag : ([ `Long | `Float ], arg, out) tag)
    (arg : arg expr) : out expr =
  match tag with
  | Long -> UniOp (BitcastLongToFloat, arg)
  | Float -> UniOp (BitcastFloatToLong, arg)

let clz (type t) (tag : ([ `Int | `Long ], t) size_tag) (arg : t expr) : t expr
    =
  match tag with
  | Int -> UniOp (CountLeadingZeros, arg)
  | Long -> UniOp (LongCountLeadingZeros, arg)

type at = At

let at = At
let store ?(offset = 0) size value At addr = Store (addr, offset, size, value)
let return = Return
