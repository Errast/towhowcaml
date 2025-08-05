%{
open! Core
open Mir
open Syntax

let or_zero = function
  | None -> 0
  | Some s -> int_of_string s


%}
%token <string> IDENT
%token <string> REF_IDENT
%token <string> LABEL_IDENT
%token <string> GLOBAL_IDENT
%token <string> NUMBER
%token INT LONG FLOAT VEC
%token IF
%token ELSE
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token BANG "!"
%token COMMA ","
%token LPAREN "("
%token RPAREN ")"
%token LCURLY "{"
%token RCURLY "}"
%token FN
%token RARROW "->"
%token EQ "="
%token SEMI ";"
%token COLON ":"
%token GO RETURN LOOP WHILE
%token DOUBLE_EQ "=="
%token NOT_EQ "!="
%token AMPERSAND "&"
%token PIPE "|"
%token NOT
%token LANGLE "<"
%token RANGLE ">"
%token STORE
%token LOAD
%token SHIFT_LEFT "<<" SHIFT_RIGHT ">>"
%token I8 I16 I32 I64 F32 F64 V128
%token VSPLAT TRUNC EXTEND BITCAST CLZ XOR
%token EOF

%start <func_def list> main
%%

(* something of a hack to have partial precedence ordering *)
%inline or_atom(X):
  | expr_atomic { $1 }
  | X { $1 }

main: defs = list(func_def); EOF { defs }

func_def: FN; name=IDENT; 
  "(" args=separated_list(COMMA,var_decl); ")" 
  rets=func_def_return
  "{" locals=loption(terminated(separated_list(COMMA,local_decl), "->")) 
  body=statements "}" 
  { {name;signature={args=List.concat args;returns=List.concat rets};locals=List.concat locals;body} }

func_def_return: { [] } | "->" "(" rets=separated_list(COMMA,var_decl) ")" { rets }

var_decl: typ=prim_typ; vars=list(IDENT) { List.map vars ~f:(fun i -> {typ;name=i}) }

local_decl: typ=prim_typ; vars=nonempty_list(local_ident) { List.map vars ~f:(fun (s,i) -> {Mir.Builder.typ;name=i;scope=s}) }
local_ident:
  | IDENT { (`Local, $1) }
  | GLOBAL_IDENT { (`Global, $1) }


prim_typ:
  | INT { Int }
  | LONG { Long }
  | FLOAT { Float }
  | VEC { Vec }

statements: 
  | { [] }
  | statements statement { $2 @ $1 }

statement:
	| lhs=IDENT; "=" rhs=expr  { [Let {lhs;rhs}] }
	| lhs=GLOBAL_IDENT; "=" rhs=expr  { [Let {lhs;rhs}] }
    | lhs=REF_IDENT; "=" rhs=expr { [Alias {lhs;rhs}] }
	| STORE offset=preceded(":", NUMBER)? size=size addr=expr "," value=expr { [Store {addr=addr;offset=or_zero offset;value;size}] }
	| STORE offset=preceded(":", NUMBER)? I8 addr=expr "," value=expr { [Store8 {addr=addr;offset=or_zero offset;value}] }
	| STORE offset=preceded(":", NUMBER)? I16 addr=expr "," value=expr { [Store16 {addr=addr;offset=or_zero offset;value}] }
    | if_statement { $1 }
	| LABEL_IDENT COLON { [Label $1] }
    | return { $1 }
    | GO LABEL_IDENT { [Goto $2] }
    | LOOP "{" body=statements "}" WHILE cond=expr { let l = fresh () in 
         If{cond;t=[Goto l];f=[]}::(body @ [Label l]) }

return:
  | RETURN { [Return] }

if_statement:
  | IF cond=expr; t=if_true_body ELSE f=if_statement { [If {cond;t;f}] }
  | IF cond=expr; t=if_true_body  ELSE f=if_body { [If {cond;t;f}] }
  | IF cond=expr; t=if_true_body { [If {cond;t;f=[]}]}

if_true_body: 
  | return { $1 }
  | if_body { $1 }

if_body:
  | GO LABEL_IDENT { [Goto $2] }
  | "{" stmts=statements "}" { stmts }

expr: 
  | eq_expr { $1 }
  | bit_expr1 { $1 }
  | arith_expr1 { $1 }
  | shift_expr { $1 }
  | expr_atomic { $1 }

eq_expr: 
  | lhs=expr_atomic "==" rhs=expr_atomic { BiOp(Eq,lhs,rhs) }
  | lhs=expr_atomic "!=" rhs=expr_atomic { BiOp(NotEq,lhs,rhs) }
  | lhs=expr_atomic "==" LONG rhs=expr_atomic { BiOp(LongEq,lhs,rhs) }
  | lhs=expr_atomic "!=" LONG rhs=expr_atomic { BiOp(LongNotEq,lhs,rhs) }
  | lhs=expr_atomic op=comp_op signed=boption("!") rhs=expr_atomic { SignBiOp {op;lhs;rhs;signed} }
  | lhs=expr_atomic op=long_comp_op LONG signed=boption("!") rhs=expr_atomic { SignBiOp {op;lhs;rhs;signed} }

comp_op: 
  | "<" { IntLT }
  | ">" { IntGT }
  | "<" "=" { IntLTE }
  | ">" "=" { IntGTE }

long_comp_op: 
  | "<" { LongLT }
  | ">" { LongGT }
  | "<" "=" { LongLTE }
  | ">" "=" { LongGTE }

bit_expr1:
  | lhs=or_atom(bit_expr1) "|" rhs=or_atom(bit_expr2) { BiOp(BitOr,lhs,rhs) }
  | lhs=or_atom(bit_expr1) "|" LONG rhs=or_atom(bit_expr2) { BiOp(LongOr,lhs,rhs) }
  | bit_expr1_2 { $1 }

bit_expr1_2:
  | lhs=or_atom(bit_expr1_2) XOR rhs=or_atom(bit_expr2) { BiOp(BitXor,lhs,rhs) }
  | lhs=or_atom(bit_expr1_2) XOR LONG rhs=or_atom(bit_expr2) { BiOp(LongXor,lhs,rhs) }
  | bit_expr2 { $1 }

bit_expr2:
  | lhs=or_atom(bit_expr2) "&" rhs=expr_atomic { BiOp(BitAnd,lhs,rhs) }
  | lhs=or_atom(bit_expr2) "&" LONG rhs=expr_atomic { BiOp(LongAnd,lhs,rhs) }

shift_expr: 
  | lhs=or_atom(shift_expr) "<<" rhs=expr_atomic { BiOp(ShiftLeft,lhs,rhs) }
  | lhs=or_atom(shift_expr) "<<" LONG rhs=expr_atomic { BiOp(LongShiftLeft,lhs,rhs) }
  | lhs=or_atom(shift_expr) ">>" signed=boption("!") rhs=expr_atomic { SignBiOp{op=ShiftRight;lhs;rhs;signed} }
  | lhs=or_atom(shift_expr) ">>" LONG signed=boption("!") rhs=expr_atomic { SignBiOp{op=LongShiftRight;lhs;rhs;signed} }

arith_expr1:
  | lhs=or_atom(arith_expr1) "+" rhs=or_atom(arith_expr2) { BiOp(Add, lhs,rhs) }
  | lhs=or_atom(arith_expr1) "+" LONG rhs=or_atom(arith_expr2) { BiOp(LongAdd, lhs,rhs) }
  | lhs=or_atom(arith_expr1) "-" rhs=or_atom(arith_expr2) { BiOp(Sub, lhs,rhs) }
  | lhs=or_atom(arith_expr1) "-" LONG rhs=or_atom(arith_expr2) { BiOp(LongSub, lhs,rhs) }
  | arith_expr2 { $1 }

arith_expr2: 
  | lhs=or_atom(arith_expr2) "*" rhs=expr_atomic { BiOp(Mul, lhs,rhs) }
  | lhs=or_atom(arith_expr2) "*" LONG rhs=expr_atomic { BiOp(LongMul, lhs,rhs) }

expr_atomic:
  | NUMBER { Const $1 }
  | NUMBER LONG { LongConst $1 }
  | v=IDENT { Var v }
  | v=GLOBAL_IDENT { Var v }
  | v=REF_IDENT { Use v }
  | LOAD offset=preceded(":", NUMBER)? size=size addr=expr_atomic { Deref {addr;offset=or_zero offset;size} }
  | LOAD offset=preceded(":",NUMBER)? I8 signed=boption("!") addr=expr_atomic { Deref8 (addr,signed,or_zero offset) }
  | LOAD offset=preceded(":",NUMBER)? I16 signed=boption("!") addr=expr_atomic { Deref16 (addr,signed,or_zero offset) }
  | "(" expr ")" { $2 }
  | NOT expr_atomic { UniOp(Not, $2) }
  | VSPLAT lane_shape=lane_shape expr=expr_atomic { Splat (lane_shape, expr) }
  | TRUNC expr_atomic { UniOp(TruncLongToInt, $2) }
  | EXTEND expr_atomic { UniOp(ZextIntToLong, $2) }
  | EXTEND "!" expr_atomic { UniOp(SextIntToLong, $3) }
  | BITCAST expr_atomic { UniOp(BitcastLongToFloat, $2) }
  | BITCAST FLOAT "->" LONG expr=expr_atomic { UniOp(BitcastFloatToLong, expr) }
  | CLZ expr_atomic { UniOp(CountLeadingZeros, $2) }
  | CLZ LONG expr_atomic { UniOp(LongCountLeadingZeros, $3) }

size: { Int }
  | INT { Int }
  | LONG { Long }
  | FLOAT { Float }
  | VEC { Vec }

lane_shape:
  | I8 { `I8 }
  | I16 { `I16 }
  | I32 { `I32 }
  | I64 { `I64 }
  | F32 { `F32 }
  | F64 { `F64 }
