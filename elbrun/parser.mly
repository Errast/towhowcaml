%{
open! Core
open Mir
open Syntax
%}
%token <string> IDENT
%token <string> REF_IDENT
%token <string> LABEL_IDENT
%token <int> NUMBER
%token IF
%token ELSE
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token BANG "!"
%token INT 
%token LONG
%token FLOAT
%token VEC
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
%token GO
%token DOUBLE_EQ "=="
%token NOT_EQ "!="
%token AMPERSAND "&"
%token PIPE "|"
%token NOT
%token EOF

%start <func_def list> main
%%
main: defs = list(func_def); EOF { defs }

func_def: FN; name=IDENT; 
  "(" args=separated_list(COMMA,var_decl); ")" 
  rets=func_def_return
  "{" locals=loption(terminated(separated_list(COMMA,var_decl), "->")) 
  body=statement* "}" 
  { {name;signature={args=List.concat args;returns=List.concat rets};locals=List.concat locals;body} }

func_def_return: { [] } | "->" "(" rets=separated_list(COMMA,var_decl) ")" { rets }

var_decl: typ=prim_typ; vars=list(IDENT) { List.map vars ~f:(fun i -> {typ;name=i}) }

prim_typ:
  | INT { Int }
  | LONG { Long }
  | FLOAT { Float }
  | VEC { Vec }

statement:
	| lhs=IDENT; "=" rhs=expr  { Let {lhs;rhs} }
    | lhs=REF_IDENT; "=" rhs=expr { Alias {lhs;rhs} }
	| addr=deref; "=" value=expr { Store {addr=fst addr;offset=snd addr;value} }
	| IF cond=expr; t=if_body  ELSE f=if_body { If {cond;t;f} }
	| IF cond=expr; t=if_body { If {cond;t;f=[]}}
	| LABEL_IDENT COLON { Label $1 }
    | GO LABEL_IDENT { Goto $2 }

if_body:
  | GO LABEL_IDENT { [Goto $2] }
  | "{" stmts=statement* "}" { stmts }

expr: keyword_expr { $1 }

keyword_expr: 
  | NOT keyword_expr { UniOp(Not, $2) }
  | eq_expr { $1 }

eq_expr: 
  | lhs=bit_expr1 "==" rhs=bit_expr1 { BiOp(Eq,lhs,rhs) }
  | lhs=bit_expr1 "!=" rhs=bit_expr1 { BiOp(NotEq,lhs,rhs) }
  | bit_expr1 { $1 }

bit_expr1:
  | lhs=bit_expr1 "|" rhs=bit_expr2 { BiOp(BitOr,lhs,rhs) }
  | bit_expr2 { $1 }

bit_expr2:
  | lhs=bit_expr2 "&" rhs=expr_atomic { BiOp(BitAnd, lhs,rhs) }
  | arith_expr1 { $1 }

arith_expr1:
  | lhs=arith_expr1; "+" rhs=arith_expr2 { BiOp(Add, lhs,rhs) }
  | lhs=arith_expr1; "-" rhs=arith_expr2 { BiOp(Sub, lhs,rhs) }
  | arith_expr2 { $1 }

arith_expr2: 
  | lhs=arith_expr2; "*" rhs=expr_atomic { BiOp(Mul, lhs,rhs) }
  | expr_atomic { $1 }

expr_atomic:
  | NUMBER { Const $1 }
  | v=REF_IDENT | v=IDENT { Var v }
  | addr=deref { Deref (fst addr, snd addr) }
  | "(" expr ")" { $2 }

deref:
  | expr_atomic "!" { $1,0 }
  | expr_atomic ":" NUMBER "!" { $1,$3 }
 
