%{
	open Types
%}
%token<string> IDENT 
%token<Radatnet.X86reg.t> REG
%token SEMI ";"
%token COMMA ","
%token COLON_EQ ":="
%token EQ "="
%token STAR "*"
%token LPAREN "(" RPAREN ")"
%token IF THEN ELSE FI
%token EOF

%type<expr> expr

%start<statement list> main
%%

main: stmts=list(statement); EOF { stmts }

statement:
	| lhs=IDENT ":=" rhs=expr ";" { Assign (lhs, rhs) }
	| lhs=REG ":=" rhs=expr ";" { AssignReg (lhs, rhs) }
	| func=IDENT ";" { FunCall (func, []) }
	| IF cond=expr THEN then_=list(statement) FI ";" { If {cond; then_; else_=[]} }
	| IF cond=expr THEN then_=list(statement) ELSE else_=list(statement) FI ";" { If {cond; then_; else_} }

expr:
	| eq_expr { $1 }

eq_expr:
	| lhs=mul_expr "=" rhs=mul_expr { Eq (lhs,rhs) }
	| mul_expr { $1 }

mul_expr:
	| lhs=atomic_expr "*" rhs=atomic_expr { Mul (lhs,rhs) }
	| atomic_expr { $1 }

atomic_expr:
	| i=IDENT { Ident i }
	| fn=IDENT "(" args=separated_list(",", expr)  ")" { FunCall (fn, args) }
	| "("e=expr ")" { e }
