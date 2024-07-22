open Core
open Parser
open Sedlexing

let string_of_token = function
  | NUMBER i -> i
  | VEC -> "vec"
  | INT -> "int"
  | STAR -> "*"
  | SEMI -> ";"
  | RPAREN -> ")"
  | LPAREN -> "("
  | RCURLY -> "}"
  | LCURLY -> "{"
  | RARROW -> "->"
  | PLUS -> "+"
  | PIPE -> "|"
  | NOT_EQ -> "!="
  | MINUS -> "-"
  | LONG -> "long"
  | IF -> "if"
  | GO -> "go"
  | FN -> "fn"
  | FLOAT -> "float"
  | EQ -> "="
  | EOF -> "<eof>"
  | ELSE -> "else"
  | DOUBLE_EQ -> "=="
  | COMMA -> ","
  | COLON -> ":"
  | BANG -> "!"
  | AMPERSAND -> "&"
  | NOT -> "not"
  | LANGLE -> "<"
  | RANGLE -> ">"
  | LOAD -> "load"
  | STORE -> "store"
  | SHIFT_LEFT -> "<<"
  | SHIFT_RIGHT -> ">>"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | V128 -> "v128"
  | VSPLAT -> "vsplat"
  | RETURN -> "return"
  | LOOP -> "loop"
  | WHILE -> "while"
  | TRUNC -> "trunc"
  | EXTEND -> "extend"
  | BITCAST -> "bitcast"
  | CLZ -> "clz"
  | XOR -> "^"
  | REF_IDENT s | IDENT s | LABEL_IDENT s | GLOBAL_IDENT s -> s

let token buf =
  let rec next_token buf =
    let ident_start = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_'] in
    let ident_continue = [%sedlex.regexp? ident_start | '0' .. '9'] in
    match%sedlex buf with
    | Plus white_space -> next_token buf
    | "--", Star (Compl ('\n' | eof)) -> next_token buf
    | "0x", Plus ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') | Plus '0' .. '9' -> NUMBER (Utf8.lexeme buf)
    | '(' -> LPAREN
    | ')' -> RPAREN
    | '{' -> LCURLY
    | '}' -> RCURLY
    | ',' -> COMMA
    | "int" -> INT
    | "long" -> LONG
    | "float" -> FLOAT
    | "vec" -> VEC
    | "fn" -> FN
    | "load" -> LOAD
    | "store" -> STORE
    | 'L' -> LONG
    | 'F' -> FLOAT
    | 'V' -> VEC
    | 'I' -> INT
    | "->" -> RARROW
    | '=' -> EQ
    | ':' -> COLON
    | ';' -> SEMI
    | '!' -> BANG
    | '+' -> PLUS
    | '-' -> MINUS
    | '*' -> STAR
    | "if" -> IF
    | "else" -> ELSE
    | "go" -> GO
    | "==" -> DOUBLE_EQ
    | "!=" -> NOT_EQ
    | '&' -> AMPERSAND
    | '|' -> PIPE
    | "not" -> NOT
    | "<<" -> SHIFT_LEFT
    | ">>" -> SHIFT_RIGHT
    | '<' -> LANGLE
    | '>' -> RANGLE
    | "i32" -> I32
    | "i16" -> I16
    | "i8" -> I8
    | "f64" -> F64
    | "f32" -> F32
    | "v128" -> V128
    | "vsplat" -> VSPLAT
    | "return" -> RETURN
    | "loop" -> LOOP
    | "while" -> WHILE
    | "trunc" -> TRUNC
    | "extend" -> EXTEND
    | "bitcast" -> BITCAST
    | "clz" -> CLZ
    | '^' -> XOR
    | '%', Star ident_continue -> REF_IDENT (Utf8.lexeme buf)
    | '.', Star ident_continue -> LABEL_IDENT (Utf8.lexeme buf)
    | '@', Star ident_continue -> GLOBAL_IDENT (Utf8.lexeme buf)
    | ident_start, Star ident_continue -> IDENT (Utf8.lexeme buf)
    | eof -> EOF
    | _ -> failwith @@ sprintf "lexer error %d" @@ Sedlexing.lexeme_start buf
  in
  let tok = next_token buf in
  (* printf "%s " (string_of_token tok); *)
  tok
