open Core
open Parser
open Sedlexing

let string_of_token = function
  | NUMBER i -> string_of_int i
  | VEC -> "vector"
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
  | REF_IDENT s | IDENT s | LABEL_IDENT s -> s

let token buf =
  let rec next_token buf =
    let ident_start = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_'] in
    let ident_continue = [%sedlex.regexp? ident_start | '0' .. '9'] in
    match%sedlex buf with
    | Plus white_space -> next_token buf
    | "0x", Plus ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') | Plus '0' .. '9' ->
        let i = Int.of_string @@ Utf8.lexeme buf in
        if i <= 0xFFFFFFFF then NUMBER i else failwith "big num"
    | '(' -> LPAREN
    | ')' -> RPAREN
    | '{' -> LCURLY
    | '}' -> RCURLY
    | ',' -> COMMA
    | "int" -> INT
    | "long" -> LONG
    | "float" -> FLOAT
    | "vector" -> VEC
    | "fn" -> FN
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
    | '%', Star ident_continue -> REF_IDENT (Utf8.lexeme buf)
    | '.', Star ident_continue -> LABEL_IDENT (Utf8.lexeme buf)
    | ident_start, Star ident_continue -> IDENT (Utf8.lexeme buf)
    | eof -> EOF
    | _ -> failwith @@ sprintf "lexer error %d" @@ Sedlexing.lexeme_start buf
  in
  let tok = next_token buf in
  (* printf "%s " (string_of_token tok); *)
  tok
