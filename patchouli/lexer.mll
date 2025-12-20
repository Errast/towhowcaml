{
	open Parser
	exception Eof
	let lexbuf_contents lb =
	  let open Lexing in
	  let pos = lb.lex_curr_pos in
	  let len = lb.lex_buffer_len - lb.lex_curr_pos in
	  (Bytes.to_string (Bytes.sub lb.lex_buffer pos len))
}
rule token = parse
	| [' ' '\t' '\n']+ { token lexbuf }
	| "(*" (_*) "*)" { token lexbuf }
	| ';' { SEMI }
	| ',' { COMMA }
	| ":=" { COLON_EQ }
	| '=' { EQ }
	| '*' | "∗" { STAR }
	| '+' { PLUS }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| "IF" { IF }
	| "THEN" { THEN }
	| "ELSE" { ELSE }
	| "FI" { FI }
	| "Byte store" { IDENT "Byte store" }
	| "Word store" { IDENT "Word store" }
	| "Doubleword store" { IDENT "Doubleword store" }
	| "EAX" { REG `eax }
	| "EBX" { REG `ebx }
	| "ECX" { REG `ecx }
	| "EDX" { REG `edx }
	| "ESI" { REG `esi }
	| "EDI" { REG `edi }
	| "ESP" { REG `esp }
	| "EBP" { REG `ebp }
	| "OF" { REG `OF}
	| "CF" { REG `CF}
	| "SF" { REG `SF}
	| "ZF" { REG `ZF}
	| "PF" { REG `PF}
	| "AF" { REG `AF}
	| (['a'-'z' 'A'-'Z']+ as id) { IDENT id }
	| (['0'-'9']+ as digits) { INT_LIT (int_of_string digits) }
	| eof { EOF  }
	| _ as c { failwith @@ Printf.sprintf "%C, %s" c @@ lexbuf_contents lexbuf }
