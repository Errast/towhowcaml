open! Core

module DSL = Dsl
let compile : Sedlexing.lexbuf -> Mir.Func.t list =
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.main in
  fun buf ->
    try Sedlexing.with_tokenizer Lexer.token buf
    |> parser |> List.map ~f:Lowering.lower
    with Parser.Error -> let start_pos, end_pos = Sedlexing.lexing_positions buf in
      let cnum (pos:Lexing.position) = pos.pos_cnum - pos.pos_bol + 1 in
        failwith @@ sprintf "Parser error at %d:%d to %d:%d" start_pos.pos_lnum (cnum start_pos) end_pos.pos_lnum (cnum end_pos)
