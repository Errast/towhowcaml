open Wrapper
open Yojson.Basic

let open_file t file = "o '" ^ file ^ "'" |> run t |> ignore
let seek t address = "s " ^ string_of_int address |> run t |> ignore
let current_address t = run t "s" |> Option.get |> int_of_string
let seek_relative_opcodes t count = "so " ^ string_of_int count |> run t |> ignore

type analysis_Level = LevelOne | LevelTwo | LevelThree

let analyze_all t = function
  | LevelOne -> run t "a" |> ignore
  | LevelTwo -> run t "aa" |> ignore
  | LevelThree -> run t "aaa" |> ignore

let list_functions t =
  run t "aflqj" |> Option.get |> Yojson.Basic.from_string |> Util.to_list
  |> List.map Util.to_int

type instr_info = { offset : int; size : char }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type function_dissassembly = {
  name : string;
  size : int;
  address : int [@key "addr"];
  ops: instr_info list
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

let disassemble_function t = run t "pdfj" |> Option.get |> Yojson.Safe.from_string |> function_dissassembly_of_yojson