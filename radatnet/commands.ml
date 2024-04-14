[@@@warning "-8"]

open! Core
open Basic
open Types
open Yojson.Basic

let open_file t file = "o '" ^ file ^ "'" |> run t |> ignore
let seek t address = "s " ^ string_of_int address |> run t |> ignore
let current_address t = run t "s" |> int_of_string

let seek_relative_opcodes t count =
  "so " ^ string_of_int count |> run t |> ignore

type analysis_level = LevelOne | LevelTwo | LevelThree

let analyze_all t = function
  | LevelOne -> run t "a" |> ignore
  | LevelTwo -> run t "aa" |> ignore
  | LevelThree -> run t "aaa" |> ignore

let list_functions t =
  run t "aflqj" |> Yojson.Basic.from_string |> Util.to_list
  |> List.map ~f:Util.to_int

let disassemble_function t =
  run t "pdfj" |> Yojson.Safe.from_string |> function_dissassembly_of_yojson

let analyze_opcode t =
  let (`List [ op ]) = run t "aoj 1" |> Yojson.Safe.from_string in
  Types.opcode_of_yojson op

let analyze_opcodes t n =
  if n <= 0 then invalid_arg "n must be > 0";
  run t ("aoj " ^ string_of_int n)
  |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list
  |> List.map ~f:Types.opcode_of_yojson

let get_func_blocks t =
  let (`List [ blocks ]) = run t "agfj" |> Yojson.Safe.from_string in
  Types.func_blocks_of_yojson blocks

let get_imports t =
  run t "iij" |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list
  |> List.map ~f:Types.bin_import_of_yojson
