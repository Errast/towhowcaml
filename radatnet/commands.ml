[@@@warning "-8"]

open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Basic
open Types

let open_file t file = "o '" ^ file ^ "'" |> run t |> ignore

type analysis_level = LevelOne | LevelTwo | LevelThree | LevelFour

let analyze_all t = function
  | LevelOne -> run t "a" |> ignore
  | LevelTwo -> run t "aa" |> ignore
  | LevelThree -> run t "aaa" |> ignore
  | LevelFour -> run t "aaaa" |> ignore

let list_functions t =
  run t "aflqj" |> Yojson.Safe.from_string |> [%of_yojson: int array]

let disassemble_function t addr =
  run_at t addr "pdfj" |> Yojson.Safe.from_string
  |> function_dissassembly_of_yojson

let analyze_opcode t addr =
  analyze_opcode t addr |> Types.opcode_of_basic_opcode

let analyze_opcodes t addr n =
  if n <= 0 then invalid_arg "n must be > 0";
  run_at t addr ("aoj " ^ string_of_int n)
  |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list
  |> List.map ~f:Types.opcode_of_yojson

let get_func_blocks t addr =
  run_at t addr "afbj" |> Yojson.Safe.from_string
  |> [%of_yojson: func_block array]

let get_imports t =
  run t "iij" |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list
  |> List.map ~f:Types.bin_import_of_yojson

let get_surrounding_func t addr =
  run_at t addr "afo" |> String.strip |> int_of_string
