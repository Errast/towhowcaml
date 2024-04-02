open Ppx_yojson_conv_lib.Yojson_conv
open Sexplib.Std

type operand_type = Register | Immediate | Memory [@@deriving sexp]

let operand_type_of_yojson = function
  | `Int 1 -> Register
  | `Int 2 -> Immediate
  | `Int 3 -> Memory
  | yojson -> of_yojson_error "invalid operand type" yojson

type instr_info = { offset : int; size : int }
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]

type function_dissassembly = {
  name : string;
  size : int;
  address : int; [@key "addr"]
  ops : instr_info list;
}
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]

type x86_segment_reg = Cs | Ds | Ss | Es | Fs | Gs [@@deriving sexp]

let x86_segment_reg_of_yojson = function
  | `String "cs" -> Cs
  | `String "ds" -> Ds
  | `String "ss" -> Ss
  | `String "es" -> Es
  | `String "fs" -> Fs
  | `String "gs" -> Gs
  | yojson -> of_yojson_error "invalid segment register" yojson

type operand =
  | Immediate of { size : int; value : int }
  | Register of { size : int; reg : X86reg.t }
  | Memory of {
      size : int;
      base : X86reg.t option;
      index : X86reg.t option;
      scale : int;
      displacement : int;
      segment : x86_segment_reg option;
    }
[@@deriving sexp]

let operand_of_yojson json =
  match json with
  | `Assoc (("size", `Int size) :: _ :: ("type", `String type_str) :: rest) as
    yojson -> (
      match (type_str, rest) with
      | "imm", [ ("value", `Int value) ] -> Immediate { size; value }
      | "reg", [ ("value", value) ] -> (
          try Register { size; reg = X86reg.t_of_yojson value }
          with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (l, _) ->
            raise @@ Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (l, json))
      | "mem", rest ->
          let segment, rest =
            match rest with
            | ("segment", seg) :: rest ->
                (Some (x86_segment_reg_of_yojson seg), rest)
            | _ -> (None, rest)
          in
          let base, rest =
            match rest with
            | ("base", base) :: rest -> (Some (X86reg.t_of_yojson base), rest)
            | _ -> (None, rest)
          in
          let index, rest =
            match rest with
            | ("index", index) :: rest -> (Some (X86reg.t_of_yojson index), rest)
            | _ -> (None, rest)
          in
          let scale, rest =
            match rest with
            | ("scale", `Int scale) :: rest -> (scale, rest)
            | _ -> of_yojson_error "missing mem operand scale" yojson
          in
          let disp, rest =
            match rest with
            | ("disp", `Int disp) :: rest -> (disp, rest)
            | _ -> (0, rest)
          in
          if rest <> [] then of_yojson_error "bad mem fields" yojson;
          Memory { size; scale; displacement = disp; base; index; segment }
      | _, _ -> of_yojson_error "invalid operand type" yojson)
  | yojson -> of_yojson_error "invalid operand" yojson

type opex = { operands : operand list }
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]

type opcode = {
  opex : opex;
  id : X86_instr.t;
  address : int; [@key "addr"]
  prefix : int; [@default 0] (* not doing this type now *)
}
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]

type jump_table_case = { value : int; jump : int } [@@deriving of_yojson, sexp]

type jump_table = { offset : int; cases : jump_table_case list }
[@@deriving of_yojson, sexp]

[@@@warning "-11"]

type func_block = {
  offset : int;
  size : int;
  jump_to : int option; [@key "jump"] [@yojson.option]
  fail_to : int option; [@key "fail"] [@yojson.option]
  ops : instr_info list;
  switch_to : jump_table option; [@key "switchop"] [@yojson.option]
}
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]

type func_blocks = {
  name : string option; [@yojson.option]
  offset : int;
  blocks : func_block list;
}
[@@deriving of_yojson, sexp] [@@yojson.allow_extra_fields]
