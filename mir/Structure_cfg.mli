open! Core
type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of Block.t * wasm_control * wasm_control
  | WasmCodeReturn of Block.t
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : Block.t; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of Block.t
  | WasmFallthrough [@@deriving sexp_of]

val to_dot : Func.t -> string
val structure_cfg : Func.t -> wasm_control
