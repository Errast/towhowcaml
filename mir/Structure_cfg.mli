type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of int * wasm_control * wasm_control
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : int; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of { bb_id : int }
[@@deriving sexp]

val structure_cfg : Func.t -> wasm_control
