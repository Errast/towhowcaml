open! Core

type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of Types.branch_target * wasm_control * wasm_control
  | WasmCodeReturn of Types.branch_target
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of {
      bb_id : Types.branch_target;
      targets : int list;
      default : int;
    }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of Types.branch_target
  | WasmFallthrough
[@@deriving sexp]

val structure_cfg : Func.t -> wasm_control
val to_dot : Func.t -> string
