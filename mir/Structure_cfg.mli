open! Core 

type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of int * wasm_control * wasm_control
  | WasmCodeReturn of int
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : int; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of int
  | WasmFallthrough
[@@deriving sexp]

val structure_cfg : Func.t -> wasm_control
val find_idoms : (Set.Make(Int).t, immutable) Array.Permissioned.t -> int array * Set.Make(Int).t array
