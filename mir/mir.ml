module Instr = Instr
module Builder = Mir_builder
include Types
module Local_info = Local_info
module Block = Block
module Variable = Variable
module Vec = Vec
module Func = Func
module Instr_list = Instr_list
module Bits= Bits
module Structure_cfg = Structure_cfg
module Wasm_backend = Wasm_backend
module Symbolic_eval = Symbolic_eval
include Opt

let float_size : [ `Single | `Double ] = `Double
let int_of_float_size = match float_size with `Single -> 4 | `Double -> 8
