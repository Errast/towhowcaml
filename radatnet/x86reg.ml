module JError = Ppx_yojson_conv_lib.Yojson_conv_error

type reg_32bit = [ `eax | `ebx | `ecx | `edx | `esi | `edi | `esp | `ebp | `eip ]
[@@deriving sexp]

let reg_32bit_of_yojson : Yojson.Safe.t -> reg_32bit = function
  | `String "eax" -> `eax
  | `String "ebx" -> `ebx
  | `String "ecx" -> `ecx
  | `String "edx" -> `edx
  | `String "esi" -> `esi
  | `String "edi" -> `edi
  | `String "esp" -> `esp
  | `String "ebp" -> `ebp
  | `String "eip" -> `eip
  | _ -> JError.no_variant_match ()
let __reg_32bit_of_yojson__ = reg_32bit_of_yojson

type reg_16bit = [ `ax | `bx | `cx | `dx | `si | `di | `sp | `bp | `ip ]
[@@deriving sexp]

let reg_16bit_of_yojson = function
  | `String "ax" -> `ax
  | `String "bx" -> `bx
  | `String "cx" -> `cx
  | `String "dx" -> `dx
  | `String "si" -> `si
  | `String "di" -> `di
  | `String "sp" -> `sp
  | `String "bp" -> `bp
  | `String "ip" -> `ip
  | _ -> JError.no_variant_match ()
let __reg_16bit_of_yojson__ = reg_16bit_of_yojson

type reg_high8bit = [ `ah | `bh | `ch | `dh ] [@@deriving sexp]

let reg_high8bit_of_yojson = function
  | `String "ah" -> `ah
  | `String "bh" -> `bh
  | `String "ch" -> `ch
  | `String "dh" -> `dh
  | _ -> JError.no_variant_match ()
let __reg_high8bit_of_yojson__ = reg_high8bit_of_yojson

type reg_low8bit = [ `al | `bl | `cl | `dl | `sil | `dil | `spl | `bpl ]
[@@deriving sexp]

let reg_low8bit_of_yojson = function
  | `String "al" -> `al
  | `String "bl" -> `bl
  | `String "cl" -> `cl
  | `String "dl" -> `dl
  | `String "sil" -> `sil
  | `String "dil" -> `dil
  | `String "spl" -> `spl
  | `String "bpl" -> `bpl
  | _ -> JError.no_variant_match ()
let __reg_low8bit_of_yojson__ = reg_low8bit_of_yojson

type x87_float = [ `st0 | `st1 | `st2 | `st3 | `st4 | `st5 ] [@@deriving sexp]

let x87_float_of_yojson = function
  | `String "st(0)" -> `st0
  | `String "st(1)" -> `st1
  | `String "st(2)" -> `st2
  | `String "st(3)" -> `st3
  | `String "st(4)" -> `st4
  | `String "st(5)" -> `st5
  | _ -> JError.no_variant_match ()
let __x87_float_of_yojson__ = x87_float_of_yojson

type mmx = [ `mm0 | `mm1 | `mm2 | `mm3 | `mm4 | `mm5 | `mm6 | `mm7 ]
[@@deriving sexp]

let mmx_of_yojson = function
  | `String "mm0" -> `mm0
  | `String "mm1" -> `mm1
  | `String "mm2" -> `mm2
  | `String "mm3" -> `mm3
  | `String "mm4" -> `mm4
  | `String "mm5" -> `mm5
  | `String "mm6" -> `mm6
  | `String "mm7" -> `mm7
  | _ -> JError.no_variant_match ()
let __mmx_of_yojson__ = mmx_of_yojson

type sse = [ `xmm0 | `xmm1 | `xmm2 | `xmm3 | `xmm4 | `xmm5 | `xmm6 | `xmm7 ]
[@@deriving sexp]

let sse_of_yojson = function
  | `String "xmm0" -> `xmm0
  | `String "xmm1" -> `xmm1
  | `String "xmm2" -> `xmm2
  | `String "xmm3" -> `xmm3
  | `String "xmm4" -> `xmm4
  | `String "xmm5" -> `xmm5
  | `String "xmm6" -> `xmm6
  | `String "xmm7" -> `xmm7
  | _ -> JError.no_variant_match ()
let __sse_of_yojson__ = sse_of_yojson

type t =
  [ reg_32bit | reg_16bit | reg_low8bit | reg_high8bit | x87_float | mmx | sse ]
[@@deriving of_yojson, sexp]

let to_32_bit (reg : [ reg_32bit | reg_16bit | reg_high8bit | reg_low8bit ]) =
  match reg with
  | `eax -> `eax
  | `ax -> `eax
  | `al -> `eax
  | `ah -> `eax
  | `ebx -> `ebx
  | `bx -> `ebx
  | `bl -> `ebx
  | `bh -> `ebx
  | `ecx -> `ecx
  | `cx -> `ecx
  | `cl -> `ecx
  | `ch -> `ecx
  | `edx -> `edx
  | `dx -> `edx
  | `dl -> `edx
  | `dh -> `edx
  | `esi -> `esi
  | `si -> `esi
  | `sil -> `esi
  | `edi -> `edi
  | `di -> `edi
  | `dil -> `edi
  | `esp -> `esp
  | `sp -> `esp
  | `spl -> `esp
  | `ebp -> `ebp
  | `bp -> `ebp
  | `bpl -> `ebp
  | `eip -> `eip
  | `ip -> `eip
