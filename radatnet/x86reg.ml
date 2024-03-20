type reg_32bit = [`Eax | `Ebx | `Ecx | `Edx | `Esi | `Edi | `Esp | `Ebp | `Eip] [@@deriving of_yojson, sexp]
type reg_16bit = [`Ax | `Bx | `Cx | `Dx | `Si | `Di | `Sp | `Bp | `Ip] [@@deriving of_yojson, sexp]
type reg_high8bit = [`Ah  | `Bh  | `Ch  | `Dh] [@@deriving of_yojson, sexp]
type reg_low8bit = [`Al | `Bl | `Cl | `Dl | `Sil | `Dil | `Spl | `Bpl] [@@deriving of_yojson, sexp]
type x87_float = [`St0 | `St1 | `St2 | `St3 | `St4 | `St5] [@@deriving of_yojson, sexp]
type mmx = [`Mm0 | `Mm1 | `Mm2 | `Mm3 | `Mm4 | `Mm5 | `Mm6 | `Mm7] [@@deriving of_yojson, sexp]
type sse = [`Xmm0 | `Xmm1 | `Xmm2 | `Xmm3 | `Xmm4 | `Xmm5 | `Xmm6 | `Xmm7] [@@deriving of_yojson, sexp]
type t = [reg_32bit | reg_16bit | reg_low8bit | reg_high8bit | x87_float | mmx | sse] [@@deriving of_yojson, sexp]
let to_32_bit (reg: [reg_32bit | reg_16bit | reg_high8bit | reg_low8bit]) = match reg with
        | `Eax -> `Eax
        | `Ax -> `Eax
        | `Al -> `Eax
        | `Ah -> `Eax
        | `Ebx -> `Ebx
        | `Bx -> `Ebx
        | `Bl -> `Ebx
        | `Bh -> `Ebx
        | `Ecx -> `Ecx
        | `Cx -> `Ecx
        | `Cl -> `Ecx
        | `Ch -> `Ecx
        | `Edx -> `Edx
        | `Dx -> `Edx
        | `Dl -> `Edx
        | `Dh -> `Edx
        | `Esi -> `Esi
        | `Si -> `Esi
        | `Sil -> `Esi
        | `Edi -> `Edi
        | `Di -> `Edi
        | `Dil -> `Edi
        | `Esp -> `Esp
        | `Sp -> `Esp
        | `Spl -> `Esp
        | `Ebp -> `Ebp
        | `Bp -> `Ebp
        | `Bpl -> `Ebp
        | `Eip -> `Eip
        | `Ip -> `Eip
