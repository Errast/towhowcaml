type _ t =
        | Eax: [> `Reg32Bit] t
        | Ax: [> `Reg16Bit] t
        | Ah: [> `RegHigh8Bit] t
        | Al: [> `RegLow8Bit] t
        | Ebx: [> `Reg32Bit] t
        | Bx: [> `Reg16Bit] t
        | Bh: [> `RegHigh8Bit] t
        | Bl: [> `RegLow8Bit] t
        | Ecx: [> `Reg32Bit] t
        | Cx: [> `Reg16Bit] t
        | Ch: [> `RegHigh8Bit] t
        | Cl: [> `RegLow8Bit] t
        | Edx: [> `Reg32Bit] t
        | Dx: [> `Reg16Bit] t
        | Dh: [> `RegHigh8Bit] t
        | Dl: [> `RegLow8Bit] t
        | Esi: [> `Reg32Bit] t
        | Si: [> `Reg16Bit] t
        | Sil: [> `RegLow8Bit] t
        | Edi: [> `Reg32Bit] t
        | Di: [> `Reg16Bit] t
        | Dil: [> `RegLow8Bit] t
        | Esp: [> `Reg32Bit] t
        | Sp: [> `Reg16Bit] t
        | Spl: [> `RegLow8Bit] t
        | Ebp: [> `Reg32Bit] t
        | Bp: [> `Reg16Bit] t
        | Bpl: [> `RegLow8Bit] t
        | Eip: [> `Reg32Bit] t
        | Ip: [> `Reg16Bit] t
        | St0: [> `FloatFpu] t
        | St1: [> `FloatFpu] t
        | St2: [> `FloatFpu] t
        | St3: [> `FloatFpu] t
        | St4: [> `FloatFpu] t
        | St5: [> `FloatFpu] t
        | Mm0: [> `Mmx] t
        | Mm1: [> `Mmx] t
        | Mm2: [> `Mmx] t
        | Mm3: [> `Mmx] t
        | Mm4: [> `Mmx] t
        | Mm5: [> `Mmx] t
        | Mm6: [> `Mmx] t
        | Mm7: [> `Mmx] t
        | Xmm0: [> `Sse] t
        | Xmm1: [> `Sse] t
        | Xmm2: [> `Sse] t
        | Xmm3: [> `Sse] t
        | Xmm4: [> `Sse] t
        | Xmm5: [> `Sse] t
        | Xmm6: [> `Sse] t
        | Xmm7: [> `Sse] t

type any_t = Reg: [< `Reg32Bit | `Reg16Bit | `RegHigh8Bit | `RegLow8Bit | `FloatFpu | `Mmx | `Sse ] t -> any_t [@@unboxed]

let any_t_of_yojson = function
        | `String "eax" -> Reg Eax
        | `String "ax" -> Reg Ax
        | `String "ah" -> Reg Ah
        | `String "al" -> Reg Al
        | `String "ebx" -> Reg Ebx
        | `String "bx" -> Reg Bx
        | `String "bh" -> Reg Bh
        | `String "bl" -> Reg Bl
        | `String "ecx" -> Reg Ecx
        | `String "cx" -> Reg Cx
        | `String "ch" -> Reg Ch
        | `String "cl" -> Reg Cl
        | `String "edx" -> Reg Edx
        | `String "dx" -> Reg Dx
        | `String "dh" -> Reg Dh
        | `String "dl" -> Reg Dl
        | `String "esi" -> Reg Esi
        | `String "si" -> Reg Si
        | `String "sil" -> Reg Sil
        | `String "edi" -> Reg Edi
        | `String "di" -> Reg Di
        | `String "dil" -> Reg Dil
        | `String "esp" -> Reg Esp
        | `String "sp" -> Reg Sp
        | `String "spl" -> Reg Spl
        | `String "ebp" -> Reg Ebp
        | `String "bp" -> Reg Bp
        | `String "bpl" -> Reg Bpl
        | `String "eip" -> Reg Eip
        | `String "ip" -> Reg Ip
        | `String "st(0)" -> Reg St0
        | `String "st(1)" -> Reg St1
        | `String "st(2)" -> Reg St2
        | `String "st(3)" -> Reg St3
        | `String "st(4)" -> Reg St4
        | `String "st(5)" -> Reg St5
        | `String "mm0" -> Reg Mm0
        | `String "mm1" -> Reg Mm1
        | `String "mm2" -> Reg Mm2
        | `String "mm3" -> Reg Mm3
        | `String "mm4" -> Reg Mm4
        | `String "mm5" -> Reg Mm5
        | `String "mm6" -> Reg Mm6
        | `String "mm7" -> Reg Mm7
        | `String "xmm0" -> Reg Xmm0
        | `String "xmm1" -> Reg Xmm1
        | `String "xmm2" -> Reg Xmm2
        | `String "xmm3" -> Reg Xmm3
        | `String "xmm4" -> Reg Xmm4
        | `String "xmm5" -> Reg Xmm5
        | `String "xmm6" -> Reg Xmm6
        | `String "xmm7" -> Reg Xmm7
        | yojson -> raise @@ Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure "invalid register", yojson)

let to_32_bit (reg: [< `Reg32Bit | `Reg16Bit | `RegHigh8Bit | `RegLow8Bit] t) = match reg with
        | Eax -> Eax
        | Ax -> Eax
        | Al -> Eax
        | Ah -> Eax
        | Ebx -> Ebx
        | Bx -> Ebx
        | Bl -> Ebx
        | Bh -> Ebx
        | Ecx -> Ecx
        | Cx -> Ecx
        | Cl -> Ecx
        | Ch -> Ecx
        | Edx -> Edx
        | Dx -> Edx
        | Dl -> Edx
        | Dh -> Edx
        | Esi -> Esi
        | Si -> Esi
        | Sil -> Esi
        | Edi -> Edi
        | Di -> Edi
        | Dil -> Edi
        | Esp -> Esp
        | Sp -> Esp
        | Spl -> Esp
        | Ebp -> Ebp
        | Bp -> Ebp
        | Bpl -> Ebp
        | Eip -> Eip
        | Ip -> Eip
  
