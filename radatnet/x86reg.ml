
type t = 
        | Eax
        | Ax
        | Ah
        | Al
        | Ebx
        | Bx
        | Bh
        | Bl
        | Ecx
        | Cx
        | Ch
        | Cl
        | Edx
        | Dx
        | Dh
        | Dl
        | Esi
        | Si
        | Sil
        | Edi
        | Di
        | Dil
        | Esp
        | Sp
        | Spl
        | Ebp
        | Bp
        | Bpl
        | Eip
        | Ip
        | St0
        | St1
        | St2
        | St3
        | St4
        | St5
        | Mm0
        | Mm1
        | Mm2
        | Mm3
        | Mm4
        | Mm5
        | Mm6
        | Mm7
        | Xmm0
        | Xmm1
        | Xmm2
        | Xmm3
        | Xmm4
        | Xmm5
        | Xmm6
        | Xmm7
        [@@deriving yojson]


(*type _ t = 
        | Eax: [> `Reg32Bit] t
        | Ax: [> `Reg16Bit] t
        | Ah: [> `Reg8HighBit] t
        | Al: [> `Reg8LowBit] t
        | Ebx: [> `Reg32Bit] t
        | Bx: [> `Reg16Bit] t
        | Bh: [> `Reg8HighBit] t
        | Bl: [> `Reg8LowBit] t
        | Ecx: [> `Reg32Bit] t
        | Cx: [> `Reg16Bit] t
        | Ch: [> `Reg8HighBit] t
        | Cl: [> `Reg8LowBit] t
        | Edx: [> `Reg32Bit] t
        | Dx: [> `Reg16Bit] t
        | Dh: [> `Reg8HighBit] t
        | Dl: [> `Reg8LowBit] t
        | Esi: [> `Reg32Bit] t
        | Si: [> `Reg16Bit] t
        | Sil: [> `Reg8LowBit] t
        | Edi: [> `Reg32Bit] t
        | Di: [> `Reg16Bit] t
        | Dil: [> `Reg8LowBit] t
        | Esp: [> `Reg32Bit] t
        | Sp: [> `Reg16Bit] t
        | Spl: [> `Reg8LowBit] t
        | Ebp: [> `Reg32Bit] t
        | Bp: [> `Reg16Bit] t
        | Bpl: [> `Reg8LowBit] t
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

type ext_t = Reg: 'a t -> ext_t [@unboxed] *)


