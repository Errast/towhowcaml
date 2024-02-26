type t = 
        | INVALID
        | AAA
        | AAD
        | AAM
        | AAS
        | FABS
        | ADC
        | ADCX
        | ADD
        | ADDPD
        | ADDPS
        | ADDSD
        | ADDSS
        | ADDSUBPD
        | ADDSUBPS
        | FADD
        | FIADD
        | FADDP
        | ADOX
        | AESDECLAST
        | AESDEC
        | AESENCLAST
        | AESENC
        | AESIMC
        | AESKEYGENASSIST
        | AND
        | ANDN
        | ANDNPD
        | ANDNPS
        | ANDPD
        | ANDPS
        | ARPL
        | BEXTR
        | BLCFILL
        | BLCI
        | BLCIC
        | BLCMSK
        | BLCS
        | BLENDPD
        | BLENDPS
        | BLENDVPD
        | BLENDVPS
        | BLSFILL
        | BLSI
        | BLSIC
        | BLSMSK
        | BLSR
        | BOUND
        | BSF
        | BSR
        | BSWAP
        | BT
        | BTC
        | BTR
        | BTS
        | BZHI
        | CALL
        | CBW
        | CDQ
        | CDQE
        | FCHS
        | CLAC
        | CLC
        | CLD
        | CLFLUSH
        | CLFLUSHOPT
        | CLGI
        | CLI
        | CLTS
        | CLWB
        | CMC
        | CMOVA
        | CMOVAE
        | CMOVB
        | CMOVBE
        | FCMOVBE
        | FCMOVB
        | CMOVE
        | FCMOVE
        | CMOVG
        | CMOVGE
        | CMOVL
        | CMOVLE
        | FCMOVNBE
        | FCMOVNB
        | CMOVNE
        | FCMOVNE
        | CMOVNO
        | CMOVNP
        | FCMOVNU
        | CMOVNS
        | CMOVO
        | CMOVP
        | FCMOVU
        | CMOVS
        | CMP
        | CMPSB
        | CMPSQ
        | CMPSW
        | CMPXCHG16B
        | CMPXCHG
        | CMPXCHG8B
        | COMISD
        | COMISS
        | FCOMP
        | FCOMIP
        | FCOMI
        | FCOM
        | FCOS
        | CPUID
        | CQO
        | CRC32
        | CVTDQ2PD
        | CVTDQ2PS
        | CVTPD2DQ
        | CVTPD2PS
        | CVTPS2DQ
        | CVTPS2PD
        | CVTSD2SI
        | CVTSD2SS
        | CVTSI2SD
        | CVTSI2SS
        | CVTSS2SD
        | CVTSS2SI
        | CVTTPD2DQ
        | CVTTPS2DQ
        | CVTTSD2SI
        | CVTTSS2SI
        | CWD
        | CWDE
        | DAA
        | DAS
        | DATA16
        | DEC
        | DIV
        | DIVPD
        | DIVPS
        | FDIVR
        | FIDIVR
        | FDIVRP
        | DIVSD
        | DIVSS
        | FDIV
        | FIDIV
        | FDIVP
        | DPPD
        | DPPS
        | RET
        | ENCLS
        | ENCLU
        | ENTER
        | EXTRACTPS
        | EXTRQ
        | F2XM1
        | LCALL
        | LJMP
        | FBLD
        | FBSTP
        | FCOMPP
        | FDECSTP
        | FEMMS
        | FFREE
        | FICOM
        | FICOMP
        | FINCSTP
        | FLDCW
        | FLDENV
        | FLDL2E
        | FLDL2T
        | FLDLG2
        | FLDLN2
        | FLDPI
        | FNCLEX
        | FNINIT
        | FNOP
        | FNSTCW
        | FNSTSW
        | FPATAN
        | FPREM
        | FPREM1
        | FPTAN
        | FFREEP
        | FRNDINT
        | FRSTOR
        | FNSAVE
        | FSCALE
        | FSETPM
        | FSINCOS
        | FNSTENV
        | FXAM
        | FXRSTOR
        | FXRSTOR64
        | FXSAVE
        | FXSAVE64
        | FXTRACT
        | FYL2X
        | FYL2XP1
        | MOVAPD
        | MOVAPS
        | ORPD
        | ORPS
        | VMOVAPD
        | VMOVAPS
        | XORPD
        | XORPS
        | GETSEC
        | HADDPD
        | HADDPS
        | HLT
        | HSUBPD
        | HSUBPS
        | IDIV
        | FILD
        | IMUL
        | IN
        | INC
        | INSB
        | INSERTPS
        | INSERTQ
        | INSD
        | INSW
        | INT
        | INT1
        | INT3
        | INTO
        | INVD
        | INVEPT
        | INVLPG
        | INVLPGA
        | INVPCID
        | INVVPID
        | IRET
        | IRETD
        | IRETQ
        | FISTTP
        | FIST
        | FISTP
        | UCOMISD
        | UCOMISS
        | VCOMISD
        | VCOMISS
        | VCVTSD2SS
        | VCVTSI2SD
        | VCVTSI2SS
        | VCVTSS2SD
        | VCVTTSD2SI
        | VCVTTSD2USI
        | VCVTTSS2SI
        | VCVTTSS2USI
        | VCVTUSI2SD
        | VCVTUSI2SS
        | VUCOMISD
        | VUCOMISS
        | JAE
        | JA
        | JBE
        | JB
        | JCXZ
        | JECXZ
        | JE
        | JGE
        | JG
        | JLE
        | JL
        | JMP
        | JNE
        | JNO
        | JNP
        | JNS
        | JO
        | JP
        | JRCXZ
        | JS
        | KANDB
        | KANDD
        | KANDNB
        | KANDND
        | KANDNQ
        | KANDNW
        | KANDQ
        | KANDW
        | KMOVB
        | KMOVD
        | KMOVQ
        | KMOVW
        | KNOTB
        | KNOTD
        | KNOTQ
        | KNOTW
        | KORB
        | KORD
        | KORQ
        | KORTESTB
        | KORTESTD
        | KORTESTQ
        | KORTESTW
        | KORW
        | KSHIFTLB
        | KSHIFTLD
        | KSHIFTLQ
        | KSHIFTLW
        | KSHIFTRB
        | KSHIFTRD
        | KSHIFTRQ
        | KSHIFTRW
        | KUNPCKBW
        | KXNORB
        | KXNORD
        | KXNORQ
        | KXNORW
        | KXORB
        | KXORD
        | KXORQ
        | KXORW
        | LAHF
        | LAR
        | LDDQU
        | LDMXCSR
        | LDS
        | FLDZ
        | FLD1
        | FLD
        | LEA
        | LEAVE
        | LES
        | LFENCE
        | LFS
        | LGDT
        | LGS
        | LIDT
        | LLDT
        | LMSW
        | OR
        | SUB
        | XOR
        | LODSB
        | LODSD
        | LODSQ
        | LODSW
        | LOOP
        | LOOPE
        | LOOPNE
        | RETF
        | RETFQ
        | LSL
        | LSS
        | LTR
        | XADD
        | LZCNT
        | MASKMOVDQU
        | MAXPD
        | MAXPS
        | MAXSD
        | MAXSS
        | MFENCE
        | MINPD
        | MINPS
        | MINSD
        | MINSS
        | CVTPD2PI
        | CVTPI2PD
        | CVTPI2PS
        | CVTPS2PI
        | CVTTPD2PI
        | CVTTPS2PI
        | EMMS
        | MASKMOVQ
        | MOVD
        | MOVDQ2Q
        | MOVNTQ
        | MOVQ2DQ
        | MOVQ
        | PABSB
        | PABSD
        | PABSW
        | PACKSSDW
        | PACKSSWB
        | PACKUSWB
        | PADDB
        | PADDD
        | PADDQ
        | PADDSB
        | PADDSW
        | PADDUSB
        | PADDUSW
        | PADDW
        | PALIGNR
        | PANDN
        | PAND
        | PAVGB
        | PAVGW
        | PCMPEQB
        | PCMPEQD
        | PCMPEQW
        | PCMPGTB
        | PCMPGTD
        | PCMPGTW
        | PEXTRW
        | PHADDSW
        | PHADDW
        | PHADDD
        | PHSUBD
        | PHSUBSW
        | PHSUBW
        | PINSRW
        | PMADDUBSW
        | PMADDWD
        | PMAXSW
        | PMAXUB
        | PMINSW
        | PMINUB
        | PMOVMSKB
        | PMULHRSW
        | PMULHUW
        | PMULHW
        | PMULLW
        | PMULUDQ
        | POR
        | PSADBW
        | PSHUFB
        | PSHUFW
        | PSIGNB
        | PSIGND
        | PSIGNW
        | PSLLD
        | PSLLQ
        | PSLLW
        | PSRAD
        | PSRAW
        | PSRLD
        | PSRLQ
        | PSRLW
        | PSUBB
        | PSUBD
        | PSUBQ
        | PSUBSB
        | PSUBSW
        | PSUBUSB
        | PSUBUSW
        | PSUBW
        | PUNPCKHBW
        | PUNPCKHDQ
        | PUNPCKHWD
        | PUNPCKLBW
        | PUNPCKLDQ
        | PUNPCKLWD
        | PXOR
        | MONITOR
        | MONTMUL
        | MOV
        | MOVABS
        | MOVBE
        | MOVDDUP
        | MOVDQA
        | MOVDQU
        | MOVHLPS
        | MOVHPD
        | MOVHPS
        | MOVLHPS
        | MOVLPD
        | MOVLPS
        | MOVMSKPD
        | MOVMSKPS
        | MOVNTDQA
        | MOVNTDQ
        | MOVNTI
        | MOVNTPD
        | MOVNTPS
        | MOVNTSD
        | MOVNTSS
        | MOVSB
        | MOVSD
        | MOVSHDUP
        | MOVSLDUP
        | MOVSQ
        | MOVSS
        | MOVSW
        | MOVSX
        | MOVSXD
        | MOVUPD
        | MOVUPS
        | MOVZX
        | MPSADBW
        | MUL
        | MULPD
        | MULPS
        | MULSD
        | MULSS
        | MULX
        | FMUL
        | FIMUL
        | FMULP
        | MWAIT
        | NEG
        | NOP
        | NOT
        | OUT
        | OUTSB
        | OUTSD
        | OUTSW
        | PACKUSDW
        | PAUSE
        | PAVGUSB
        | PBLENDVB
        | PBLENDW
        | PCLMULQDQ
        | PCMPEQQ
        | PCMPESTRI
        | PCMPESTRM
        | PCMPGTQ
        | PCMPISTRI
        | PCMPISTRM
        | PCOMMIT
        | PDEP
        | PEXT
        | PEXTRB
        | PEXTRD
        | PEXTRQ
        | PF2ID
        | PF2IW
        | PFACC
        | PFADD
        | PFCMPEQ
        | PFCMPGE
        | PFCMPGT
        | PFMAX
        | PFMIN
        | PFMUL
        | PFNACC
        | PFPNACC
        | PFRCPIT1
        | PFRCPIT2
        | PFRCP
        | PFRSQIT1
        | PFRSQRT
        | PFSUBR
        | PFSUB
        | PHMINPOSUW
        | PI2FD
        | PI2FW
        | PINSRB
        | PINSRD
        | PINSRQ
        | PMAXSB
        | PMAXSD
        | PMAXUD
        | PMAXUW
        | PMINSB
        | PMINSD
        | PMINUD
        | PMINUW
        | PMOVSXBD
        | PMOVSXBQ
        | PMOVSXBW
        | PMOVSXDQ
        | PMOVSXWD
        | PMOVSXWQ
        | PMOVZXBD
        | PMOVZXBQ
        | PMOVZXBW
        | PMOVZXDQ
        | PMOVZXWD
        | PMOVZXWQ
        | PMULDQ
        | PMULHRW
        | PMULLD
        | POP
        | POPAW
        | POPAL
        | POPCNT
        | POPF
        | POPFD
        | POPFQ
        | PREFETCH
        | PREFETCHNTA
        | PREFETCHT0
        | PREFETCHT1
        | PREFETCHT2
        | PREFETCHW
        | PSHUFD
        | PSHUFHW
        | PSHUFLW
        | PSLLDQ
        | PSRLDQ
        | PSWAPD
        | PTEST
        | PUNPCKHQDQ
        | PUNPCKLQDQ
        | PUSH
        | PUSHAW
        | PUSHAL
        | PUSHF
        | PUSHFD
        | PUSHFQ
        | RCL
        | RCPPS
        | RCPSS
        | RCR
        | RDFSBASE
        | RDGSBASE
        | RDMSR
        | RDPMC
        | RDRAND
        | RDSEED
        | RDTSC
        | RDTSCP
        | ROL
        | ROR
        | RORX
        | ROUNDPD
        | ROUNDPS
        | ROUNDSD
        | ROUNDSS
        | RSM
        | RSQRTPS
        | RSQRTSS
        | SAHF
        | SAL
        | SALC
        | SAR
        | SARX
        | SBB
        | SCASB
        | SCASD
        | SCASQ
        | SCASW
        | SETAE
        | SETA
        | SETBE
        | SETB
        | SETE
        | SETGE
        | SETG
        | SETLE
        | SETL
        | SETNE
        | SETNO
        | SETNP
        | SETNS
        | SETO
        | SETP
        | SETS
        | SFENCE
        | SGDT
        | SHA1MSG1
        | SHA1MSG2
        | SHA1NEXTE
        | SHA1RNDS4
        | SHA256MSG1
        | SHA256MSG2
        | SHA256RNDS2
        | SHL
        | SHLD
        | SHLX
        | SHR
        | SHRD
        | SHRX
        | SHUFPD
        | SHUFPS
        | SIDT
        | FSIN
        | SKINIT
        | SLDT
        | SMSW
        | SQRTPD
        | SQRTPS
        | SQRTSD
        | SQRTSS
        | FSQRT
        | STAC
        | STC
        | STD
        | STGI
        | STI
        | STMXCSR
        | STOSB
        | STOSD
        | STOSQ
        | STOSW
        | STR
        | FST
        | FSTP
        | FSTPNCE
        | FXCH
        | SUBPD
        | SUBPS
        | FSUBR
        | FISUBR
        | FSUBRP
        | SUBSD
        | SUBSS
        | FSUB
        | FISUB
        | FSUBP
        | SWAPGS
        | SYSCALL
        | SYSENTER
        | SYSEXIT
        | SYSRET
        | T1MSKC
        | TEST
        | UD2
        | FTST
        | TZCNT
        | TZMSK
        | FUCOMIP
        | FUCOMI
        | FUCOMPP
        | FUCOMP
        | FUCOM
        | UD2B
        | UNPCKHPD
        | UNPCKHPS
        | UNPCKLPD
        | UNPCKLPS
        | VADDPD
        | VADDPS
        | VADDSD
        | VADDSS
        | VADDSUBPD
        | VADDSUBPS
        | VAESDECLAST
        | VAESDEC
        | VAESENCLAST
        | VAESENC
        | VAESIMC
        | VAESKEYGENASSIST
        | VALIGND
        | VALIGNQ
        | VANDNPD
        | VANDNPS
        | VANDPD
        | VANDPS
        | VBLENDMPD
        | VBLENDMPS
        | VBLENDPD
        | VBLENDPS
        | VBLENDVPD
        | VBLENDVPS
        | VBROADCASTF128
        | VBROADCASTI32X4
        | VBROADCASTI64X4
        | VBROADCASTSD
        | VBROADCASTSS
        | VCOMPRESSPD
        | VCOMPRESSPS
        | VCVTDQ2PD
        | VCVTDQ2PS
        | VCVTPD2DQX
        | VCVTPD2DQ
        | VCVTPD2PSX
        | VCVTPD2PS
        | VCVTPD2UDQ
        | VCVTPH2PS
        | VCVTPS2DQ
        | VCVTPS2PD
        | VCVTPS2PH
        | VCVTPS2UDQ
        | VCVTSD2SI
        | VCVTSD2USI
        | VCVTSS2SI
        | VCVTSS2USI
        | VCVTTPD2DQX
        | VCVTTPD2DQ
        | VCVTTPD2UDQ
        | VCVTTPS2DQ
        | VCVTTPS2UDQ
        | VCVTUDQ2PD
        | VCVTUDQ2PS
        | VDIVPD
        | VDIVPS
        | VDIVSD
        | VDIVSS
        | VDPPD
        | VDPPS
        | VERR
        | VERW
        | VEXP2PD
        | VEXP2PS
        | VEXPANDPD
        | VEXPANDPS
        | VEXTRACTF128
        | VEXTRACTF32X4
        | VEXTRACTF64X4
        | VEXTRACTI128
        | VEXTRACTI32X4
        | VEXTRACTI64X4
        | VEXTRACTPS
        | VFMADD132PD
        | VFMADD132PS
        | VFMADDPD
        | VFMADD213PD
        | VFMADD231PD
        | VFMADDPS
        | VFMADD213PS
        | VFMADD231PS
        | VFMADDSD
        | VFMADD213SD
        | VFMADD132SD
        | VFMADD231SD
        | VFMADDSS
        | VFMADD213SS
        | VFMADD132SS
        | VFMADD231SS
        | VFMADDSUB132PD
        | VFMADDSUB132PS
        | VFMADDSUBPD
        | VFMADDSUB213PD
        | VFMADDSUB231PD
        | VFMADDSUBPS
        | VFMADDSUB213PS
        | VFMADDSUB231PS
        | VFMSUB132PD
        | VFMSUB132PS
        | VFMSUBADD132PD
        | VFMSUBADD132PS
        | VFMSUBADDPD
        | VFMSUBADD213PD
        | VFMSUBADD231PD
        | VFMSUBADDPS
        | VFMSUBADD213PS
        | VFMSUBADD231PS
        | VFMSUBPD
        | VFMSUB213PD
        | VFMSUB231PD
        | VFMSUBPS
        | VFMSUB213PS
        | VFMSUB231PS
        | VFMSUBSD
        | VFMSUB213SD
        | VFMSUB132SD
        | VFMSUB231SD
        | VFMSUBSS
        | VFMSUB213SS
        | VFMSUB132SS
        | VFMSUB231SS
        | VFNMADD132PD
        | VFNMADD132PS
        | VFNMADDPD
        | VFNMADD213PD
        | VFNMADD231PD
        | VFNMADDPS
        | VFNMADD213PS
        | VFNMADD231PS
        | VFNMADDSD
        | VFNMADD213SD
        | VFNMADD132SD
        | VFNMADD231SD
        | VFNMADDSS
        | VFNMADD213SS
        | VFNMADD132SS
        | VFNMADD231SS
        | VFNMSUB132PD
        | VFNMSUB132PS
        | VFNMSUBPD
        | VFNMSUB213PD
        | VFNMSUB231PD
        | VFNMSUBPS
        | VFNMSUB213PS
        | VFNMSUB231PS
        | VFNMSUBSD
        | VFNMSUB213SD
        | VFNMSUB132SD
        | VFNMSUB231SD
        | VFNMSUBSS
        | VFNMSUB213SS
        | VFNMSUB132SS
        | VFNMSUB231SS
        | VFRCZPD
        | VFRCZPS
        | VFRCZSD
        | VFRCZSS
        | VORPD
        | VORPS
        | VXORPD
        | VXORPS
        | VGATHERDPD
        | VGATHERDPS
        | VGATHERPF0DPD
        | VGATHERPF0DPS
        | VGATHERPF0QPD
        | VGATHERPF0QPS
        | VGATHERPF1DPD
        | VGATHERPF1DPS
        | VGATHERPF1QPD
        | VGATHERPF1QPS
        | VGATHERQPD
        | VGATHERQPS
        | VHADDPD
        | VHADDPS
        | VHSUBPD
        | VHSUBPS
        | VINSERTF128
        | VINSERTF32X4
        | VINSERTF32X8
        | VINSERTF64X2
        | VINSERTF64X4
        | VINSERTI128
        | VINSERTI32X4
        | VINSERTI32X8
        | VINSERTI64X2
        | VINSERTI64X4
        | VINSERTPS
        | VLDDQU
        | VLDMXCSR
        | VMASKMOVDQU
        | VMASKMOVPD
        | VMASKMOVPS
        | VMAXPD
        | VMAXPS
        | VMAXSD
        | VMAXSS
        | VMCALL
        | VMCLEAR
        | VMFUNC
        | VMINPD
        | VMINPS
        | VMINSD
        | VMINSS
        | VMLAUNCH
        | VMLOAD
        | VMMCALL
        | VMOVQ
        | VMOVDDUP
        | VMOVD
        | VMOVDQA32
        | VMOVDQA64
        | VMOVDQA
        | VMOVDQU16
        | VMOVDQU32
        | VMOVDQU64
        | VMOVDQU8
        | VMOVDQU
        | VMOVHLPS
        | VMOVHPD
        | VMOVHPS
        | VMOVLHPS
        | VMOVLPD
        | VMOVLPS
        | VMOVMSKPD
        | VMOVMSKPS
        | VMOVNTDQA
        | VMOVNTDQ
        | VMOVNTPD
        | VMOVNTPS
        | VMOVSD
        | VMOVSHDUP
        | VMOVSLDUP
        | VMOVSS
        | VMOVUPD
        | VMOVUPS
        | VMPSADBW
        | VMPTRLD
        | VMPTRST
        | VMREAD
        | VMRESUME
        | VMRUN
        | VMSAVE
        | VMULPD
        | VMULPS
        | VMULSD
        | VMULSS
        | VMWRITE
        | VMXOFF
        | VMXON
        | VPABSB
        | VPABSD
        | VPABSQ
        | VPABSW
        | VPACKSSDW
        | VPACKSSWB
        | VPACKUSDW
        | VPACKUSWB
        | VPADDB
        | VPADDD
        | VPADDQ
        | VPADDSB
        | VPADDSW
        | VPADDUSB
        | VPADDUSW
        | VPADDW
        | VPALIGNR
        | VPANDD
        | VPANDND
        | VPANDNQ
        | VPANDN
        | VPANDQ
        | VPAND
        | VPAVGB
        | VPAVGW
        | VPBLENDD
        | VPBLENDMB
        | VPBLENDMD
        | VPBLENDMQ
        | VPBLENDMW
        | VPBLENDVB
        | VPBLENDW
        | VPBROADCASTB
        | VPBROADCASTD
        | VPBROADCASTMB2Q
        | VPBROADCASTMW2D
        | VPBROADCASTQ
        | VPBROADCASTW
        | VPCLMULQDQ
        | VPCMOV
        | VPCMPB
        | VPCMPD
        | VPCMPEQB
        | VPCMPEQD
        | VPCMPEQQ
        | VPCMPEQW
        | VPCMPESTRI
        | VPCMPESTRM
        | VPCMPGTB
        | VPCMPGTD
        | VPCMPGTQ
        | VPCMPGTW
        | VPCMPISTRI
        | VPCMPISTRM
        | VPCMPQ
        | VPCMPUB
        | VPCMPUD
        | VPCMPUQ
        | VPCMPUW
        | VPCMPW
        | VPCOMB
        | VPCOMD
        | VPCOMPRESSD
        | VPCOMPRESSQ
        | VPCOMQ
        | VPCOMUB
        | VPCOMUD
        | VPCOMUQ
        | VPCOMUW
        | VPCOMW
        | VPCONFLICTD
        | VPCONFLICTQ
        | VPERM2F128
        | VPERM2I128
        | VPERMD
        | VPERMI2D
        | VPERMI2PD
        | VPERMI2PS
        | VPERMI2Q
        | VPERMIL2PD
        | VPERMIL2PS
        | VPERMILPD
        | VPERMILPS
        | VPERMPD
        | VPERMPS
        | VPERMQ
        | VPERMT2D
        | VPERMT2PD
        | VPERMT2PS
        | VPERMT2Q
        | VPEXPANDD
        | VPEXPANDQ
        | VPEXTRB
        | VPEXTRD
        | VPEXTRQ
        | VPEXTRW
        | VPGATHERDD
        | VPGATHERDQ
        | VPGATHERQD
        | VPGATHERQQ
        | VPHADDBD
        | VPHADDBQ
        | VPHADDBW
        | VPHADDDQ
        | VPHADDD
        | VPHADDSW
        | VPHADDUBD
        | VPHADDUBQ
        | VPHADDUBW
        | VPHADDUDQ
        | VPHADDUWD
        | VPHADDUWQ
        | VPHADDWD
        | VPHADDWQ
        | VPHADDW
        | VPHMINPOSUW
        | VPHSUBBW
        | VPHSUBDQ
        | VPHSUBD
        | VPHSUBSW
        | VPHSUBWD
        | VPHSUBW
        | VPINSRB
        | VPINSRD
        | VPINSRQ
        | VPINSRW
        | VPLZCNTD
        | VPLZCNTQ
        | VPMACSDD
        | VPMACSDQH
        | VPMACSDQL
        | VPMACSSDD
        | VPMACSSDQH
        | VPMACSSDQL
        | VPMACSSWD
        | VPMACSSWW
        | VPMACSWD
        | VPMACSWW
        | VPMADCSSWD
        | VPMADCSWD
        | VPMADDUBSW
        | VPMADDWD
        | VPMASKMOVD
        | VPMASKMOVQ
        | VPMAXSB
        | VPMAXSD
        | VPMAXSQ
        | VPMAXSW
        | VPMAXUB
        | VPMAXUD
        | VPMAXUQ
        | VPMAXUW
        | VPMINSB
        | VPMINSD
        | VPMINSQ
        | VPMINSW
        | VPMINUB
        | VPMINUD
        | VPMINUQ
        | VPMINUW
        | VPMOVDB
        | VPMOVDW
        | VPMOVM2B
        | VPMOVM2D
        | VPMOVM2Q
        | VPMOVM2W
        | VPMOVMSKB
        | VPMOVQB
        | VPMOVQD
        | VPMOVQW
        | VPMOVSDB
        | VPMOVSDW
        | VPMOVSQB
        | VPMOVSQD
        | VPMOVSQW
        | VPMOVSXBD
        | VPMOVSXBQ
        | VPMOVSXBW
        | VPMOVSXDQ
        | VPMOVSXWD
        | VPMOVSXWQ
        | VPMOVUSDB
        | VPMOVUSDW
        | VPMOVUSQB
        | VPMOVUSQD
        | VPMOVUSQW
        | VPMOVZXBD
        | VPMOVZXBQ
        | VPMOVZXBW
        | VPMOVZXDQ
        | VPMOVZXWD
        | VPMOVZXWQ
        | VPMULDQ
        | VPMULHRSW
        | VPMULHUW
        | VPMULHW
        | VPMULLD
        | VPMULLQ
        | VPMULLW
        | VPMULUDQ
        | VPORD
        | VPORQ
        | VPOR
        | VPPERM
        | VPROTB
        | VPROTD
        | VPROTQ
        | VPROTW
        | VPSADBW
        | VPSCATTERDD
        | VPSCATTERDQ
        | VPSCATTERQD
        | VPSCATTERQQ
        | VPSHAB
        | VPSHAD
        | VPSHAQ
        | VPSHAW
        | VPSHLB
        | VPSHLD
        | VPSHLQ
        | VPSHLW
        | VPSHUFB
        | VPSHUFD
        | VPSHUFHW
        | VPSHUFLW
        | VPSIGNB
        | VPSIGND
        | VPSIGNW
        | VPSLLDQ
        | VPSLLD
        | VPSLLQ
        | VPSLLVD
        | VPSLLVQ
        | VPSLLW
        | VPSRAD
        | VPSRAQ
        | VPSRAVD
        | VPSRAVQ
        | VPSRAW
        | VPSRLDQ
        | VPSRLD
        | VPSRLQ
        | VPSRLVD
        | VPSRLVQ
        | VPSRLW
        | VPSUBB
        | VPSUBD
        | VPSUBQ
        | VPSUBSB
        | VPSUBSW
        | VPSUBUSB
        | VPSUBUSW
        | VPSUBW
        | VPTESTMD
        | VPTESTMQ
        | VPTESTNMD
        | VPTESTNMQ
        | VPTEST
        | VPUNPCKHBW
        | VPUNPCKHDQ
        | VPUNPCKHQDQ
        | VPUNPCKHWD
        | VPUNPCKLBW
        | VPUNPCKLDQ
        | VPUNPCKLQDQ
        | VPUNPCKLWD
        | VPXORD
        | VPXORQ
        | VPXOR
        | VRCP14PD
        | VRCP14PS
        | VRCP14SD
        | VRCP14SS
        | VRCP28PD
        | VRCP28PS
        | VRCP28SD
        | VRCP28SS
        | VRCPPS
        | VRCPSS
        | VRNDSCALEPD
        | VRNDSCALEPS
        | VRNDSCALESD
        | VRNDSCALESS
        | VROUNDPD
        | VROUNDPS
        | VROUNDSD
        | VROUNDSS
        | VRSQRT14PD
        | VRSQRT14PS
        | VRSQRT14SD
        | VRSQRT14SS
        | VRSQRT28PD
        | VRSQRT28PS
        | VRSQRT28SD
        | VRSQRT28SS
        | VRSQRTPS
        | VRSQRTSS
        | VSCATTERDPD
        | VSCATTERDPS
        | VSCATTERPF0DPD
        | VSCATTERPF0DPS
        | VSCATTERPF0QPD
        | VSCATTERPF0QPS
        | VSCATTERPF1DPD
        | VSCATTERPF1DPS
        | VSCATTERPF1QPD
        | VSCATTERPF1QPS
        | VSCATTERQPD
        | VSCATTERQPS
        | VSHUFPD
        | VSHUFPS
        | VSQRTPD
        | VSQRTPS
        | VSQRTSD
        | VSQRTSS
        | VSTMXCSR
        | VSUBPD
        | VSUBPS
        | VSUBSD
        | VSUBSS
        | VTESTPD
        | VTESTPS
        | VUNPCKHPD
        | VUNPCKHPS
        | VUNPCKLPD
        | VUNPCKLPS
        | VZEROALL
        | VZEROUPPER
        | WAIT
        | WBINVD
        | WRFSBASE
        | WRGSBASE
        | WRMSR
        | XABORT
        | XACQUIRE
        | XBEGIN
        | XCHG
        | XCRYPTCBC
        | XCRYPTCFB
        | XCRYPTCTR
        | XCRYPTECB
        | XCRYPTOFB
        | XEND
        | XGETBV
        | XLATB
        | XRELEASE
        | XRSTOR
        | XRSTOR64
        | XRSTORS
        | XRSTORS64
        | XSAVE
        | XSAVE64
        | XSAVEC
        | XSAVEC64
        | XSAVEOPT
        | XSAVEOPT64
        | XSAVES
        | 	XSAVES64
        | XSETBV
        | XSHA1
        | XSHA256
        | XSTORE
        | XTEST
        | FDISI8087_NOP
        | FENI8087_NOP
        | CMPSS
        | CMPEQSS
        | CMPLTSS
        | CMPLESS
        | CMPUNORDSS
        | CMPNEQSS
        | CMPNLTSS
        | CMPNLESS
        | CMPORDSS
        | CMPSD
        | CMPEQSD
        | CMPLTSD
        | CMPLESD
        | CMPUNORDSD
        | CMPNEQSD
        | CMPNLTSD
        | CMPNLESD
        | CMPORDSD
        | CMPPS
        | CMPEQPS
        | CMPLTPS
        | CMPLEPS
        | CMPUNORDPS
        | CMPNEQPS
        | CMPNLTPS
        | CMPNLEPS
        | CMPORDPS
        | CMPPD
        | CMPEQPD
        | CMPLTPD
        | CMPLEPD
        | CMPUNORDPD
        | CMPNEQPD
        | CMPNLTPD
        | CMPNLEPD
        | CMPORDPD
        | VCMPSS
        | VCMPEQSS
        | VCMPLTSS
        | VCMPLESS
        | VCMPUNORDSS
        | VCMPNEQSS
        | VCMPNLTSS
        | VCMPNLESS
        | VCMPORDSS
        | VCMPEQ_UQSS
        | VCMPNGESS
        | VCMPNGTSS
        | VCMPFALSESS
        | VCMPNEQ_OQSS
        | VCMPGESS
        | VCMPGTSS
        | VCMPTRUESS
        | VCMPEQ_OSSS
        | VCMPLT_OQSS
        | VCMPLE_OQSS
        | VCMPUNORD_SSS
        | VCMPNEQ_USSS
        | VCMPNLT_UQSS
        | VCMPNLE_UQSS
        | VCMPORD_SSS
        | VCMPEQ_USSS
        | VCMPNGE_UQSS
        | VCMPNGT_UQSS
        | VCMPFALSE_OSSS
        | VCMPNEQ_OSSS
        | VCMPGE_OQSS
        | VCMPGT_OQSS
        | VCMPTRUE_USSS
        | VCMPSD
        | VCMPEQSD
        | VCMPLTSD
        | VCMPLESD
        | VCMPUNORDSD
        | VCMPNEQSD
        | VCMPNLTSD
        | VCMPNLESD
        | VCMPORDSD
        | VCMPEQ_UQSD
        | VCMPNGESD
        | VCMPNGTSD
        | VCMPFALSESD
        | VCMPNEQ_OQSD
        | VCMPGESD
        | VCMPGTSD
        | VCMPTRUESD
        | VCMPEQ_OSSD
        | VCMPLT_OQSD
        | VCMPLE_OQSD
        | VCMPUNORD_SSD
        | VCMPNEQ_USSD
        | VCMPNLT_UQSD
        | VCMPNLE_UQSD
        | VCMPORD_SSD
        | VCMPEQ_USSD
        | VCMPNGE_UQSD
        | VCMPNGT_UQSD
        | VCMPFALSE_OSSD
        | VCMPNEQ_OSSD
        | VCMPGE_OQSD
        | VCMPGT_OQSD
        | VCMPTRUE_USSD
        | VCMPPS
        | VCMPEQPS
        | VCMPLTPS
        | VCMPLEPS
        | VCMPUNORDPS
        | VCMPNEQPS
        | VCMPNLTPS
        | VCMPNLEPS
        | VCMPORDPS
        | VCMPEQ_UQPS
        | VCMPNGEPS
        | VCMPNGTPS
        | VCMPFALSEPS
        | VCMPNEQ_OQPS
        | VCMPGEPS
        | VCMPGTPS
        | VCMPTRUEPS
        | VCMPEQ_OSPS
        | VCMPLT_OQPS
        | VCMPLE_OQPS
        | VCMPUNORD_SPS
        | VCMPNEQ_USPS
        | VCMPNLT_UQPS
        | VCMPNLE_UQPS
        | VCMPORD_SPS
        | VCMPEQ_USPS
        | VCMPNGE_UQPS
        | VCMPNGT_UQPS
        | VCMPFALSE_OSPS
        | VCMPNEQ_OSPS
        | VCMPGE_OQPS
        | VCMPGT_OQPS
        | VCMPTRUE_USPS
        | VCMPPD
        | VCMPEQPD
        | VCMPLTPD
        | VCMPLEPD
        | VCMPUNORDPD
        | VCMPNEQPD
        | VCMPNLTPD
        | VCMPNLEPD
        | VCMPORDPD
        | VCMPEQ_UQPD
        | VCMPNGEPD
        | VCMPNGTPD
        | VCMPFALSEPD
        | VCMPNEQ_OQPD
        | VCMPGEPD
        | VCMPGTPD
        | VCMPTRUEPD
        | VCMPEQ_OSPD
        | VCMPLT_OQPD
        | VCMPLE_OQPD
        | VCMPUNORD_SPD
        | VCMPNEQ_USPD
        | VCMPNLT_UQPD
        | VCMPNLE_UQPD
        | VCMPORD_SPD
        | VCMPEQ_USPD
        | VCMPNGE_UQPD
        | VCMPNGT_UQPD
        | VCMPFALSE_OSPD
        | VCMPNEQ_OSPD
        | VCMPGE_OQPD
        | VCMPGT_OQPD
        | VCMPTRUE_USPD
        | UD0
        | ENDBR32
        | ENDBR64
        [@@deriving sexp]



let t_of_yojson = function
        | `Int 0 -> INVALID
        | `Int 1 -> AAA
        | `Int 2 -> AAD
        | `Int 3 -> AAM
        | `Int 4 -> AAS
        | `Int 5 -> FABS
        | `Int 6 -> ADC
        | `Int 7 -> ADCX
        | `Int 8 -> ADD
        | `Int 9 -> ADDPD
        | `Int 10 -> ADDPS
        | `Int 11 -> ADDSD
        | `Int 12 -> ADDSS
        | `Int 13 -> ADDSUBPD
        | `Int 14 -> ADDSUBPS
        | `Int 15 -> FADD
        | `Int 16 -> FIADD
        | `Int 17 -> FADDP
        | `Int 18 -> ADOX
        | `Int 19 -> AESDECLAST
        | `Int 20 -> AESDEC
        | `Int 21 -> AESENCLAST
        | `Int 22 -> AESENC
        | `Int 23 -> AESIMC
        | `Int 24 -> AESKEYGENASSIST
        | `Int 25 -> AND
        | `Int 26 -> ANDN
        | `Int 27 -> ANDNPD
        | `Int 28 -> ANDNPS
        | `Int 29 -> ANDPD
        | `Int 30 -> ANDPS
        | `Int 31 -> ARPL
        | `Int 32 -> BEXTR
        | `Int 33 -> BLCFILL
        | `Int 34 -> BLCI
        | `Int 35 -> BLCIC
        | `Int 36 -> BLCMSK
        | `Int 37 -> BLCS
        | `Int 38 -> BLENDPD
        | `Int 39 -> BLENDPS
        | `Int 40 -> BLENDVPD
        | `Int 41 -> BLENDVPS
        | `Int 42 -> BLSFILL
        | `Int 43 -> BLSI
        | `Int 44 -> BLSIC
        | `Int 45 -> BLSMSK
        | `Int 46 -> BLSR
        | `Int 47 -> BOUND
        | `Int 48 -> BSF
        | `Int 49 -> BSR
        | `Int 50 -> BSWAP
        | `Int 51 -> BT
        | `Int 52 -> BTC
        | `Int 53 -> BTR
        | `Int 54 -> BTS
        | `Int 55 -> BZHI
        | `Int 56 -> CALL
        | `Int 57 -> CBW
        | `Int 58 -> CDQ
        | `Int 59 -> CDQE
        | `Int 60 -> FCHS
        | `Int 61 -> CLAC
        | `Int 62 -> CLC
        | `Int 63 -> CLD
        | `Int 64 -> CLFLUSH
        | `Int 65 -> CLFLUSHOPT
        | `Int 66 -> CLGI
        | `Int 67 -> CLI
        | `Int 68 -> CLTS
        | `Int 69 -> CLWB
        | `Int 70 -> CMC
        | `Int 71 -> CMOVA
        | `Int 72 -> CMOVAE
        | `Int 73 -> CMOVB
        | `Int 74 -> CMOVBE
        | `Int 75 -> FCMOVBE
        | `Int 76 -> FCMOVB
        | `Int 77 -> CMOVE
        | `Int 78 -> FCMOVE
        | `Int 79 -> CMOVG
        | `Int 80 -> CMOVGE
        | `Int 81 -> CMOVL
        | `Int 82 -> CMOVLE
        | `Int 83 -> FCMOVNBE
        | `Int 84 -> FCMOVNB
        | `Int 85 -> CMOVNE
        | `Int 86 -> FCMOVNE
        | `Int 87 -> CMOVNO
        | `Int 88 -> CMOVNP
        | `Int 89 -> FCMOVNU
        | `Int 90 -> CMOVNS
        | `Int 91 -> CMOVO
        | `Int 92 -> CMOVP
        | `Int 93 -> FCMOVU
        | `Int 94 -> CMOVS
        | `Int 95 -> CMP
        | `Int 96 -> CMPSB
        | `Int 97 -> CMPSQ
        | `Int 98 -> CMPSW
        | `Int 99 -> CMPXCHG16B
        | `Int 100 -> CMPXCHG
        | `Int 101 -> CMPXCHG8B
        | `Int 102 -> COMISD
        | `Int 103 -> COMISS
        | `Int 104 -> FCOMP
        | `Int 105 -> FCOMIP
        | `Int 106 -> FCOMI
        | `Int 107 -> FCOM
        | `Int 108 -> FCOS
        | `Int 109 -> CPUID
        | `Int 110 -> CQO
        | `Int 111 -> CRC32
        | `Int 112 -> CVTDQ2PD
        | `Int 113 -> CVTDQ2PS
        | `Int 114 -> CVTPD2DQ
        | `Int 115 -> CVTPD2PS
        | `Int 116 -> CVTPS2DQ
        | `Int 117 -> CVTPS2PD
        | `Int 118 -> CVTSD2SI
        | `Int 119 -> CVTSD2SS
        | `Int 120 -> CVTSI2SD
        | `Int 121 -> CVTSI2SS
        | `Int 122 -> CVTSS2SD
        | `Int 123 -> CVTSS2SI
        | `Int 124 -> CVTTPD2DQ
        | `Int 125 -> CVTTPS2DQ
        | `Int 126 -> CVTTSD2SI
        | `Int 127 -> CVTTSS2SI
        | `Int 128 -> CWD
        | `Int 129 -> CWDE
        | `Int 130 -> DAA
        | `Int 131 -> DAS
        | `Int 132 -> DATA16
        | `Int 133 -> DEC
        | `Int 134 -> DIV
        | `Int 135 -> DIVPD
        | `Int 136 -> DIVPS
        | `Int 137 -> FDIVR
        | `Int 138 -> FIDIVR
        | `Int 139 -> FDIVRP
        | `Int 140 -> DIVSD
        | `Int 141 -> DIVSS
        | `Int 142 -> FDIV
        | `Int 143 -> FIDIV
        | `Int 144 -> FDIVP
        | `Int 145 -> DPPD
        | `Int 146 -> DPPS
        | `Int 147 -> RET
        | `Int 148 -> ENCLS
        | `Int 149 -> ENCLU
        | `Int 150 -> ENTER
        | `Int 151 -> EXTRACTPS
        | `Int 152 -> EXTRQ
        | `Int 153 -> F2XM1
        | `Int 154 -> LCALL
        | `Int 155 -> LJMP
        | `Int 156 -> FBLD
        | `Int 157 -> FBSTP
        | `Int 158 -> FCOMPP
        | `Int 159 -> FDECSTP
        | `Int 160 -> FEMMS
        | `Int 161 -> FFREE
        | `Int 162 -> FICOM
        | `Int 163 -> FICOMP
        | `Int 164 -> FINCSTP
        | `Int 165 -> FLDCW
        | `Int 166 -> FLDENV
        | `Int 167 -> FLDL2E
        | `Int 168 -> FLDL2T
        | `Int 169 -> FLDLG2
        | `Int 170 -> FLDLN2
        | `Int 171 -> FLDPI
        | `Int 172 -> FNCLEX
        | `Int 173 -> FNINIT
        | `Int 174 -> FNOP
        | `Int 175 -> FNSTCW
        | `Int 176 -> FNSTSW
        | `Int 177 -> FPATAN
        | `Int 178 -> FPREM
        | `Int 179 -> FPREM1
        | `Int 180 -> FPTAN
        | `Int 181 -> FFREEP
        | `Int 182 -> FRNDINT
        | `Int 183 -> FRSTOR
        | `Int 184 -> FNSAVE
        | `Int 185 -> FSCALE
        | `Int 186 -> FSETPM
        | `Int 187 -> FSINCOS
        | `Int 188 -> FNSTENV
        | `Int 189 -> FXAM
        | `Int 190 -> FXRSTOR
        | `Int 191 -> FXRSTOR64
        | `Int 192 -> FXSAVE
        | `Int 193 -> FXSAVE64
        | `Int 194 -> FXTRACT
        | `Int 195 -> FYL2X
        | `Int 196 -> FYL2XP1
        | `Int 197 -> MOVAPD
        | `Int 198 -> MOVAPS
        | `Int 199 -> ORPD
        | `Int 200 -> ORPS
        | `Int 201 -> VMOVAPD
        | `Int 202 -> VMOVAPS
        | `Int 203 -> XORPD
        | `Int 204 -> XORPS
        | `Int 205 -> GETSEC
        | `Int 206 -> HADDPD
        | `Int 207 -> HADDPS
        | `Int 208 -> HLT
        | `Int 209 -> HSUBPD
        | `Int 210 -> HSUBPS
        | `Int 211 -> IDIV
        | `Int 212 -> FILD
        | `Int 213 -> IMUL
        | `Int 214 -> IN
        | `Int 215 -> INC
        | `Int 216 -> INSB
        | `Int 217 -> INSERTPS
        | `Int 218 -> INSERTQ
        | `Int 219 -> INSD
        | `Int 220 -> INSW
        | `Int 221 -> INT
        | `Int 222 -> INT1
        | `Int 223 -> INT3
        | `Int 224 -> INTO
        | `Int 225 -> INVD
        | `Int 226 -> INVEPT
        | `Int 227 -> INVLPG
        | `Int 228 -> INVLPGA
        | `Int 229 -> INVPCID
        | `Int 230 -> INVVPID
        | `Int 231 -> IRET
        | `Int 232 -> IRETD
        | `Int 233 -> IRETQ
        | `Int 234 -> FISTTP
        | `Int 235 -> FIST
        | `Int 236 -> FISTP
        | `Int 237 -> UCOMISD
        | `Int 238 -> UCOMISS
        | `Int 239 -> VCOMISD
        | `Int 240 -> VCOMISS
        | `Int 241 -> VCVTSD2SS
        | `Int 242 -> VCVTSI2SD
        | `Int 243 -> VCVTSI2SS
        | `Int 244 -> VCVTSS2SD
        | `Int 245 -> VCVTTSD2SI
        | `Int 246 -> VCVTTSD2USI
        | `Int 247 -> VCVTTSS2SI
        | `Int 248 -> VCVTTSS2USI
        | `Int 249 -> VCVTUSI2SD
        | `Int 250 -> VCVTUSI2SS
        | `Int 251 -> VUCOMISD
        | `Int 252 -> VUCOMISS
        | `Int 253 -> JAE
        | `Int 254 -> JA
        | `Int 255 -> JBE
        | `Int 256 -> JB
        | `Int 257 -> JCXZ
        | `Int 258 -> JECXZ
        | `Int 259 -> JE
        | `Int 260 -> JGE
        | `Int 261 -> JG
        | `Int 262 -> JLE
        | `Int 263 -> JL
        | `Int 264 -> JMP
        | `Int 265 -> JNE
        | `Int 266 -> JNO
        | `Int 267 -> JNP
        | `Int 268 -> JNS
        | `Int 269 -> JO
        | `Int 270 -> JP
        | `Int 271 -> JRCXZ
        | `Int 272 -> JS
        | `Int 273 -> KANDB
        | `Int 274 -> KANDD
        | `Int 275 -> KANDNB
        | `Int 276 -> KANDND
        | `Int 277 -> KANDNQ
        | `Int 278 -> KANDNW
        | `Int 279 -> KANDQ
        | `Int 280 -> KANDW
        | `Int 281 -> KMOVB
        | `Int 282 -> KMOVD
        | `Int 283 -> KMOVQ
        | `Int 284 -> KMOVW
        | `Int 285 -> KNOTB
        | `Int 286 -> KNOTD
        | `Int 287 -> KNOTQ
        | `Int 288 -> KNOTW
        | `Int 289 -> KORB
        | `Int 290 -> KORD
        | `Int 291 -> KORQ
        | `Int 292 -> KORTESTB
        | `Int 293 -> KORTESTD
        | `Int 294 -> KORTESTQ
        | `Int 295 -> KORTESTW
        | `Int 296 -> KORW
        | `Int 297 -> KSHIFTLB
        | `Int 298 -> KSHIFTLD
        | `Int 299 -> KSHIFTLQ
        | `Int 300 -> KSHIFTLW
        | `Int 301 -> KSHIFTRB
        | `Int 302 -> KSHIFTRD
        | `Int 303 -> KSHIFTRQ
        | `Int 304 -> KSHIFTRW
        | `Int 305 -> KUNPCKBW
        | `Int 306 -> KXNORB
        | `Int 307 -> KXNORD
        | `Int 308 -> KXNORQ
        | `Int 309 -> KXNORW
        | `Int 310 -> KXORB
        | `Int 311 -> KXORD
        | `Int 312 -> KXORQ
        | `Int 313 -> KXORW
        | `Int 314 -> LAHF
        | `Int 315 -> LAR
        | `Int 316 -> LDDQU
        | `Int 317 -> LDMXCSR
        | `Int 318 -> LDS
        | `Int 319 -> FLDZ
        | `Int 320 -> FLD1
        | `Int 321 -> FLD
        | `Int 322 -> LEA
        | `Int 323 -> LEAVE
        | `Int 324 -> LES
        | `Int 325 -> LFENCE
        | `Int 326 -> LFS
        | `Int 327 -> LGDT
        | `Int 328 -> LGS
        | `Int 329 -> LIDT
        | `Int 330 -> LLDT
        | `Int 331 -> LMSW
        | `Int 332 -> OR
        | `Int 333 -> SUB
        | `Int 334 -> XOR
        | `Int 335 -> LODSB
        | `Int 336 -> LODSD
        | `Int 337 -> LODSQ
        | `Int 338 -> LODSW
        | `Int 339 -> LOOP
        | `Int 340 -> LOOPE
        | `Int 341 -> LOOPNE
        | `Int 342 -> RETF
        | `Int 343 -> RETFQ
        | `Int 344 -> LSL
        | `Int 345 -> LSS
        | `Int 346 -> LTR
        | `Int 347 -> XADD
        | `Int 348 -> LZCNT
        | `Int 349 -> MASKMOVDQU
        | `Int 350 -> MAXPD
        | `Int 351 -> MAXPS
        | `Int 352 -> MAXSD
        | `Int 353 -> MAXSS
        | `Int 354 -> MFENCE
        | `Int 355 -> MINPD
        | `Int 356 -> MINPS
        | `Int 357 -> MINSD
        | `Int 358 -> MINSS
        | `Int 359 -> CVTPD2PI
        | `Int 360 -> CVTPI2PD
        | `Int 361 -> CVTPI2PS
        | `Int 362 -> CVTPS2PI
        | `Int 363 -> CVTTPD2PI
        | `Int 364 -> CVTTPS2PI
        | `Int 365 -> EMMS
        | `Int 366 -> MASKMOVQ
        | `Int 367 -> MOVD
        | `Int 368 -> MOVDQ2Q
        | `Int 369 -> MOVNTQ
        | `Int 370 -> MOVQ2DQ
        | `Int 371 -> MOVQ
        | `Int 372 -> PABSB
        | `Int 373 -> PABSD
        | `Int 374 -> PABSW
        | `Int 375 -> PACKSSDW
        | `Int 376 -> PACKSSWB
        | `Int 377 -> PACKUSWB
        | `Int 378 -> PADDB
        | `Int 379 -> PADDD
        | `Int 380 -> PADDQ
        | `Int 381 -> PADDSB
        | `Int 382 -> PADDSW
        | `Int 383 -> PADDUSB
        | `Int 384 -> PADDUSW
        | `Int 385 -> PADDW
        | `Int 386 -> PALIGNR
        | `Int 387 -> PANDN
        | `Int 388 -> PAND
        | `Int 389 -> PAVGB
        | `Int 390 -> PAVGW
        | `Int 391 -> PCMPEQB
        | `Int 392 -> PCMPEQD
        | `Int 393 -> PCMPEQW
        | `Int 394 -> PCMPGTB
        | `Int 395 -> PCMPGTD
        | `Int 396 -> PCMPGTW
        | `Int 397 -> PEXTRW
        | `Int 398 -> PHADDSW
        | `Int 399 -> PHADDW
        | `Int 400 -> PHADDD
        | `Int 401 -> PHSUBD
        | `Int 402 -> PHSUBSW
        | `Int 403 -> PHSUBW
        | `Int 404 -> PINSRW
        | `Int 405 -> PMADDUBSW
        | `Int 406 -> PMADDWD
        | `Int 407 -> PMAXSW
        | `Int 408 -> PMAXUB
        | `Int 409 -> PMINSW
        | `Int 410 -> PMINUB
        | `Int 411 -> PMOVMSKB
        | `Int 412 -> PMULHRSW
        | `Int 413 -> PMULHUW
        | `Int 414 -> PMULHW
        | `Int 415 -> PMULLW
        | `Int 416 -> PMULUDQ
        | `Int 417 -> POR
        | `Int 418 -> PSADBW
        | `Int 419 -> PSHUFB
        | `Int 420 -> PSHUFW
        | `Int 421 -> PSIGNB
        | `Int 422 -> PSIGND
        | `Int 423 -> PSIGNW
        | `Int 424 -> PSLLD
        | `Int 425 -> PSLLQ
        | `Int 426 -> PSLLW
        | `Int 427 -> PSRAD
        | `Int 428 -> PSRAW
        | `Int 429 -> PSRLD
        | `Int 430 -> PSRLQ
        | `Int 431 -> PSRLW
        | `Int 432 -> PSUBB
        | `Int 433 -> PSUBD
        | `Int 434 -> PSUBQ
        | `Int 435 -> PSUBSB
        | `Int 436 -> PSUBSW
        | `Int 437 -> PSUBUSB
        | `Int 438 -> PSUBUSW
        | `Int 439 -> PSUBW
        | `Int 440 -> PUNPCKHBW
        | `Int 441 -> PUNPCKHDQ
        | `Int 442 -> PUNPCKHWD
        | `Int 443 -> PUNPCKLBW
        | `Int 444 -> PUNPCKLDQ
        | `Int 445 -> PUNPCKLWD
        | `Int 446 -> PXOR
        | `Int 447 -> MONITOR
        | `Int 448 -> MONTMUL
        | `Int 449 -> MOV
        | `Int 450 -> MOVABS
        | `Int 451 -> MOVBE
        | `Int 452 -> MOVDDUP
        | `Int 453 -> MOVDQA
        | `Int 454 -> MOVDQU
        | `Int 455 -> MOVHLPS
        | `Int 456 -> MOVHPD
        | `Int 457 -> MOVHPS
        | `Int 458 -> MOVLHPS
        | `Int 459 -> MOVLPD
        | `Int 460 -> MOVLPS
        | `Int 461 -> MOVMSKPD
        | `Int 462 -> MOVMSKPS
        | `Int 463 -> MOVNTDQA
        | `Int 464 -> MOVNTDQ
        | `Int 465 -> MOVNTI
        | `Int 466 -> MOVNTPD
        | `Int 467 -> MOVNTPS
        | `Int 468 -> MOVNTSD
        | `Int 469 -> MOVNTSS
        | `Int 470 -> MOVSB
        | `Int 471 -> MOVSD
        | `Int 472 -> MOVSHDUP
        | `Int 473 -> MOVSLDUP
        | `Int 474 -> MOVSQ
        | `Int 475 -> MOVSS
        | `Int 476 -> MOVSW
        | `Int 477 -> MOVSX
        | `Int 478 -> MOVSXD
        | `Int 479 -> MOVUPD
        | `Int 480 -> MOVUPS
        | `Int 481 -> MOVZX
        | `Int 482 -> MPSADBW
        | `Int 483 -> MUL
        | `Int 484 -> MULPD
        | `Int 485 -> MULPS
        | `Int 486 -> MULSD
        | `Int 487 -> MULSS
        | `Int 488 -> MULX
        | `Int 489 -> FMUL
        | `Int 490 -> FIMUL
        | `Int 491 -> FMULP
        | `Int 492 -> MWAIT
        | `Int 493 -> NEG
        | `Int 494 -> NOP
        | `Int 495 -> NOT
        | `Int 496 -> OUT
        | `Int 497 -> OUTSB
        | `Int 498 -> OUTSD
        | `Int 499 -> OUTSW
        | `Int 500 -> PACKUSDW
        | `Int 501 -> PAUSE
        | `Int 502 -> PAVGUSB
        | `Int 503 -> PBLENDVB
        | `Int 504 -> PBLENDW
        | `Int 505 -> PCLMULQDQ
        | `Int 506 -> PCMPEQQ
        | `Int 507 -> PCMPESTRI
        | `Int 508 -> PCMPESTRM
        | `Int 509 -> PCMPGTQ
        | `Int 510 -> PCMPISTRI
        | `Int 511 -> PCMPISTRM
        | `Int 512 -> PCOMMIT
        | `Int 513 -> PDEP
        | `Int 514 -> PEXT
        | `Int 515 -> PEXTRB
        | `Int 516 -> PEXTRD
        | `Int 517 -> PEXTRQ
        | `Int 518 -> PF2ID
        | `Int 519 -> PF2IW
        | `Int 520 -> PFACC
        | `Int 521 -> PFADD
        | `Int 522 -> PFCMPEQ
        | `Int 523 -> PFCMPGE
        | `Int 524 -> PFCMPGT
        | `Int 525 -> PFMAX
        | `Int 526 -> PFMIN
        | `Int 527 -> PFMUL
        | `Int 528 -> PFNACC
        | `Int 529 -> PFPNACC
        | `Int 530 -> PFRCPIT1
        | `Int 531 -> PFRCPIT2
        | `Int 532 -> PFRCP
        | `Int 533 -> PFRSQIT1
        | `Int 534 -> PFRSQRT
        | `Int 535 -> PFSUBR
        | `Int 536 -> PFSUB
        | `Int 537 -> PHMINPOSUW
        | `Int 538 -> PI2FD
        | `Int 539 -> PI2FW
        | `Int 540 -> PINSRB
        | `Int 541 -> PINSRD
        | `Int 542 -> PINSRQ
        | `Int 543 -> PMAXSB
        | `Int 544 -> PMAXSD
        | `Int 545 -> PMAXUD
        | `Int 546 -> PMAXUW
        | `Int 547 -> PMINSB
        | `Int 548 -> PMINSD
        | `Int 549 -> PMINUD
        | `Int 550 -> PMINUW
        | `Int 551 -> PMOVSXBD
        | `Int 552 -> PMOVSXBQ
        | `Int 553 -> PMOVSXBW
        | `Int 554 -> PMOVSXDQ
        | `Int 555 -> PMOVSXWD
        | `Int 556 -> PMOVSXWQ
        | `Int 557 -> PMOVZXBD
        | `Int 558 -> PMOVZXBQ
        | `Int 559 -> PMOVZXBW
        | `Int 560 -> PMOVZXDQ
        | `Int 561 -> PMOVZXWD
        | `Int 562 -> PMOVZXWQ
        | `Int 563 -> PMULDQ
        | `Int 564 -> PMULHRW
        | `Int 565 -> PMULLD
        | `Int 566 -> POP
        | `Int 567 -> POPAW
        | `Int 568 -> POPAL
        | `Int 569 -> POPCNT
        | `Int 570 -> POPF
        | `Int 571 -> POPFD
        | `Int 572 -> POPFQ
        | `Int 573 -> PREFETCH
        | `Int 574 -> PREFETCHNTA
        | `Int 575 -> PREFETCHT0
        | `Int 576 -> PREFETCHT1
        | `Int 577 -> PREFETCHT2
        | `Int 578 -> PREFETCHW
        | `Int 579 -> PSHUFD
        | `Int 580 -> PSHUFHW
        | `Int 581 -> PSHUFLW
        | `Int 582 -> PSLLDQ
        | `Int 583 -> PSRLDQ
        | `Int 584 -> PSWAPD
        | `Int 585 -> PTEST
        | `Int 586 -> PUNPCKHQDQ
        | `Int 587 -> PUNPCKLQDQ
        | `Int 588 -> PUSH
        | `Int 589 -> PUSHAW
        | `Int 590 -> PUSHAL
        | `Int 591 -> PUSHF
        | `Int 592 -> PUSHFD
        | `Int 593 -> PUSHFQ
        | `Int 594 -> RCL
        | `Int 595 -> RCPPS
        | `Int 596 -> RCPSS
        | `Int 597 -> RCR
        | `Int 598 -> RDFSBASE
        | `Int 599 -> RDGSBASE
        | `Int 600 -> RDMSR
        | `Int 601 -> RDPMC
        | `Int 602 -> RDRAND
        | `Int 603 -> RDSEED
        | `Int 604 -> RDTSC
        | `Int 605 -> RDTSCP
        | `Int 606 -> ROL
        | `Int 607 -> ROR
        | `Int 608 -> RORX
        | `Int 609 -> ROUNDPD
        | `Int 610 -> ROUNDPS
        | `Int 611 -> ROUNDSD
        | `Int 612 -> ROUNDSS
        | `Int 613 -> RSM
        | `Int 614 -> RSQRTPS
        | `Int 615 -> RSQRTSS
        | `Int 616 -> SAHF
        | `Int 617 -> SAL
        | `Int 618 -> SALC
        | `Int 619 -> SAR
        | `Int 620 -> SARX
        | `Int 621 -> SBB
        | `Int 622 -> SCASB
        | `Int 623 -> SCASD
        | `Int 624 -> SCASQ
        | `Int 625 -> SCASW
        | `Int 626 -> SETAE
        | `Int 627 -> SETA
        | `Int 628 -> SETBE
        | `Int 629 -> SETB
        | `Int 630 -> SETE
        | `Int 631 -> SETGE
        | `Int 632 -> SETG
        | `Int 633 -> SETLE
        | `Int 634 -> SETL
        | `Int 635 -> SETNE
        | `Int 636 -> SETNO
        | `Int 637 -> SETNP
        | `Int 638 -> SETNS
        | `Int 639 -> SETO
        | `Int 640 -> SETP
        | `Int 641 -> SETS
        | `Int 642 -> SFENCE
        | `Int 643 -> SGDT
        | `Int 644 -> SHA1MSG1
        | `Int 645 -> SHA1MSG2
        | `Int 646 -> SHA1NEXTE
        | `Int 647 -> SHA1RNDS4
        | `Int 648 -> SHA256MSG1
        | `Int 649 -> SHA256MSG2
        | `Int 650 -> SHA256RNDS2
        | `Int 651 -> SHL
        | `Int 652 -> SHLD
        | `Int 653 -> SHLX
        | `Int 654 -> SHR
        | `Int 655 -> SHRD
        | `Int 656 -> SHRX
        | `Int 657 -> SHUFPD
        | `Int 658 -> SHUFPS
        | `Int 659 -> SIDT
        | `Int 660 -> FSIN
        | `Int 661 -> SKINIT
        | `Int 662 -> SLDT
        | `Int 663 -> SMSW
        | `Int 664 -> SQRTPD
        | `Int 665 -> SQRTPS
        | `Int 666 -> SQRTSD
        | `Int 667 -> SQRTSS
        | `Int 668 -> FSQRT
        | `Int 669 -> STAC
        | `Int 670 -> STC
        | `Int 671 -> STD
        | `Int 672 -> STGI
        | `Int 673 -> STI
        | `Int 674 -> STMXCSR
        | `Int 675 -> STOSB
        | `Int 676 -> STOSD
        | `Int 677 -> STOSQ
        | `Int 678 -> STOSW
        | `Int 679 -> STR
        | `Int 680 -> FST
        | `Int 681 -> FSTP
        | `Int 682 -> FSTPNCE
        | `Int 683 -> FXCH
        | `Int 684 -> SUBPD
        | `Int 685 -> SUBPS
        | `Int 686 -> FSUBR
        | `Int 687 -> FISUBR
        | `Int 688 -> FSUBRP
        | `Int 689 -> SUBSD
        | `Int 690 -> SUBSS
        | `Int 691 -> FSUB
        | `Int 692 -> FISUB
        | `Int 693 -> FSUBP
        | `Int 694 -> SWAPGS
        | `Int 695 -> SYSCALL
        | `Int 696 -> SYSENTER
        | `Int 697 -> SYSEXIT
        | `Int 698 -> SYSRET
        | `Int 699 -> T1MSKC
        | `Int 700 -> TEST
        | `Int 701 -> UD2
        | `Int 702 -> FTST
        | `Int 703 -> TZCNT
        | `Int 704 -> TZMSK
        | `Int 705 -> FUCOMIP
        | `Int 706 -> FUCOMI
        | `Int 707 -> FUCOMPP
        | `Int 708 -> FUCOMP
        | `Int 709 -> FUCOM
        | `Int 710 -> UD2B
        | `Int 711 -> UNPCKHPD
        | `Int 712 -> UNPCKHPS
        | `Int 713 -> UNPCKLPD
        | `Int 714 -> UNPCKLPS
        | `Int 715 -> VADDPD
        | `Int 716 -> VADDPS
        | `Int 717 -> VADDSD
        | `Int 718 -> VADDSS
        | `Int 719 -> VADDSUBPD
        | `Int 720 -> VADDSUBPS
        | `Int 721 -> VAESDECLAST
        | `Int 722 -> VAESDEC
        | `Int 723 -> VAESENCLAST
        | `Int 724 -> VAESENC
        | `Int 725 -> VAESIMC
        | `Int 726 -> VAESKEYGENASSIST
        | `Int 727 -> VALIGND
        | `Int 728 -> VALIGNQ
        | `Int 729 -> VANDNPD
        | `Int 730 -> VANDNPS
        | `Int 731 -> VANDPD
        | `Int 732 -> VANDPS
        | `Int 733 -> VBLENDMPD
        | `Int 734 -> VBLENDMPS
        | `Int 735 -> VBLENDPD
        | `Int 736 -> VBLENDPS
        | `Int 737 -> VBLENDVPD
        | `Int 738 -> VBLENDVPS
        | `Int 739 -> VBROADCASTF128
        | `Int 740 -> VBROADCASTI32X4
        | `Int 741 -> VBROADCASTI64X4
        | `Int 742 -> VBROADCASTSD
        | `Int 743 -> VBROADCASTSS
        | `Int 744 -> VCOMPRESSPD
        | `Int 745 -> VCOMPRESSPS
        | `Int 746 -> VCVTDQ2PD
        | `Int 747 -> VCVTDQ2PS
        | `Int 748 -> VCVTPD2DQX
        | `Int 749 -> VCVTPD2DQ
        | `Int 750 -> VCVTPD2PSX
        | `Int 751 -> VCVTPD2PS
        | `Int 752 -> VCVTPD2UDQ
        | `Int 753 -> VCVTPH2PS
        | `Int 754 -> VCVTPS2DQ
        | `Int 755 -> VCVTPS2PD
        | `Int 756 -> VCVTPS2PH
        | `Int 757 -> VCVTPS2UDQ
        | `Int 758 -> VCVTSD2SI
        | `Int 759 -> VCVTSD2USI
        | `Int 760 -> VCVTSS2SI
        | `Int 761 -> VCVTSS2USI
        | `Int 762 -> VCVTTPD2DQX
        | `Int 763 -> VCVTTPD2DQ
        | `Int 764 -> VCVTTPD2UDQ
        | `Int 765 -> VCVTTPS2DQ
        | `Int 766 -> VCVTTPS2UDQ
        | `Int 767 -> VCVTUDQ2PD
        | `Int 768 -> VCVTUDQ2PS
        | `Int 769 -> VDIVPD
        | `Int 770 -> VDIVPS
        | `Int 771 -> VDIVSD
        | `Int 772 -> VDIVSS
        | `Int 773 -> VDPPD
        | `Int 774 -> VDPPS
        | `Int 775 -> VERR
        | `Int 776 -> VERW
        | `Int 777 -> VEXP2PD
        | `Int 778 -> VEXP2PS
        | `Int 779 -> VEXPANDPD
        | `Int 780 -> VEXPANDPS
        | `Int 781 -> VEXTRACTF128
        | `Int 782 -> VEXTRACTF32X4
        | `Int 783 -> VEXTRACTF64X4
        | `Int 784 -> VEXTRACTI128
        | `Int 785 -> VEXTRACTI32X4
        | `Int 786 -> VEXTRACTI64X4
        | `Int 787 -> VEXTRACTPS
        | `Int 788 -> VFMADD132PD
        | `Int 789 -> VFMADD132PS
        | `Int 790 -> VFMADDPD
        | `Int 791 -> VFMADD213PD
        | `Int 792 -> VFMADD231PD
        | `Int 793 -> VFMADDPS
        | `Int 794 -> VFMADD213PS
        | `Int 795 -> VFMADD231PS
        | `Int 796 -> VFMADDSD
        | `Int 797 -> VFMADD213SD
        | `Int 798 -> VFMADD132SD
        | `Int 799 -> VFMADD231SD
        | `Int 800 -> VFMADDSS
        | `Int 801 -> VFMADD213SS
        | `Int 802 -> VFMADD132SS
        | `Int 803 -> VFMADD231SS
        | `Int 804 -> VFMADDSUB132PD
        | `Int 805 -> VFMADDSUB132PS
        | `Int 806 -> VFMADDSUBPD
        | `Int 807 -> VFMADDSUB213PD
        | `Int 808 -> VFMADDSUB231PD
        | `Int 809 -> VFMADDSUBPS
        | `Int 810 -> VFMADDSUB213PS
        | `Int 811 -> VFMADDSUB231PS
        | `Int 812 -> VFMSUB132PD
        | `Int 813 -> VFMSUB132PS
        | `Int 814 -> VFMSUBADD132PD
        | `Int 815 -> VFMSUBADD132PS
        | `Int 816 -> VFMSUBADDPD
        | `Int 817 -> VFMSUBADD213PD
        | `Int 818 -> VFMSUBADD231PD
        | `Int 819 -> VFMSUBADDPS
        | `Int 820 -> VFMSUBADD213PS
        | `Int 821 -> VFMSUBADD231PS
        | `Int 822 -> VFMSUBPD
        | `Int 823 -> VFMSUB213PD
        | `Int 824 -> VFMSUB231PD
        | `Int 825 -> VFMSUBPS
        | `Int 826 -> VFMSUB213PS
        | `Int 827 -> VFMSUB231PS
        | `Int 828 -> VFMSUBSD
        | `Int 829 -> VFMSUB213SD
        | `Int 830 -> VFMSUB132SD
        | `Int 831 -> VFMSUB231SD
        | `Int 832 -> VFMSUBSS
        | `Int 833 -> VFMSUB213SS
        | `Int 834 -> VFMSUB132SS
        | `Int 835 -> VFMSUB231SS
        | `Int 836 -> VFNMADD132PD
        | `Int 837 -> VFNMADD132PS
        | `Int 838 -> VFNMADDPD
        | `Int 839 -> VFNMADD213PD
        | `Int 840 -> VFNMADD231PD
        | `Int 841 -> VFNMADDPS
        | `Int 842 -> VFNMADD213PS
        | `Int 843 -> VFNMADD231PS
        | `Int 844 -> VFNMADDSD
        | `Int 845 -> VFNMADD213SD
        | `Int 846 -> VFNMADD132SD
        | `Int 847 -> VFNMADD231SD
        | `Int 848 -> VFNMADDSS
        | `Int 849 -> VFNMADD213SS
        | `Int 850 -> VFNMADD132SS
        | `Int 851 -> VFNMADD231SS
        | `Int 852 -> VFNMSUB132PD
        | `Int 853 -> VFNMSUB132PS
        | `Int 854 -> VFNMSUBPD
        | `Int 855 -> VFNMSUB213PD
        | `Int 856 -> VFNMSUB231PD
        | `Int 857 -> VFNMSUBPS
        | `Int 858 -> VFNMSUB213PS
        | `Int 859 -> VFNMSUB231PS
        | `Int 860 -> VFNMSUBSD
        | `Int 861 -> VFNMSUB213SD
        | `Int 862 -> VFNMSUB132SD
        | `Int 863 -> VFNMSUB231SD
        | `Int 864 -> VFNMSUBSS
        | `Int 865 -> VFNMSUB213SS
        | `Int 866 -> VFNMSUB132SS
        | `Int 867 -> VFNMSUB231SS
        | `Int 868 -> VFRCZPD
        | `Int 869 -> VFRCZPS
        | `Int 870 -> VFRCZSD
        | `Int 871 -> VFRCZSS
        | `Int 872 -> VORPD
        | `Int 873 -> VORPS
        | `Int 874 -> VXORPD
        | `Int 875 -> VXORPS
        | `Int 876 -> VGATHERDPD
        | `Int 877 -> VGATHERDPS
        | `Int 878 -> VGATHERPF0DPD
        | `Int 879 -> VGATHERPF0DPS
        | `Int 880 -> VGATHERPF0QPD
        | `Int 881 -> VGATHERPF0QPS
        | `Int 882 -> VGATHERPF1DPD
        | `Int 883 -> VGATHERPF1DPS
        | `Int 884 -> VGATHERPF1QPD
        | `Int 885 -> VGATHERPF1QPS
        | `Int 886 -> VGATHERQPD
        | `Int 887 -> VGATHERQPS
        | `Int 888 -> VHADDPD
        | `Int 889 -> VHADDPS
        | `Int 890 -> VHSUBPD
        | `Int 891 -> VHSUBPS
        | `Int 892 -> VINSERTF128
        | `Int 893 -> VINSERTF32X4
        | `Int 894 -> VINSERTF32X8
        | `Int 895 -> VINSERTF64X2
        | `Int 896 -> VINSERTF64X4
        | `Int 897 -> VINSERTI128
        | `Int 898 -> VINSERTI32X4
        | `Int 899 -> VINSERTI32X8
        | `Int 900 -> VINSERTI64X2
        | `Int 901 -> VINSERTI64X4
        | `Int 902 -> VINSERTPS
        | `Int 903 -> VLDDQU
        | `Int 904 -> VLDMXCSR
        | `Int 905 -> VMASKMOVDQU
        | `Int 906 -> VMASKMOVPD
        | `Int 907 -> VMASKMOVPS
        | `Int 908 -> VMAXPD
        | `Int 909 -> VMAXPS
        | `Int 910 -> VMAXSD
        | `Int 911 -> VMAXSS
        | `Int 912 -> VMCALL
        | `Int 913 -> VMCLEAR
        | `Int 914 -> VMFUNC
        | `Int 915 -> VMINPD
        | `Int 916 -> VMINPS
        | `Int 917 -> VMINSD
        | `Int 918 -> VMINSS
        | `Int 919 -> VMLAUNCH
        | `Int 920 -> VMLOAD
        | `Int 921 -> VMMCALL
        | `Int 922 -> VMOVQ
        | `Int 923 -> VMOVDDUP
        | `Int 924 -> VMOVD
        | `Int 925 -> VMOVDQA32
        | `Int 926 -> VMOVDQA64
        | `Int 927 -> VMOVDQA
        | `Int 928 -> VMOVDQU16
        | `Int 929 -> VMOVDQU32
        | `Int 930 -> VMOVDQU64
        | `Int 931 -> VMOVDQU8
        | `Int 932 -> VMOVDQU
        | `Int 933 -> VMOVHLPS
        | `Int 934 -> VMOVHPD
        | `Int 935 -> VMOVHPS
        | `Int 936 -> VMOVLHPS
        | `Int 937 -> VMOVLPD
        | `Int 938 -> VMOVLPS
        | `Int 939 -> VMOVMSKPD
        | `Int 940 -> VMOVMSKPS
        | `Int 941 -> VMOVNTDQA
        | `Int 942 -> VMOVNTDQ
        | `Int 943 -> VMOVNTPD
        | `Int 944 -> VMOVNTPS
        | `Int 945 -> VMOVSD
        | `Int 946 -> VMOVSHDUP
        | `Int 947 -> VMOVSLDUP
        | `Int 948 -> VMOVSS
        | `Int 949 -> VMOVUPD
        | `Int 950 -> VMOVUPS
        | `Int 951 -> VMPSADBW
        | `Int 952 -> VMPTRLD
        | `Int 953 -> VMPTRST
        | `Int 954 -> VMREAD
        | `Int 955 -> VMRESUME
        | `Int 956 -> VMRUN
        | `Int 957 -> VMSAVE
        | `Int 958 -> VMULPD
        | `Int 959 -> VMULPS
        | `Int 960 -> VMULSD
        | `Int 961 -> VMULSS
        | `Int 962 -> VMWRITE
        | `Int 963 -> VMXOFF
        | `Int 964 -> VMXON
        | `Int 965 -> VPABSB
        | `Int 966 -> VPABSD
        | `Int 967 -> VPABSQ
        | `Int 968 -> VPABSW
        | `Int 969 -> VPACKSSDW
        | `Int 970 -> VPACKSSWB
        | `Int 971 -> VPACKUSDW
        | `Int 972 -> VPACKUSWB
        | `Int 973 -> VPADDB
        | `Int 974 -> VPADDD
        | `Int 975 -> VPADDQ
        | `Int 976 -> VPADDSB
        | `Int 977 -> VPADDSW
        | `Int 978 -> VPADDUSB
        | `Int 979 -> VPADDUSW
        | `Int 980 -> VPADDW
        | `Int 981 -> VPALIGNR
        | `Int 982 -> VPANDD
        | `Int 983 -> VPANDND
        | `Int 984 -> VPANDNQ
        | `Int 985 -> VPANDN
        | `Int 986 -> VPANDQ
        | `Int 987 -> VPAND
        | `Int 988 -> VPAVGB
        | `Int 989 -> VPAVGW
        | `Int 990 -> VPBLENDD
        | `Int 991 -> VPBLENDMB
        | `Int 992 -> VPBLENDMD
        | `Int 993 -> VPBLENDMQ
        | `Int 994 -> VPBLENDMW
        | `Int 995 -> VPBLENDVB
        | `Int 996 -> VPBLENDW
        | `Int 997 -> VPBROADCASTB
        | `Int 998 -> VPBROADCASTD
        | `Int 999 -> VPBROADCASTMB2Q
        | `Int 1000 -> VPBROADCASTMW2D
        | `Int 1001 -> VPBROADCASTQ
        | `Int 1002 -> VPBROADCASTW
        | `Int 1003 -> VPCLMULQDQ
        | `Int 1004 -> VPCMOV
        | `Int 1005 -> VPCMPB
        | `Int 1006 -> VPCMPD
        | `Int 1007 -> VPCMPEQB
        | `Int 1008 -> VPCMPEQD
        | `Int 1009 -> VPCMPEQQ
        | `Int 1010 -> VPCMPEQW
        | `Int 1011 -> VPCMPESTRI
        | `Int 1012 -> VPCMPESTRM
        | `Int 1013 -> VPCMPGTB
        | `Int 1014 -> VPCMPGTD
        | `Int 1015 -> VPCMPGTQ
        | `Int 1016 -> VPCMPGTW
        | `Int 1017 -> VPCMPISTRI
        | `Int 1018 -> VPCMPISTRM
        | `Int 1019 -> VPCMPQ
        | `Int 1020 -> VPCMPUB
        | `Int 1021 -> VPCMPUD
        | `Int 1022 -> VPCMPUQ
        | `Int 1023 -> VPCMPUW
        | `Int 1024 -> VPCMPW
        | `Int 1025 -> VPCOMB
        | `Int 1026 -> VPCOMD
        | `Int 1027 -> VPCOMPRESSD
        | `Int 1028 -> VPCOMPRESSQ
        | `Int 1029 -> VPCOMQ
        | `Int 1030 -> VPCOMUB
        | `Int 1031 -> VPCOMUD
        | `Int 1032 -> VPCOMUQ
        | `Int 1033 -> VPCOMUW
        | `Int 1034 -> VPCOMW
        | `Int 1035 -> VPCONFLICTD
        | `Int 1036 -> VPCONFLICTQ
        | `Int 1037 -> VPERM2F128
        | `Int 1038 -> VPERM2I128
        | `Int 1039 -> VPERMD
        | `Int 1040 -> VPERMI2D
        | `Int 1041 -> VPERMI2PD
        | `Int 1042 -> VPERMI2PS
        | `Int 1043 -> VPERMI2Q
        | `Int 1044 -> VPERMIL2PD
        | `Int 1045 -> VPERMIL2PS
        | `Int 1046 -> VPERMILPD
        | `Int 1047 -> VPERMILPS
        | `Int 1048 -> VPERMPD
        | `Int 1049 -> VPERMPS
        | `Int 1050 -> VPERMQ
        | `Int 1051 -> VPERMT2D
        | `Int 1052 -> VPERMT2PD
        | `Int 1053 -> VPERMT2PS
        | `Int 1054 -> VPERMT2Q
        | `Int 1055 -> VPEXPANDD
        | `Int 1056 -> VPEXPANDQ
        | `Int 1057 -> VPEXTRB
        | `Int 1058 -> VPEXTRD
        | `Int 1059 -> VPEXTRQ
        | `Int 1060 -> VPEXTRW
        | `Int 1061 -> VPGATHERDD
        | `Int 1062 -> VPGATHERDQ
        | `Int 1063 -> VPGATHERQD
        | `Int 1064 -> VPGATHERQQ
        | `Int 1065 -> VPHADDBD
        | `Int 1066 -> VPHADDBQ
        | `Int 1067 -> VPHADDBW
        | `Int 1068 -> VPHADDDQ
        | `Int 1069 -> VPHADDD
        | `Int 1070 -> VPHADDSW
        | `Int 1071 -> VPHADDUBD
        | `Int 1072 -> VPHADDUBQ
        | `Int 1073 -> VPHADDUBW
        | `Int 1074 -> VPHADDUDQ
        | `Int 1075 -> VPHADDUWD
        | `Int 1076 -> VPHADDUWQ
        | `Int 1077 -> VPHADDWD
        | `Int 1078 -> VPHADDWQ
        | `Int 1079 -> VPHADDW
        | `Int 1080 -> VPHMINPOSUW
        | `Int 1081 -> VPHSUBBW
        | `Int 1082 -> VPHSUBDQ
        | `Int 1083 -> VPHSUBD
        | `Int 1084 -> VPHSUBSW
        | `Int 1085 -> VPHSUBWD
        | `Int 1086 -> VPHSUBW
        | `Int 1087 -> VPINSRB
        | `Int 1088 -> VPINSRD
        | `Int 1089 -> VPINSRQ
        | `Int 1090 -> VPINSRW
        | `Int 1091 -> VPLZCNTD
        | `Int 1092 -> VPLZCNTQ
        | `Int 1093 -> VPMACSDD
        | `Int 1094 -> VPMACSDQH
        | `Int 1095 -> VPMACSDQL
        | `Int 1096 -> VPMACSSDD
        | `Int 1097 -> VPMACSSDQH
        | `Int 1098 -> VPMACSSDQL
        | `Int 1099 -> VPMACSSWD
        | `Int 1100 -> VPMACSSWW
        | `Int 1101 -> VPMACSWD
        | `Int 1102 -> VPMACSWW
        | `Int 1103 -> VPMADCSSWD
        | `Int 1104 -> VPMADCSWD
        | `Int 1105 -> VPMADDUBSW
        | `Int 1106 -> VPMADDWD
        | `Int 1107 -> VPMASKMOVD
        | `Int 1108 -> VPMASKMOVQ
        | `Int 1109 -> VPMAXSB
        | `Int 1110 -> VPMAXSD
        | `Int 1111 -> VPMAXSQ
        | `Int 1112 -> VPMAXSW
        | `Int 1113 -> VPMAXUB
        | `Int 1114 -> VPMAXUD
        | `Int 1115 -> VPMAXUQ
        | `Int 1116 -> VPMAXUW
        | `Int 1117 -> VPMINSB
        | `Int 1118 -> VPMINSD
        | `Int 1119 -> VPMINSQ
        | `Int 1120 -> VPMINSW
        | `Int 1121 -> VPMINUB
        | `Int 1122 -> VPMINUD
        | `Int 1123 -> VPMINUQ
        | `Int 1124 -> VPMINUW
        | `Int 1125 -> VPMOVDB
        | `Int 1126 -> VPMOVDW
        | `Int 1127 -> VPMOVM2B
        | `Int 1128 -> VPMOVM2D
        | `Int 1129 -> VPMOVM2Q
        | `Int 1130 -> VPMOVM2W
        | `Int 1131 -> VPMOVMSKB
        | `Int 1132 -> VPMOVQB
        | `Int 1133 -> VPMOVQD
        | `Int 1134 -> VPMOVQW
        | `Int 1135 -> VPMOVSDB
        | `Int 1136 -> VPMOVSDW
        | `Int 1137 -> VPMOVSQB
        | `Int 1138 -> VPMOVSQD
        | `Int 1139 -> VPMOVSQW
        | `Int 1140 -> VPMOVSXBD
        | `Int 1141 -> VPMOVSXBQ
        | `Int 1142 -> VPMOVSXBW
        | `Int 1143 -> VPMOVSXDQ
        | `Int 1144 -> VPMOVSXWD
        | `Int 1145 -> VPMOVSXWQ
        | `Int 1146 -> VPMOVUSDB
        | `Int 1147 -> VPMOVUSDW
        | `Int 1148 -> VPMOVUSQB
        | `Int 1149 -> VPMOVUSQD
        | `Int 1150 -> VPMOVUSQW
        | `Int 1151 -> VPMOVZXBD
        | `Int 1152 -> VPMOVZXBQ
        | `Int 1153 -> VPMOVZXBW
        | `Int 1154 -> VPMOVZXDQ
        | `Int 1155 -> VPMOVZXWD
        | `Int 1156 -> VPMOVZXWQ
        | `Int 1157 -> VPMULDQ
        | `Int 1158 -> VPMULHRSW
        | `Int 1159 -> VPMULHUW
        | `Int 1160 -> VPMULHW
        | `Int 1161 -> VPMULLD
        | `Int 1162 -> VPMULLQ
        | `Int 1163 -> VPMULLW
        | `Int 1164 -> VPMULUDQ
        | `Int 1165 -> VPORD
        | `Int 1166 -> VPORQ
        | `Int 1167 -> VPOR
        | `Int 1168 -> VPPERM
        | `Int 1169 -> VPROTB
        | `Int 1170 -> VPROTD
        | `Int 1171 -> VPROTQ
        | `Int 1172 -> VPROTW
        | `Int 1173 -> VPSADBW
        | `Int 1174 -> VPSCATTERDD
        | `Int 1175 -> VPSCATTERDQ
        | `Int 1176 -> VPSCATTERQD
        | `Int 1177 -> VPSCATTERQQ
        | `Int 1178 -> VPSHAB
        | `Int 1179 -> VPSHAD
        | `Int 1180 -> VPSHAQ
        | `Int 1181 -> VPSHAW
        | `Int 1182 -> VPSHLB
        | `Int 1183 -> VPSHLD
        | `Int 1184 -> VPSHLQ
        | `Int 1185 -> VPSHLW
        | `Int 1186 -> VPSHUFB
        | `Int 1187 -> VPSHUFD
        | `Int 1188 -> VPSHUFHW
        | `Int 1189 -> VPSHUFLW
        | `Int 1190 -> VPSIGNB
        | `Int 1191 -> VPSIGND
        | `Int 1192 -> VPSIGNW
        | `Int 1193 -> VPSLLDQ
        | `Int 1194 -> VPSLLD
        | `Int 1195 -> VPSLLQ
        | `Int 1196 -> VPSLLVD
        | `Int 1197 -> VPSLLVQ
        | `Int 1198 -> VPSLLW
        | `Int 1199 -> VPSRAD
        | `Int 1200 -> VPSRAQ
        | `Int 1201 -> VPSRAVD
        | `Int 1202 -> VPSRAVQ
        | `Int 1203 -> VPSRAW
        | `Int 1204 -> VPSRLDQ
        | `Int 1205 -> VPSRLD
        | `Int 1206 -> VPSRLQ
        | `Int 1207 -> VPSRLVD
        | `Int 1208 -> VPSRLVQ
        | `Int 1209 -> VPSRLW
        | `Int 1210 -> VPSUBB
        | `Int 1211 -> VPSUBD
        | `Int 1212 -> VPSUBQ
        | `Int 1213 -> VPSUBSB
        | `Int 1214 -> VPSUBSW
        | `Int 1215 -> VPSUBUSB
        | `Int 1216 -> VPSUBUSW
        | `Int 1217 -> VPSUBW
        | `Int 1218 -> VPTESTMD
        | `Int 1219 -> VPTESTMQ
        | `Int 1220 -> VPTESTNMD
        | `Int 1221 -> VPTESTNMQ
        | `Int 1222 -> VPTEST
        | `Int 1223 -> VPUNPCKHBW
        | `Int 1224 -> VPUNPCKHDQ
        | `Int 1225 -> VPUNPCKHQDQ
        | `Int 1226 -> VPUNPCKHWD
        | `Int 1227 -> VPUNPCKLBW
        | `Int 1228 -> VPUNPCKLDQ
        | `Int 1229 -> VPUNPCKLQDQ
        | `Int 1230 -> VPUNPCKLWD
        | `Int 1231 -> VPXORD
        | `Int 1232 -> VPXORQ
        | `Int 1233 -> VPXOR
        | `Int 1234 -> VRCP14PD
        | `Int 1235 -> VRCP14PS
        | `Int 1236 -> VRCP14SD
        | `Int 1237 -> VRCP14SS
        | `Int 1238 -> VRCP28PD
        | `Int 1239 -> VRCP28PS
        | `Int 1240 -> VRCP28SD
        | `Int 1241 -> VRCP28SS
        | `Int 1242 -> VRCPPS
        | `Int 1243 -> VRCPSS
        | `Int 1244 -> VRNDSCALEPD
        | `Int 1245 -> VRNDSCALEPS
        | `Int 1246 -> VRNDSCALESD
        | `Int 1247 -> VRNDSCALESS
        | `Int 1248 -> VROUNDPD
        | `Int 1249 -> VROUNDPS
        | `Int 1250 -> VROUNDSD
        | `Int 1251 -> VROUNDSS
        | `Int 1252 -> VRSQRT14PD
        | `Int 1253 -> VRSQRT14PS
        | `Int 1254 -> VRSQRT14SD
        | `Int 1255 -> VRSQRT14SS
        | `Int 1256 -> VRSQRT28PD
        | `Int 1257 -> VRSQRT28PS
        | `Int 1258 -> VRSQRT28SD
        | `Int 1259 -> VRSQRT28SS
        | `Int 1260 -> VRSQRTPS
        | `Int 1261 -> VRSQRTSS
        | `Int 1262 -> VSCATTERDPD
        | `Int 1263 -> VSCATTERDPS
        | `Int 1264 -> VSCATTERPF0DPD
        | `Int 1265 -> VSCATTERPF0DPS
        | `Int 1266 -> VSCATTERPF0QPD
        | `Int 1267 -> VSCATTERPF0QPS
        | `Int 1268 -> VSCATTERPF1DPD
        | `Int 1269 -> VSCATTERPF1DPS
        | `Int 1270 -> VSCATTERPF1QPD
        | `Int 1271 -> VSCATTERPF1QPS
        | `Int 1272 -> VSCATTERQPD
        | `Int 1273 -> VSCATTERQPS
        | `Int 1274 -> VSHUFPD
        | `Int 1275 -> VSHUFPS
        | `Int 1276 -> VSQRTPD
        | `Int 1277 -> VSQRTPS
        | `Int 1278 -> VSQRTSD
        | `Int 1279 -> VSQRTSS
        | `Int 1280 -> VSTMXCSR
        | `Int 1281 -> VSUBPD
        | `Int 1282 -> VSUBPS
        | `Int 1283 -> VSUBSD
        | `Int 1284 -> VSUBSS
        | `Int 1285 -> VTESTPD
        | `Int 1286 -> VTESTPS
        | `Int 1287 -> VUNPCKHPD
        | `Int 1288 -> VUNPCKHPS
        | `Int 1289 -> VUNPCKLPD
        | `Int 1290 -> VUNPCKLPS
        | `Int 1291 -> VZEROALL
        | `Int 1292 -> VZEROUPPER
        | `Int 1293 -> WAIT
        | `Int 1294 -> WBINVD
        | `Int 1295 -> WRFSBASE
        | `Int 1296 -> WRGSBASE
        | `Int 1297 -> WRMSR
        | `Int 1298 -> XABORT
        | `Int 1299 -> XACQUIRE
        | `Int 1300 -> XBEGIN
        | `Int 1301 -> XCHG
        | `Int 1302 -> XCRYPTCBC
        | `Int 1303 -> XCRYPTCFB
        | `Int 1304 -> XCRYPTCTR
        | `Int 1305 -> XCRYPTECB
        | `Int 1306 -> XCRYPTOFB
        | `Int 1307 -> XEND
        | `Int 1308 -> XGETBV
        | `Int 1309 -> XLATB
        | `Int 1310 -> XRELEASE
        | `Int 1311 -> XRSTOR
        | `Int 1312 -> XRSTOR64
        | `Int 1313 -> XRSTORS
        | `Int 1314 -> XRSTORS64
        | `Int 1315 -> XSAVE
        | `Int 1316 -> XSAVE64
        | `Int 1317 -> XSAVEC
        | `Int 1318 -> XSAVEC64
        | `Int 1319 -> XSAVEOPT
        | `Int 1320 -> XSAVEOPT64
        | `Int 1321 -> XSAVES
        | `Int 1322 -> 	XSAVES64
        | `Int 1323 -> XSETBV
        | `Int 1324 -> XSHA1
        | `Int 1325 -> XSHA256
        | `Int 1326 -> XSTORE
        | `Int 1327 -> XTEST
        | `Int 1328 -> FDISI8087_NOP
        | `Int 1329 -> FENI8087_NOP
        | `Int 1330 -> CMPSS
        | `Int 1331 -> CMPEQSS
        | `Int 1332 -> CMPLTSS
        | `Int 1333 -> CMPLESS
        | `Int 1334 -> CMPUNORDSS
        | `Int 1335 -> CMPNEQSS
        | `Int 1336 -> CMPNLTSS
        | `Int 1337 -> CMPNLESS
        | `Int 1338 -> CMPORDSS
        | `Int 1339 -> CMPSD
        | `Int 1340 -> CMPEQSD
        | `Int 1341 -> CMPLTSD
        | `Int 1342 -> CMPLESD
        | `Int 1343 -> CMPUNORDSD
        | `Int 1344 -> CMPNEQSD
        | `Int 1345 -> CMPNLTSD
        | `Int 1346 -> CMPNLESD
        | `Int 1347 -> CMPORDSD
        | `Int 1348 -> CMPPS
        | `Int 1349 -> CMPEQPS
        | `Int 1350 -> CMPLTPS
        | `Int 1351 -> CMPLEPS
        | `Int 1352 -> CMPUNORDPS
        | `Int 1353 -> CMPNEQPS
        | `Int 1354 -> CMPNLTPS
        | `Int 1355 -> CMPNLEPS
        | `Int 1356 -> CMPORDPS
        | `Int 1357 -> CMPPD
        | `Int 1358 -> CMPEQPD
        | `Int 1359 -> CMPLTPD
        | `Int 1360 -> CMPLEPD
        | `Int 1361 -> CMPUNORDPD
        | `Int 1362 -> CMPNEQPD
        | `Int 1363 -> CMPNLTPD
        | `Int 1364 -> CMPNLEPD
        | `Int 1365 -> CMPORDPD
        | `Int 1366 -> VCMPSS
        | `Int 1367 -> VCMPEQSS
        | `Int 1368 -> VCMPLTSS
        | `Int 1369 -> VCMPLESS
        | `Int 1370 -> VCMPUNORDSS
        | `Int 1371 -> VCMPNEQSS
        | `Int 1372 -> VCMPNLTSS
        | `Int 1373 -> VCMPNLESS
        | `Int 1374 -> VCMPORDSS
        | `Int 1375 -> VCMPEQ_UQSS
        | `Int 1376 -> VCMPNGESS
        | `Int 1377 -> VCMPNGTSS
        | `Int 1378 -> VCMPFALSESS
        | `Int 1379 -> VCMPNEQ_OQSS
        | `Int 1380 -> VCMPGESS
        | `Int 1381 -> VCMPGTSS
        | `Int 1382 -> VCMPTRUESS
        | `Int 1383 -> VCMPEQ_OSSS
        | `Int 1384 -> VCMPLT_OQSS
        | `Int 1385 -> VCMPLE_OQSS
        | `Int 1386 -> VCMPUNORD_SSS
        | `Int 1387 -> VCMPNEQ_USSS
        | `Int 1388 -> VCMPNLT_UQSS
        | `Int 1389 -> VCMPNLE_UQSS
        | `Int 1390 -> VCMPORD_SSS
        | `Int 1391 -> VCMPEQ_USSS
        | `Int 1392 -> VCMPNGE_UQSS
        | `Int 1393 -> VCMPNGT_UQSS
        | `Int 1394 -> VCMPFALSE_OSSS
        | `Int 1395 -> VCMPNEQ_OSSS
        | `Int 1396 -> VCMPGE_OQSS
        | `Int 1397 -> VCMPGT_OQSS
        | `Int 1398 -> VCMPTRUE_USSS
        | `Int 1399 -> VCMPSD
        | `Int 1400 -> VCMPEQSD
        | `Int 1401 -> VCMPLTSD
        | `Int 1402 -> VCMPLESD
        | `Int 1403 -> VCMPUNORDSD
        | `Int 1404 -> VCMPNEQSD
        | `Int 1405 -> VCMPNLTSD
        | `Int 1406 -> VCMPNLESD
        | `Int 1407 -> VCMPORDSD
        | `Int 1408 -> VCMPEQ_UQSD
        | `Int 1409 -> VCMPNGESD
        | `Int 1410 -> VCMPNGTSD
        | `Int 1411 -> VCMPFALSESD
        | `Int 1412 -> VCMPNEQ_OQSD
        | `Int 1413 -> VCMPGESD
        | `Int 1414 -> VCMPGTSD
        | `Int 1415 -> VCMPTRUESD
        | `Int 1416 -> VCMPEQ_OSSD
        | `Int 1417 -> VCMPLT_OQSD
        | `Int 1418 -> VCMPLE_OQSD
        | `Int 1419 -> VCMPUNORD_SSD
        | `Int 1420 -> VCMPNEQ_USSD
        | `Int 1421 -> VCMPNLT_UQSD
        | `Int 1422 -> VCMPNLE_UQSD
        | `Int 1423 -> VCMPORD_SSD
        | `Int 1424 -> VCMPEQ_USSD
        | `Int 1425 -> VCMPNGE_UQSD
        | `Int 1426 -> VCMPNGT_UQSD
        | `Int 1427 -> VCMPFALSE_OSSD
        | `Int 1428 -> VCMPNEQ_OSSD
        | `Int 1429 -> VCMPGE_OQSD
        | `Int 1430 -> VCMPGT_OQSD
        | `Int 1431 -> VCMPTRUE_USSD
        | `Int 1432 -> VCMPPS
        | `Int 1433 -> VCMPEQPS
        | `Int 1434 -> VCMPLTPS
        | `Int 1435 -> VCMPLEPS
        | `Int 1436 -> VCMPUNORDPS
        | `Int 1437 -> VCMPNEQPS
        | `Int 1438 -> VCMPNLTPS
        | `Int 1439 -> VCMPNLEPS
        | `Int 1440 -> VCMPORDPS
        | `Int 1441 -> VCMPEQ_UQPS
        | `Int 1442 -> VCMPNGEPS
        | `Int 1443 -> VCMPNGTPS
        | `Int 1444 -> VCMPFALSEPS
        | `Int 1445 -> VCMPNEQ_OQPS
        | `Int 1446 -> VCMPGEPS
        | `Int 1447 -> VCMPGTPS
        | `Int 1448 -> VCMPTRUEPS
        | `Int 1449 -> VCMPEQ_OSPS
        | `Int 1450 -> VCMPLT_OQPS
        | `Int 1451 -> VCMPLE_OQPS
        | `Int 1452 -> VCMPUNORD_SPS
        | `Int 1453 -> VCMPNEQ_USPS
        | `Int 1454 -> VCMPNLT_UQPS
        | `Int 1455 -> VCMPNLE_UQPS
        | `Int 1456 -> VCMPORD_SPS
        | `Int 1457 -> VCMPEQ_USPS
        | `Int 1458 -> VCMPNGE_UQPS
        | `Int 1459 -> VCMPNGT_UQPS
        | `Int 1460 -> VCMPFALSE_OSPS
        | `Int 1461 -> VCMPNEQ_OSPS
        | `Int 1462 -> VCMPGE_OQPS
        | `Int 1463 -> VCMPGT_OQPS
        | `Int 1464 -> VCMPTRUE_USPS
        | `Int 1465 -> VCMPPD
        | `Int 1466 -> VCMPEQPD
        | `Int 1467 -> VCMPLTPD
        | `Int 1468 -> VCMPLEPD
        | `Int 1469 -> VCMPUNORDPD
        | `Int 1470 -> VCMPNEQPD
        | `Int 1471 -> VCMPNLTPD
        | `Int 1472 -> VCMPNLEPD
        | `Int 1473 -> VCMPORDPD
        | `Int 1474 -> VCMPEQ_UQPD
        | `Int 1475 -> VCMPNGEPD
        | `Int 1476 -> VCMPNGTPD
        | `Int 1477 -> VCMPFALSEPD
        | `Int 1478 -> VCMPNEQ_OQPD
        | `Int 1479 -> VCMPGEPD
        | `Int 1480 -> VCMPGTPD
        | `Int 1481 -> VCMPTRUEPD
        | `Int 1482 -> VCMPEQ_OSPD
        | `Int 1483 -> VCMPLT_OQPD
        | `Int 1484 -> VCMPLE_OQPD
        | `Int 1485 -> VCMPUNORD_SPD
        | `Int 1486 -> VCMPNEQ_USPD
        | `Int 1487 -> VCMPNLT_UQPD
        | `Int 1488 -> VCMPNLE_UQPD
        | `Int 1489 -> VCMPORD_SPD
        | `Int 1490 -> VCMPEQ_USPD
        | `Int 1491 -> VCMPNGE_UQPD
        | `Int 1492 -> VCMPNGT_UQPD
        | `Int 1493 -> VCMPFALSE_OSPD
        | `Int 1494 -> VCMPNEQ_OSPD
        | `Int 1495 -> VCMPGE_OQPD
        | `Int 1496 -> VCMPGT_OQPD
        | `Int 1497 -> VCMPTRUE_USPD
        | `Int 1498 -> UD0
        | `Int 1499 -> ENDBR32
        | `Int 1500 -> ENDBR64
        | yojson -> raise @@ Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure "invalid instruction", yojson)


