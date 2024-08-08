open! Core
open Towhowcaml
module C = Radatnet.Commands

let c =
  lazy
    (let c = Radatnet.Core.create () in

     C.open_file c "/home/errast/code/Touhou7/th07.exe";
     C.analyze_all c LevelFour;
     c)

let intrinsics = make_intrinsics @@ force c
let translate = translate_func ~intrinsics @@ force c

let test_trans addr =
  let func = translate addr in
  print_s @@ Mir.Func.sexp_of_t @@ func;
  print_s @@ Mir.Structure_cfg.sexp_of_wasm_control
  @@ Mir.Structure_cfg.structure_cfg func

let test_trans_block addr =
  let name = func_name (force c) addr in
  let blocks =
    C.get_func_blocks (force c) addr |> Array.map ~f:(make_block @@ force c)
  in
  let index =
    Option.value_exn
    @@ Array.binary_search
         ~compare:(fun b addr -> compare_int b.offset addr)
         blocks `Last_less_than_or_equal_to addr
  in
  print_s @@ Mir.Block.sexp_of_t
  @@ Array.Permissioned.get
       (Func_translator.translate ~blocks ~name ~intrinsics).blocks index

let%expect_test "small regs" =
  test_trans_block 0x0047bd35;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var esi) (op Load32) (addr (Ref 0)) (offset 52)))
       (2 (LoadOp (var edi) (op Load32) (addr (Ref 0)) (offset 48)))
       (3 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)))
       (4 Nop) (5 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (6 Nop)
       (7
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 64)))
       (8 Nop) (9 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (10 Nop) (11 Nop)
       (12 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)))
       (13 Nop)
       (14 (BiOp (var __i32) (op Multiply) (lhs (Ref 3)) (rhs (Ref 12))))
       (15 Nop)
       (16
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 32)))
       (17 Nop) (18 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (19 Nop) (20 Nop)
       (21
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 64)))
       (22 Nop)
       (23 (BiOp (var __i32) (op Multiply) (lhs (Ref 7)) (rhs (Ref 21))))
       (24 Nop)
       (25
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 96)))
       (26 Nop) (27 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (28 Nop) (29 Nop)
       (30
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 32)))
       (31 Nop)
       (32 (BiOp (var __i32) (op Multiply) (lhs (Ref 16)) (rhs (Ref 30))))
       (33 Nop) (34 Nop)
       (35
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 96)))
       (36 Nop)
       (37 (BiOp (var __i32) (op Multiply) (lhs (Ref 25)) (rhs (Ref 35))))
       (38 Nop) (39 Nop)
       (40 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 14)) (offset 32)))
       (41 Nop) (42 Nop)
       (43 (BiOp (var __i32) (op Add) (lhs (Ref 14)) (rhs (Ref 23)))) (44 Nop)
       (45
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 32)))
       (46 Nop) (47 Nop)
       (48 (BiOp (var __i32) (op Subtract) (lhs (Ref 45)) (rhs (Ref 23))))
       (49 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 48)) (offset 32)))
       (50 Nop) (51 Nop) (52 Nop) (53 Nop)
       (54 (BiOp (var __i32) (op Add) (lhs (Ref 32)) (rhs (Ref 37)))) (55 Nop)
       (56 Nop) (57 Nop)
       (58 (BiOp (var __i32) (op Subtract) (lhs (Ref 32)) (rhs (Ref 37))))
       (59 (BiOp (var ebx) (op MergeTrunc16) (lhs (Ref 58)) (rhs (Ref 9))))
       (60 Nop) (61 Nop)
       (62 (UniOp (var ebx) (op SignExtend16) (operand (Ref 59)))) (63 Nop)
       (64 Nop) (65 (BiOp (var __i32) (op Add) (lhs (Ref 43)) (rhs (Ref 54))))
       (66 Nop) (67 (Const __i32 4785088))
       (68 (LoadOp (var __i32) (op Load32) (addr (Ref 67))))
       (69 (BiOp (var ebx) (op Multiply) (lhs (Ref 62)) (rhs (Ref 68))))
       (70 Nop) (71 Nop)
       (72 (BiOp (var __i32) (op Subtract) (lhs (Ref 43)) (rhs (Ref 54))))
       (73 Nop) (74 Nop)
       (75 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 65)) (offset 28)))
       (76 Nop)
       (77 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 72)) (offset 24)))
       (78 Nop) (79 (Const __i32 8))
       (80
        (SignedBiOp (var ebx) (op ShiftRight) (signed true) (lhs (Ref 69))
         (rhs (Ref 79))))
       (81
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 32)))
       (82 Nop) (83 Nop) (84 Nop) (85 Nop)
       (86 (BiOp (var __i32) (op Subtract) (lhs (Ref 80)) (rhs (Ref 54))))
       (87 Nop) (88 Nop) (89 Nop)
       (90 (BiOp (var __i32) (op Add) (lhs (Ref 81)) (rhs (Ref 86)))) (91 Nop)
       (92
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 32)))
       (93 Nop) (94 Nop)
       (95 (BiOp (var __i32) (op Subtract) (lhs (Ref 92)) (rhs (Ref 86))))
       (96 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 95)) (offset 32)))
       (97 Nop)
       (98 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 90)) (offset 20)))
       (99
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 16)))
       (100 Nop) (101 Nop) (102 Nop)
       (103
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 16)))
       (104 Nop)
       (105 (BiOp (var __i32) (op Multiply) (lhs (Ref 99)) (rhs (Ref 103))))
       (106 Nop)
       (107
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 112)))
       (108 Nop) (109 Nop)
       (110
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 48)))
       (111 Nop) (112 Nop) (113 Nop)
       (114
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 112)))
       (115 Nop)
       (116 (BiOp (var __i32) (op Multiply) (lhs (Ref 107)) (rhs (Ref 114))))
       (117 Nop)
       (118
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)
         (offset 80)))
       (119 Nop) (120 Nop) (121 Nop)
       (122
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 48)))
       (123 Nop)
       (124 (BiOp (var __i32) (op Multiply) (lhs (Ref 110)) (rhs (Ref 122))))
       (125 Nop) (126 Nop)
       (127
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 2)) (signed true)
         (offset 80)))
       (128 Nop)
       (129 (BiOp (var __i32) (op Multiply) (lhs (Ref 118)) (rhs (Ref 127))))
       (130 Nop) (131 Nop)
       (132 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 105)) (offset 16)))
       (133 Nop) (134 Nop)
       (135 (BiOp (var __i32) (op Add) (lhs (Ref 105)) (rhs (Ref 116))))
       (136 Nop)
       (137
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 16)))
       (138 Nop) (139 Nop)
       (140 (BiOp (var __i32) (op Subtract) (lhs (Ref 137)) (rhs (Ref 116))))
       (141 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 140)) (offset 16)))
       (142 Nop) (143 Nop) (144 Nop) (145 Nop)
       (146 (BiOp (var __i32) (op Add) (lhs (Ref 129)) (rhs (Ref 124))))
       (147 Nop) (148 Nop) (149 Nop)
       (150 (BiOp (var __i32) (op Subtract) (lhs (Ref 129)) (rhs (Ref 124))))
       (151 Nop) (152 Nop) (153 Nop) (154 Nop) (155 Nop)
       (156 (BiOp (var __i32) (op Add) (lhs (Ref 135)) (rhs (Ref 146))))
       (157 Nop) (158 Nop) (159 Nop)
       (160 (BiOp (var __i32) (op Subtract) (lhs (Ref 135)) (rhs (Ref 146))))
       (161 (BiOp (var ecx) (op MergeTrunc16) (lhs (Ref 160)) (rhs (Ref 18))))
       (162 (UniOp (var ecx) (op SignExtend16) (operand (Ref 161)))) (163 Nop)
       (164 (BiOp (var edx) (op MergeTrunc16) (lhs (Ref 150)) (rhs (Ref 27))))
       (165 Nop)
       (166
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 16)))
       (167 Nop)
       (168 (BiOp (var __i32) (op Add) (lhs (Ref 150)) (rhs (Ref 166))))
       (169 (BiOp (var ebx) (op MergeTrunc16) (lhs (Ref 168)) (rhs (Ref 80))))
       (170 (Const __i32 4785088))
       (171 (LoadOp (var __i32) (op Load32) (addr (Ref 170))))
       (172 (BiOp (var ecx) (op Multiply) (lhs (Ref 162)) (rhs (Ref 171))))
       (173 (UniOp (var edx) (op SignExtend16) (operand (Ref 164))))
       (174 (UniOp (var ebx) (op SignExtend16) (operand (Ref 169))))
       (175 (Const __i32 4785092))
       (176 (LoadOp (var __i32) (op Load32) (addr (Ref 175))))
       (177 (BiOp (var edx) (op Multiply) (lhs (Ref 173)) (rhs (Ref 176))))
       (178 (Const __i32 4785100))
       (179 (LoadOp (var __i32) (op Load32) (addr (Ref 178))))
       (180 (BiOp (var ebx) (op Multiply) (lhs (Ref 174)) (rhs (Ref 179))))
       (181
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 16)))
       (182 Nop) (183 Nop) (184 Nop) (185 Nop) (186 (Const __i32 8))
       (187
        (SignedBiOp (var ecx) (op ShiftRight) (signed true) (lhs (Ref 172))
         (rhs (Ref 186))))
       (188 Nop) (189 (Const __i32 8))
       (190
        (SignedBiOp (var ebx) (op ShiftRight) (signed true) (lhs (Ref 180))
         (rhs (Ref 189))))
       (191 (Const __i32 4785096))
       (192 (LoadOp (var __i32) (op Load32) (addr (Ref 191))))
       (193 (BiOp (var edi) (op Multiply) (lhs (Ref 181)) (rhs (Ref 192))))
       (194 Nop) (195 (Const __i32 8))
       (196
        (SignedBiOp (var edx) (op ShiftRight) (signed true) (lhs (Ref 177))
         (rhs (Ref 195))))
       (197 Nop) (198 (Const __i32 8))
       (199
        (SignedBiOp (var edi) (op ShiftRight) (signed true) (lhs (Ref 193))
         (rhs (Ref 198))))
       (200 Nop) (201 Nop)
       (202 (BiOp (var __i32) (op Subtract) (lhs (Ref 199)) (rhs (Ref 190))))
       (203 Nop) (204 Nop) (205 Nop)
       (206 (BiOp (var __i32) (op Add) (lhs (Ref 196)) (rhs (Ref 190))))
       (207 Nop) (208 Nop) (209 Nop)
       (210 (BiOp (var __i32) (op Subtract) (lhs (Ref 206)) (rhs (Ref 156))))
       (211 Nop) (212 Nop) (213 Nop)
       (214 (BiOp (var __i32) (op Subtract) (lhs (Ref 187)) (rhs (Ref 210))))
       (215 Nop) (216 Nop) (217 Nop)
       (218 (BiOp (var __i32) (op Add) (lhs (Ref 202)) (rhs (Ref 214))))
       (219 Nop) (220 Nop)
       (221 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 218)) (offset 12)))
       (222 (LoadOp (var edi) (op Load32) (addr (Ref 0)) (offset 44))) (223 Nop)
       (224 (BiOp (var esi) (op MergeTrunc16) (lhs (Ref 156)) (rhs (Ref 1))))
       (225
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 28)))
       (226 Nop) (227 Nop) (228 Nop)
       (229
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 28)))
       (230 Nop)
       (231 (BiOp (var __i32) (op Add) (lhs (Ref 156)) (rhs (Ref 229))))
       (232 Nop) (233 Nop) (234 Nop)
       (235 (BiOp (var __i32) (op Subtract) (lhs (Ref 225)) (rhs (Ref 156))))
       (236 Nop) (237 Nop)
       (238 (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 231)))) (239 Nop)
       (240
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 235)) (offset 112)))
       (241 Nop) (242 Nop)
       (243
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 20)))
       (244 Nop) (245 Nop) (246 Nop) (247 Nop)
       (248 (BiOp (var __i32) (op Add) (lhs (Ref 210)) (rhs (Ref 243))))
       (249 Nop) (250 Nop) (251 Nop)
       (252 (BiOp (var __i32) (op Subtract) (lhs (Ref 243)) (rhs (Ref 210))))
       (253 Nop) (254 Nop)
       (255
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 248)) (offset 16)))
       (256 Nop)
       (257
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 252)) (offset 96)))
       (258 Nop)
       (259 (BiOp (var edx) (op MergeTrunc16) (lhs (Ref 214)) (rhs (Ref 196))))
       (260
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 32)))
       (261 Nop) (262 Nop) (263 Nop) (264 Nop)
       (265 (BiOp (var __i32) (op Add) (lhs (Ref 214)) (rhs (Ref 260))))
       (266 Nop) (267 Nop) (268 Nop)
       (269 (BiOp (var __i32) (op Subtract) (lhs (Ref 260)) (rhs (Ref 214))))
       (270 Nop) (271 Nop)
       (272
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 265)) (offset 32)))
       (273 Nop)
       (274
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 269)) (offset 80)))
       (275
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 12)))
       (276 Nop)
       (277 (BiOp (var ecx) (op MergeTrunc16) (lhs (Ref 275)) (rhs (Ref 187))))
       (278
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 24)))
       (279 Nop) (280 Nop)
       (281
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 12)))
       (282 Nop) (283 Nop)
       (284 (BiOp (var __i32) (op Add) (lhs (Ref 281)) (rhs (Ref 278))))
       (285 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 284)) (offset 12)))
       (286 Nop) (287 Nop)
       (288 (BiOp (var __i32) (op Subtract) (lhs (Ref 278)) (rhs (Ref 275))))
       (289 Nop)
       (290
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 12)))
       (291 Nop)
       (292 (BiOp (var ebx) (op MergeTrunc16) (lhs (Ref 290)) (rhs (Ref 190))))
       (293 Nop)
       (294
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 290)) (offset 64)))
       (295 Nop)
       (296
        (StoreOp (op Store16) (addr (Ref 222)) (value (Ref 288)) (offset 48)))
       (297 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 52)))
       (298 (Const __i32 2))
       (299 (BiOp (var __i32) (op Add) (lhs (Ref 297)) (rhs (Ref 298))))
       (300 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 299)) (offset 52)))
       (301 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 48)))
       (302 (Const __i32 2))
       (303 (BiOp (var __i32) (op Add) (lhs (Ref 301)) (rhs (Ref 302))))
       (304 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 303)) (offset 48)))
       (305 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 44)))
       (306 (Const __i32 2))
       (307 (BiOp (var __i32) (op Add) (lhs (Ref 305)) (rhs (Ref 306))))
       (308 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 307)) (offset 44)))
       (309
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset 36)))
       (310 Nop) (311 Nop) (312 (Const __i32 1)) (313 Nop)
       (314 (BiOp (var __i32) (op Subtract) (lhs (Ref 309)) (rhs (Ref 312))))
       (315 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 314)) (rhs (Ref 5))))
       (316 Nop)
       (317 (StoreOp (op Store16) (addr (Ref 0)) (value (Ref 314)) (offset 36)))
       (318 (UniOp (var __i32) (op SignExtend16) (operand (Ref 314))))
       (319 (SetGlobalOp (value (Ref 259)) (global ((name edx) (typ Int)))))
       (320 (SetGlobalOp (value (Ref 277)) (global ((name ecx) (typ Int)))))
       (321 (SetGlobalOp (value (Ref 224)) (global ((name esi) (typ Int)))))
       (322 (SetGlobalOp (value (Ref 315)) (global ((name eax) (typ Int)))))
       (323 (SetGlobalOp (value (Ref 222)) (global ((name edi) (typ Int)))))
       (324 (SetGlobalOp (value (Ref 292)) (global ((name ebx) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 318))))
     (roots ((Ref 0) (Ref 318))))
    |}]

let%expect_test "_sd" =
  test_trans_block 0x00484bb6;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (1 (GetGlobalOp (var xmm1) (global ((name xmm1) (typ Vec)))))
       (2
        (VecReplaceLaneOp (var xmm1) (dest (Ref 1)) (lane_value (Ref 0))
         (shape I32) (lane 0)))
       (3 (GetGlobalOp (var xmm0) (global ((name xmm0) (typ Vec)))))
       (4
        (VecLaneBiOp (var xmm0) (op VecAdd) (shape I64) (lhs (Ref 3))
         (rhs (Ref 2))))
       (5 (Const __i32 8))
       (6
        (VecShiftRightOp (var xmm0) (operand (Ref 4)) (count (Ref 5)) (shape I64)
         (signed false)))
       (7
        (UniOp (var xmm0) (op VecConvertLow32BitsToFloatsSigned)
         (operand (Ref 6))))
       (8 (Const __i32 4803656))
       (9
        (VecLoadLaneOp (var xmm1) (dest_vec (Ref 2)) (addr (Ref 8)) (shape I64)
         (lane 0)))
       (10 (GetGlobalOp (var xmm3) (global ((name xmm3) (typ Vec)))))
       (11 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec))))) (12 Nop)
       (13 Nop) (14 (Const __i32 38))
       (15
        (VecShiftRightOp (var xmm7) (operand (Ref 11)) (count (Ref 14))
         (shape I64) (signed false)))
       (16 (VecExtractLaneOp (var eax) (src (Ref 15)) (shape I16) (lane 0)))
       (17 (Const __i32 4803568))
       (18 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 17))))
       (19 (GetGlobalOp (var xmm4) (global ((name xmm4) (typ Vec)))))
       (20 (BiOp (var xmm4) (op VecAnd) (lhs (Ref 19)) (rhs (Ref 18))))
       (21 (Const __i32 255))
       (22 (BiOp (var eax) (op And) (lhs (Ref 16)) (rhs (Ref 21))))
       (23 (Const __i32 1))
       (24 (BiOp (var eax) (op Add) (lhs (Ref 22)) (rhs (Ref 23))))
       (25 (Const __i32 510))
       (26 (BiOp (var eax) (op And) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (Const __i32 4))
       (28 (BiOp (var __i32) (op Multiply) (lhs (Ref 26)) (rhs (Ref 27))))
       (29
        (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 28))
         (offset 4792192)))
       (30
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 11))
         (rhs (Ref 29))))
       (31 (VecExtractLaneOp (var __i64) (src (Ref 10)) (shape I64) (lane 1)))
       (32
        (VecReplaceLaneOp (var xmm3) (dest (Ref 30)) (lane_value (Ref 31))
         (shape I64) (lane 1)))
       (33 (GetGlobalOp (var xmm5) (global ((name xmm5) (typ Vec)))))
       (34 (Const __i32 4))
       (35 (BiOp (var __i32) (op Multiply) (lhs (Ref 26)) (rhs (Ref 34))))
       (36
        (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 35))
         (offset 4792192)))
       (37
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 33))
         (rhs (Ref 36))))
       (38 Nop) (39 Nop)
       (40 (BiOp (var eax) (op Add) (lhs (Ref 26)) (rhs (Ref 26))))
       (41 (Const __i32 4))
       (42 (BiOp (var __i32) (op Multiply) (lhs (Ref 40)) (rhs (Ref 41))))
       (43 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 42)) (offset 4793232)))
       (44 (GetGlobalOp (var xmm6) (global ((name xmm6) (typ Vec)))))
       (45
        (VecLaneBiOp (var xmm6) (op VecAdd) (shape F64) (lhs (Ref 44))
         (rhs (Ref 43))))
       (46 (Const __i32 4803584))
       (47 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 46))))
       (48 (BiOp (var xmm4) (op VecOr) (lhs (Ref 20)) (rhs (Ref 47))))
       (49
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 45))
         (rhs (Ref 7))))
       (50 (VecExtractLaneOp (var __i64) (src (Ref 45)) (shape I64) (lane 1)))
       (51
        (VecReplaceLaneOp (var xmm6) (dest (Ref 49)) (lane_value (Ref 50))
         (shape I64) (lane 1)))
       (52 (BiOp (var xmm1) (op VecAnd) (lhs (Ref 9)) (rhs (Ref 48))))
       (53 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec))))) (54 Nop)
       (55 Nop) (56 (Const __i32 31))
       (57
        (VecShiftRightOp (var xmm3) (operand (Ref 32)) (count (Ref 56))
         (shape I64) (signed false)))
       (58 (VecExtractLaneOp (var eax) (src (Ref 57)) (shape I16) (lane 0)))
       (59 (Const __i32 4803656))
       (60
        (VecLoadLaneOp (var xmm0) (dest_vec (Ref 7)) (addr (Ref 59)) (shape I64)
         (lane 0)))
       (61
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 48))
         (rhs (Ref 52))))
       (62 Nop) (63 Nop) (64 (Const __i32 4803664))
       (65
        (VecLoadLaneOp (var xmm7) (dest_vec (Ref 15)) (addr (Ref 64)) (shape I64)
         (lane 0)))
       (66 (Const __i32 511))
       (67 (BiOp (var eax) (op And) (lhs (Ref 58)) (rhs (Ref 66))))
       (68 (Const __i32 1))
       (69 (BiOp (var eax) (op Add) (lhs (Ref 67)) (rhs (Ref 68))))
       (70 (Const __i32 1022))
       (71 (BiOp (var eax) (op And) (lhs (Ref 69)) (rhs (Ref 70))))
       (72 (Const __i32 4))
       (73 (BiOp (var __i32) (op Multiply) (lhs (Ref 71)) (rhs (Ref 72))))
       (74
        (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 73))
         (offset 4795296)))
       (75
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 37))
         (rhs (Ref 74))))
       (76 (VecExtractLaneOp (var __i64) (src (Ref 33)) (shape I64) (lane 1)))
       (77
        (VecReplaceLaneOp (var xmm5) (dest (Ref 75)) (lane_value (Ref 76))
         (shape I64) (lane 1)))
       (78 (Const __i32 4))
       (79 (BiOp (var __i32) (op Multiply) (lhs (Ref 71)) (rhs (Ref 78))))
       (80
        (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 79))
         (offset 4795296)))
       (81
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 30))
         (rhs (Ref 80))))
       (82 Nop) (83 Nop) (84 (Const __i32 8))
       (85 (BiOp (var __i32) (op Multiply) (lhs (Ref 71)) (rhs (Ref 84))))
       (86 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 85)) (offset 4797360)))
       (87
        (VecLaneBiOp (var xmm6) (op VecAdd) (shape F64) (lhs (Ref 51))
         (rhs (Ref 86))))
       (88 (BiOp (var xmm0) (op VecAnd) (lhs (Ref 60)) (rhs (Ref 77))))
       (89
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 75))
         (rhs (Ref 88))))
       (90 (VecExtractLaneOp (var __i64) (src (Ref 33)) (shape I64) (lane 1)))
       (91
        (VecReplaceLaneOp (var xmm5) (dest (Ref 89)) (lane_value (Ref 90))
         (shape I64) (lane 1)))
       (92
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 65))
         (rhs (Ref 81))))
       (93 Nop) (94 Nop) (95 Nop) (96 Nop)
       (97
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 88))
         (rhs (Ref 52))))
       (98 Nop) (99 Nop)
       (100
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 52))
         (rhs (Ref 89))))
       (101 (VecExtractLaneOp (var __i64) (src (Ref 52)) (shape I64) (lane 1)))
       (102
        (VecReplaceLaneOp (var xmm1) (dest (Ref 100)) (lane_value (Ref 101))
         (shape I64) (lane 1)))
       (103
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 88))
         (rhs (Ref 61))))
       (104 (VecExtractLaneOp (var __i64) (src (Ref 57)) (shape I64) (lane 1)))
       (105
        (VecReplaceLaneOp (var xmm3) (dest (Ref 103)) (lane_value (Ref 104))
         (shape I64) (lane 1)))
       (106
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 81))
         (rhs (Ref 97))))
       (107 Nop) (108 Nop)
       (109
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 61))
         (rhs (Ref 89))))
       (110 Nop) (111 Nop) (112 Nop) (113 Nop)
       (114
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 106))
         (rhs (Ref 100))))
       (115 Nop) (116 Nop)
       (117
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 87))
         (rhs (Ref 92))))
       (118 (VecExtractLaneOp (var __i64) (src (Ref 87)) (shape I64) (lane 1)))
       (119
        (VecReplaceLaneOp (var xmm6) (dest (Ref 117)) (lane_value (Ref 118))
         (shape I64) (lane 1)))
       (120 (OutsideContext (var esp) (typ Int)))
       (121
        (VecLoadLaneOp (var xmm1) (dest_vec (Ref 102)) (addr (Ref 120))
         (shape I64) (lane 0) (offset 12)))
       (122 (VecExtractLaneOp (var eax) (src (Ref 121)) (shape I16) (lane 3)))
       (123
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 114))
         (rhs (Ref 103))))
       (124 Nop) (125 Nop)
       (126
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 87))
         (rhs (Ref 117))))
       (127 Nop) (128 Nop) (129 (Const __i32 4803656))
       (130
        (VecLoadLaneOp (var xmm3) (dest_vec (Ref 105)) (addr (Ref 129))
         (shape I64) (lane 0)))
       (131 (VecExtractLaneOp (var edx) (src (Ref 119)) (shape I16) (lane 3)))
       (132
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 123))
         (rhs (Ref 109))))
       (133 (VecExtractLaneOp (var __i64) (src (Ref 53)) (shape I64) (lane 1)))
       (134
        (VecReplaceLaneOp (var xmm2) (dest (Ref 132)) (lane_value (Ref 133))
         (shape I64) (lane 1)))
       (135 (VecExtractLaneOp (var __i64) (src (Ref 117)) (shape I64) (lane 0)))
       (136
        (VecReplaceLaneOp (var xmm4) (dest (Ref 48)) (lane_value (Ref 135))
         (shape I64) (lane 0)))
       (137
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 126))
         (rhs (Ref 92))))
       (138 (VecExtractLaneOp (var __i64) (src (Ref 88)) (shape I64) (lane 1)))
       (139
        (VecReplaceLaneOp (var xmm0) (dest (Ref 137)) (lane_value (Ref 138))
         (shape I64) (lane 1)))
       (140
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 92))
         (rhs (Ref 132))))
       (141 (VecExtractLaneOp (var __i64) (src (Ref 65)) (shape I64) (lane 1)))
       (142
        (VecReplaceLaneOp (var xmm7) (dest (Ref 140)) (lane_value (Ref 141))
         (shape I64) (lane 1)))
       (143
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 117))
         (rhs (Ref 132))))
       (144 (VecExtractLaneOp (var __i64) (src (Ref 87)) (shape I64) (lane 1)))
       (145
        (VecReplaceLaneOp (var xmm6) (dest (Ref 143)) (lane_value (Ref 144))
         (shape I64) (lane 1)))
       (146
        (VecShuffleOp (var xmm7) (arg1 (Ref 142)) (arg2 (Ref 142))
         (control_lower_bits 506097522914230528)
         (control_upper_bits 1663540288323457296)))
       (147 (Const __i32 32752))
       (148 (BiOp (var eax) (op And) (lhs (Ref 122)) (rhs (Ref 147))))
       (149 (Const __i32 32752))
       (150
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 148)) (rhs (Ref 149))))
       (151 (SetGlobalOp (value (Ref 131)) (global ((name edx) (typ Int)))))
       (152 (SetGlobalOp (value (Ref 134)) (global ((name xmm2) (typ Vec)))))
       (153 (SetGlobalOp (value (Ref 130)) (global ((name xmm3) (typ Vec)))))
       (154 (SetGlobalOp (value (Ref 148)) (global ((name eax) (typ Int)))))
       (155 (SetGlobalOp (value (Ref 139)) (global ((name xmm0) (typ Vec)))))
       (156 (SetGlobalOp (value (Ref 146)) (global ((name xmm7) (typ Vec)))))
       (157 (SetGlobalOp (value (Ref 145)) (global ((name xmm6) (typ Vec)))))
       (158 (SetGlobalOp (value (Ref 121)) (global ((name xmm1) (typ Vec)))))
       (159 (SetGlobalOp (value (Ref 136)) (global ((name xmm4) (typ Vec)))))
       (160 (SetGlobalOp (value (Ref 91)) (global ((name xmm5) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 39)) (fail (Block 3)) (condition (Ref 150))))
     (roots ((Ref 120) (Ref 150))))
    |}]

let%expect_test "bt" =
  test_trans_block 0x00486fa8;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (1 (Const __i32 1))
       (2 (BiOp (var esi) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (OutsideContext (var esp) (typ Int)))
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 3)))) (5 (Const __i32 1))
       (6 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (7 (Const __i32 31))
       (8 (BiOp (var __i32) (op And) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 5)) (rhs (Ref 8))))
       (10 (BiOp (var __i32) (op And) (lhs (Ref 4)) (rhs (Ref 9))))
       (11 (UniOp (var __i32) (op EqualsZero) (operand (Ref 10))))
       (12 (SetGlobalOp (value (Ref 2)) (global ((name esi) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 6)) (condition (Ref 11))))
     (roots ((Ref 3) (Ref 11))))
    |}]

let%expect_test "bts" =
  test_trans_block 0x00486f8d;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (1 (Const __i32 1))
       (2 (BiOp (var edx) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (OutsideContext (var esp) (typ Int)))
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 3)))) (5 (Const __i32 1))
       (6 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (7 (Const __i32 31))
       (8 (BiOp (var __i32) (op And) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 5)) (rhs (Ref 8))))
       (10 (BiOp (var __i32) (op Or) (lhs (Ref 4)) (rhs (Ref 9))))
       (11 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 10))))
       (12 (SetGlobalOp (value (Ref 2)) (global ((name edx) (typ Int)))))))
     (terminator (Goto (Block 1))) (roots ((Ref 3))))
    |}]

let%expect_test "rol, xlatb" =
  test_trans_block 0x00483ad4;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int))))) (1 Nop)
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (StoreOp (op Store16) (addr (Ref 2)) (value (Ref 0)) (offset -162)))
       (4 (Const ebx 4841516))
       (5
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
       (7 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (8 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 7)) (offset -148)))
       (9 (Const __i32 0))
       (10 (StoreOp (op Store16) (addr (Ref 2)) (value (Ref 9)) (offset -160)))
       (11 Nop) (12 (Const __i32 0))
       (13 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 12)) (offset -144)))
       (14
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 2)) (signed true)
         (offset -159)))
       (15 Nop) (16 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (17 Nop) (18 Nop) (19 (Const __i32 1))
       (20 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 14)) (rhs (Ref 19))))
       (21 Nop) (22 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 20))))
       (23 Nop) (24 (Const __i32 1))
       (25
        (SignedBiOp (var __i32) (op ShiftRight) (signed true) (lhs (Ref 22))
         (rhs (Ref 24))))
       (26 Nop) (27 (Const __i32 1))
       (28 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 25))))
       (29 (BiOp (var __i32) (op RotateLeft) (lhs (Ref 28)) (rhs (Ref 27))))
       (30 (BiOp (var ecx) (op MergeTruncLow8) (lhs (Ref 29)) (rhs (Ref 16))))
       (31 Nop) (32 Nop) (33 Nop) (34 Nop) (35 Nop) (36 (Const __i32 15))
       (37 (BiOp (var __i32) (op And) (lhs (Ref 29)) (rhs (Ref 36)))) (38 Nop)
       (39 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 37))))
       (40 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 39))))
       (41 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 40)) (signed true)))
       (42 Nop) (43 Nop) (44 (Const __i32 1028))
       (45 (BiOp (var ecx) (op And) (lhs (Ref 30)) (rhs (Ref 44)))) (46 Nop)
       (47 (BiOp (var ebx) (op Add) (lhs (Ref 7)) (rhs (Ref 41))))
       (48 (Const __i32 16))
       (49 (BiOp (var ebx) (op Add) (lhs (Ref 47)) (rhs (Ref 48))))
       (50 (LoadOp (var __i32) (op Load32) (addr (Ref 49))))
       (51 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 6))))
       (52 (SetGlobalOp (value (Ref 45)) (global ((name ecx) (typ Int)))))
       (53 (SetGlobalOp (value (Ref 41)) (global ((name eax) (typ Int)))))
       (54 (SetGlobalOp (value (Ref 49)) (global ((name ebx) (typ Int)))))
       (55 (OutsideContext (var esp) (typ Int)))
       (56 (CallIndirectOp (table_index (Ref 50)) (args ((Ref 55)))))
       (57 (ReturnedOp (var esp) (typ Int)))))
     (terminator Return) (roots ((Ref 57))))
    |}]

let%expect_test "shufpd" =
  test_trans_block 0x0048be1e;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec))))) (1 Nop)
       (2
        (VecLaneBiOp (var xmm1) (op VecMul) (shape F64) (lhs (Ref 0))
         (rhs (Ref 0))))
       (3 Nop)
       (4
        (VecLaneBiOp (var xmm3) (op VecMul) (shape F64) (lhs (Ref 2))
         (rhs (Ref 2))))
       (5 (Const __i32 4820576))
       (6 (LoadOp (var xmm5) (op VecLoad128) (addr (Ref 5))))
       (7
        (VecLaneBiOp (var xmm5) (op VecMul) (shape F64) (lhs (Ref 6))
         (rhs (Ref 4))))
       (8 (Const __i32 4820560))
       (9 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 8))))
       (10
        (VecLaneBiOp (var xmm5) (op VecAdd) (shape F64) (lhs (Ref 7))
         (rhs (Ref 9))))
       (11
        (VecLaneBiOp (var xmm5) (op VecMul) (shape F64) (lhs (Ref 10))
         (rhs (Ref 4))))
       (12 (Const __i32 4820544))
       (13 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 12))))
       (14
        (VecLaneBiOp (var xmm5) (op VecAdd) (shape F64) (lhs (Ref 11))
         (rhs (Ref 13))))
       (15
        (VecLaneBiOp (var xmm5) (op VecMul) (shape F64) (lhs (Ref 14))
         (rhs (Ref 4))))
       (16 (Const __i32 4820528))
       (17 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 16))))
       (18
        (VecLaneBiOp (var xmm5) (op VecAdd) (shape F64) (lhs (Ref 15))
         (rhs (Ref 17))))
       (19
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 18))
         (rhs (Ref 2))))
       (20 (VecExtractLaneOp (var __i64) (src (Ref 18)) (shape I64) (lane 1)))
       (21
        (VecReplaceLaneOp (var xmm5) (dest (Ref 19)) (lane_value (Ref 20))
         (shape I64) (lane 1)))
       (22 Nop)
       (23
        (VecShuffleOp (var xmm3) (arg1 (Ref 21)) (arg2 (Ref 21))
         (control_lower_bits 1084818905618843912)
         (control_upper_bits 1663540288323457296)))
       (24
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 19))
         (rhs (Ref 23))))
       (25 Nop) (26 Nop)
       (27 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec)))))
       (28
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 24))
         (rhs (Ref 27))))
       (29 (VecExtractLaneOp (var __i64) (src (Ref 18)) (shape I64) (lane 1)))
       (30
        (VecReplaceLaneOp (var xmm5) (dest (Ref 28)) (lane_value (Ref 29))
         (shape I64) (lane 1)))
       (31
        (VecLaneBiOp (var __vec) (op VecSub) (shape F64) (lhs (Ref 27))
         (rhs (Ref 28))))
       (32 (VecExtractLaneOp (var __i64) (src (Ref 27)) (shape I64) (lane 1)))
       (33
        (VecReplaceLaneOp (var xmm7) (dest (Ref 31)) (lane_value (Ref 32))
         (shape I64) (lane 1)))
       (34 (VecExtractLaneOp (var __i64) (src (Ref 31)) (shape I64) (lane 0)))
       (35 (OutsideContext (var esp) (typ Int)))
       (36
        (StoreOp (op LongStore64) (addr (Ref 35)) (value (Ref 34)) (offset 4)))
       (37 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 35)) (offset 4)))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 35))))
       (39 (OutsideContext (var __ret_addr__) (typ Int)))
       (40 (BiOp (var __i32) (op Equal) (lhs (Ref 38)) (rhs (Ref 39))))
       (41 (AssertOp (condition (Ref 40)))) (42 (Const __i32 4))
       (43 (BiOp (var esp) (op Add) (lhs (Ref 35)) (rhs (Ref 42))))
       (44
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (45
        (StoreOp (op FloatStore64) (addr (Ref 44)) (value (Ref 37)) (offset 8)))
       (46 (Const __i32 8))
       (47 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 44)) (rhs (Ref 46))))
       (48 (SetGlobalOp (value (Ref 23)) (global ((name xmm3) (typ Vec)))))
       (49
        (SetGlobalOp (value (Ref 47)) (global ((name __fpuStack__) (typ Int)))))
       (50 (SetGlobalOp (value (Ref 33)) (global ((name xmm7) (typ Vec)))))
       (51 (SetGlobalOp (value (Ref 2)) (global ((name xmm1) (typ Vec)))))
       (52 (SetGlobalOp (value (Ref 30)) (global ((name xmm5) (typ Vec)))))))
     (terminator Return) (roots ((Ref 39) (Ref 43))))
    |}]

let%expect_test "movq, comisd jae" =
  test_trans_block 0x0048bed8;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec)))))
       (1 (VecExtractLaneOp (var __fl) (src (Ref 0)) (shape F64) (lane 0)))
       (2 (GetGlobalOp (var xmm6) (global ((name xmm6) (typ Vec)))))
       (3
        (VecReplaceLaneOp (var xmm6) (dest (Ref 2)) (lane_value (Ref 1))
         (shape F64) (lane 0)))
       (4 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec)))))
       (5 (BiOp (var xmm6) (op VecXor) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (Const __i32 4820656))
       (7 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 6))))
       (8
        (SignedVecLaneBiOp (var __vec) (op VecGreaterThan) (signed true)
         (shape F64) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (VecExtractLaneOp (var __i32) (src (Ref 8)) (shape I32) (lane 0)))
       (10 (SetGlobalOp (value (Ref 5)) (global ((name xmm6) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 8)) (condition (Ref 9))))
     (roots ((Ref 9))))
    |}]

let%expect_test "comisd jp" =
  test_trans_block 0x0048bdb6;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec)))))
       (3
        (VecShuffleOp (var xmm7) (arg1 (Ref 2)) (arg2 (Ref 2))
         (control_lower_bits 506097522914230528)
         (control_upper_bits 1663540288323457296)))
       (4 Nop) (5 (Const __i32 4820352))
       (6 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 5))))
       (7 (BiOp (var xmm2) (op VecAnd) (lhs (Ref 3)) (rhs (Ref 6))))
       (8 (Const __i32 4820720))
       (9 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 8))))
       (10
        (VecLaneBiOp (var __vec) (op VecNotEqual) (shape F64) (lhs (Ref 9))
         (rhs (Ref 9))))
       (11
        (VecLaneBiOp (var __vec) (op VecNotEqual) (shape F64) (lhs (Ref 7))
         (rhs (Ref 7))))
       (12 (BiOp (var __vec) (op VecOr) (lhs (Ref 11)) (rhs (Ref 10))))
       (13 (VecExtractLaneOp (var __i32) (src (Ref 12)) (shape I32) (lane 0)))
       (14 (SetGlobalOp (value (Ref 7)) (global ((name xmm2) (typ Vec)))))
       (15 (SetGlobalOp (value (Ref 3)) (global ((name xmm7) (typ Vec)))))
       (16
        (SignedVecLaneBiOp (var __vec) (op VecGreaterThan) (signed true)
         (shape F64) (lhs (Ref 7)) (rhs (Ref 9))))
       (17 (VecExtractLaneOp (var __i32) (src (Ref 16)) (shape I32) (lane 0)))
       (18 (DupVar (var __input_compare_arg__) (src (Ref 17)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 18)) (fail (Block 1)) (condition (Ref 13))))
     (roots ((Ref 0) (Ref 1) (Ref 13) (Ref 18))))
    |}]

let%expect_test "fprem" =
  test_trans_block 0x004892f4;
  [%expect
    {|
    ((id 9)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int))) (1 (Const __i32 40))
       (2 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (CallOp (func __load_big_float__) (args ((Ref 2)))))
       (4 (ReturnedOp (var __fl) (typ Float)))
       (5 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 24)))
       (6 (LoadOp (var ebx) (op Load32) (addr (Ref 0)) (offset 48)))
       (7 (Const __i32 32767))
       (8 (BiOp (var ebx) (op And) (lhs (Ref 6)) (rhs (Ref 7)))) (9 Nop)
       (10 (BiOp (var ebx) (op Subtract) (lhs (Ref 8)) (rhs (Ref 5))))
       (11 (Const __i32 7))
       (12 (BiOp (var ebx) (op And) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var ebx) (op Or) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (BiOp (var ecx) (op Subtract) (lhs (Ref 8)) (rhs (Ref 14)))) (16 Nop)
       (17 (Const __i32 32768))
       (18 (BiOp (var ebx) (op And) (lhs (Ref 5)) (rhs (Ref 17))))
       (19 (BiOp (var ecx) (op Or) (lhs (Ref 15)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 19)) (offset 24)))
       (21 (Const __i32 16))
       (22 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 21))))
       (23 (CallOp (func __load_big_float__) (args ((Ref 22)))))
       (24 (ReturnedOp (var __fl) (typ Float)))
       (25 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset 24)))
       (26 (CallOp (func __float_mod__) (args ((Ref 4) (Ref 24)))))
       (27 (ReturnedOp (var __fl) (typ Float))) (28 (Const __i32 40))
       (29 (BiOp (var __i32) (op Add) (lhs (Ref 0)) (rhs (Ref 28))))
       (30 (CallOp (func __store_big_float__) (args ((Ref 29) (Ref 27)))))
       (31 Nop)
       (32 (SetGlobalOp (value (Ref 19)) (global ((name ecx) (typ Int)))))
       (33 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))
       (34 (SetGlobalOp (value (Ref 18)) (global ((name ebx) (typ Int)))))))
     (terminator (Goto (Block 8))) (roots ((Ref 0))))
    |}]

let%expect_test "sub ja" =
  test_trans_block 0x0048929e;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 24)))
       (2 (Const __i32 32767))
       (3 (BiOp (var eax) (op And) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (Const __i32 63))
       (5 (BiOp (var eax) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (LoadOp (var ebx) (op Load32) (addr (Ref 0)) (offset 48)))
       (7 (Const __i32 32767))
       (8 (BiOp (var ebx) (op And) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (BiOp (var ebx) (op Subtract) (lhs (Ref 8)) (rhs (Ref 5))))
       (10
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 8))
         (rhs (Ref 5))))
       (11 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))
       (12 (SetGlobalOp (value (Ref 9)) (global ((name ebx) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 10)) (fail (Block 8)) (condition (Ref 10))))
     (roots ((Ref 0) (Ref 10))))
    |}]

let%expect_test "sahf je" =
  test_trans_block 0x0048582f;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (Const __i32 4841890))
       (3 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 2))))
       (4 (BiOp (var __fl) (op FloatMult) (lhs (Ref 1)) (rhs (Ref 3))))
       (5 (UniOp (var __fl) (op FloatRound) (operand (Ref 4))))
       (6 (Landmine (var eax) (typ Int)))
       (7 (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 5)) (rhs (Ref 4))))
       (8
        (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 5)) (rhs (Ref 4))))
       (9 (BiOp (var __i32) (op Equal) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 1))))
       (11 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 4)) (offset 8)))
       (12 (Const __i32 8))
       (13 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 0)) (rhs (Ref 12))))
       (14 (SetGlobalOp (value (Ref 6)) (global ((name eax) (typ Int)))))
       (15
        (SetGlobalOp (value (Ref 13)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 2)) (condition (Ref 9))))
     (roots ((Ref 9))))
    |}]

let%expect_test "add jae" =
  test_trans_block 0x00488a1e;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (BiOp (var eax) (op Add) (lhs (Ref 1)) (rhs (Ref 1))))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 2)) (rhs (Ref 1))))
       (4 (SetGlobalOp (value (Ref 2)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 11)) (fail (Block 2)) (condition (Ref 3))))
     (roots ((Ref 0) (Ref 3))))
    |}]

let%expect_test "pmaxsw, pcmpeqd, pmovmskb" =
  test_trans_block 0x00484ef1;
  [%expect
    {|
    ((id 9)
     (instrs
      ((0 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (1 (Const __i32 0))
       (2 (BiOp (var edx) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
       (3 (Const __i32 32751))
       (4 (BiOp (var edx) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (GetGlobalOp (var xmm3) (global ((name xmm3) (typ Vec)))))
       (6 (Const __i32 52))
       (7
        (VecShiftLeftOp (var xmm3) (operand (Ref 5)) (count (Ref 6)) (shape I64)))
       (8 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec)))))
       (9 (BiOp (var xmm2) (op VecOr) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (Const ecx 1011))
       (11
        (VecReplaceLaneOp (var xmm3) (dest (Ref 7)) (lane_value (Ref 10))
         (shape I32) (lane 0)))
       (12 (GetGlobalOp (var xmm1) (global ((name xmm1) (typ Vec)))))
       (13 (Const __i32 20))
       (14
        (VecShiftRightOp (var xmm1) (operand (Ref 12)) (count (Ref 13))
         (shape I64) (signed false)))
       (15
        (VecLaneBiOp (var xmm1) (op VecSub) (shape I32) (lhs (Ref 14))
         (rhs (Ref 11))))
       (16 (VecConst (var xmm3) (lower_bits 0) (upper_bits 0)))
       (17
        (SignedVecLaneBiOp (var xmm1) (op VecMax) (signed true) (shape I16)
         (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (VecExtractLaneOp (var __i32) (src (Ref 17)) (shape I32) (lane 0)))
       (19
        (VecShiftLeftOp (var xmm2) (operand (Ref 9)) (count (Ref 18))
         (shape I64)))
       (20
        (VecLaneBiOp (var xmm2) (op VecEqual) (shape I32) (lhs (Ref 19))
         (rhs (Ref 16))))
       (21 (UniOp (var eax) (op VecInt8SignBitmask) (operand (Ref 20))))
       (22 Nop) (23 (Const __i32 32767))
       (24 (BiOp (var edx) (op And) (lhs (Ref 4)) (rhs (Ref 23))))
       (25 (Const __i32 32752))
       (26
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (SetGlobalOp (value (Ref 24)) (global ((name edx) (typ Int)))))
       (28 (SetGlobalOp (value (Ref 4)) (global ((name ecx) (typ Int)))))
       (29 (SetGlobalOp (value (Ref 20)) (global ((name xmm2) (typ Vec)))))
       (30 (SetGlobalOp (value (Ref 16)) (global ((name xmm3) (typ Vec)))))
       (31 (SetGlobalOp (value (Ref 21)) (global ((name eax) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 17)) (global ((name xmm1) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 10)) (condition (Ref 26))))
     (roots ((Ref 26))))
    |}]

let%expect_test "xorpd, pinsrw, movlpd, mulsd" =
  test_trans_block 0x00484fd8;
  [%expect
    {|
    ((id 18)
     (instrs
      ((0 (VecConst (var xmm0) (lower_bits 0) (upper_bits 0)))
       (1 (Const eax 17392))
       (2
        (VecReplaceLaneOp (var xmm0) (dest (Ref 0)) (lane_value (Ref 1))
         (shape I16) (lane 3)))
       (3 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec)))))
       (4 (Const __i32 4803568))
       (5
        (VecLoadLaneOp (var xmm7) (dest_vec (Ref 3)) (addr (Ref 4)) (shape I64)
         (lane 0)))
       (6 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec)))))
       (7 (Const __i32 4803584))
       (8
        (VecLoadLaneOp (var xmm2) (dest_vec (Ref 6)) (addr (Ref 7)) (shape I64)
         (lane 0)))
       (9 (GetGlobalOp (var xmm4) (global ((name xmm4) (typ Vec)))))
       (10
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 2))
         (rhs (Ref 9))))
       (11 (VecExtractLaneOp (var __i64) (src (Ref 2)) (shape I64) (lane 1)))
       (12
        (VecReplaceLaneOp (var xmm0) (dest (Ref 10)) (lane_value (Ref 11))
         (shape I64) (lane 1)))
       (13 (VecExtractLaneOp (var edx) (src (Ref 9)) (shape I32) (lane 0)))
       (14 (Const __i32 32))
       (15
        (VecShiftRightOp (var xmm4) (operand (Ref 9)) (count (Ref 14))
         (shape I64) (signed false)))
       (16 (VecExtractLaneOp (var eax) (src (Ref 15)) (shape I32) (lane 0)))
       (17 (Const __i32 0))
       (18 (BiOp (var __i32) (op Equal) (lhs (Ref 13)) (rhs (Ref 17))))
       (19 (SetGlobalOp (value (Ref 13)) (global ((name edx) (typ Int)))))
       (20 (SetGlobalOp (value (Ref 8)) (global ((name xmm2) (typ Vec)))))
       (21 (SetGlobalOp (value (Ref 16)) (global ((name eax) (typ Int)))))
       (22 (SetGlobalOp (value (Ref 12)) (global ((name xmm0) (typ Vec)))))
       (23 (SetGlobalOp (value (Ref 5)) (global ((name xmm7) (typ Vec)))))
       (24 (SetGlobalOp (value (Ref 15)) (global ((name xmm4) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 18))))
     (roots ((Ref 18))))
    |}]

let%expect_test "pextrw, mulsd" =
  test_trans_block 0x00485561;
  [%expect
    {|
    ((id 69)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var esi) (op Load32) (addr (Ref 0)))) (2 (Const __i32 4))
       (3 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 2))))
       (4 (GetGlobalOp (var xmm0) (global ((name xmm0) (typ Vec)))))
       (5 (GetGlobalOp (var xmm1) (global ((name xmm1) (typ Vec)))))
       (6
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 4))
         (rhs (Ref 5))))
       (7 Nop) (8 Nop)
       (9 (GetGlobalOp (var xmm7) (global ((name xmm7) (typ Vec)))))
       (10
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 6))
         (rhs (Ref 9))))
       (11 Nop) (12 Nop)
       (13 (GetGlobalOp (var xmm6) (global ((name xmm6) (typ Vec)))))
       (14
        (VecLaneBiOp (var __vec) (op VecMul) (shape F64) (lhs (Ref 13))
         (rhs (Ref 10))))
       (15 (VecExtractLaneOp (var __i64) (src (Ref 13)) (shape I64) (lane 1)))
       (16
        (VecReplaceLaneOp (var xmm6) (dest (Ref 14)) (lane_value (Ref 15))
         (shape I64) (lane 1)))
       (17
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 10))
         (rhs (Ref 14))))
       (18 (VecExtractLaneOp (var __i64) (src (Ref 4)) (shape I64) (lane 1)))
       (19
        (VecReplaceLaneOp (var xmm0) (dest (Ref 17)) (lane_value (Ref 18))
         (shape I64) (lane 1)))
       (20 (VecExtractLaneOp (var eax) (src (Ref 19)) (shape I16) (lane 3)))
       (21 (Const __i32 32752))
       (22 (BiOp (var eax) (op And) (lhs (Ref 20)) (rhs (Ref 21))))
       (23 (Const edx 24)) (24 (Const __i32 32752))
       (25 (BiOp (var __i32) (op Equal) (lhs (Ref 22)) (rhs (Ref 24))))
       (26 (SetGlobalOp (value (Ref 23)) (global ((name edx) (typ Int)))))
       (27 (SetGlobalOp (value (Ref 1)) (global ((name esi) (typ Int)))))
       (28 (SetGlobalOp (value (Ref 22)) (global ((name eax) (typ Int)))))
       (29 (SetGlobalOp (value (Ref 19)) (global ((name xmm0) (typ Vec)))))
       (30 (SetGlobalOp (value (Ref 16)) (global ((name xmm6) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 59)) (fail (Block 70)) (condition (Ref 25))))
     (roots ((Ref 3) (Ref 25))))
    |}]

let%expect_test "movsd" =
  test_trans_block 0x004852c9;
  [%expect
    {|
    ((id 52)
     (instrs
      ((0 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec)))))
       (1
        (VecLaneBiOp (var __vec) (op VecAdd) (shape F64) (lhs (Ref 0))
         (rhs (Ref 0))))
       (2 (VecExtractLaneOp (var __i64) (src (Ref 0)) (shape I64) (lane 1)))
       (3
        (VecReplaceLaneOp (var xmm2) (dest (Ref 1)) (lane_value (Ref 2))
         (shape I64) (lane 1)))
       (4 (GetGlobalOp (var xmm0) (global ((name xmm0) (typ Vec)))))
       (5 (VecExtractLaneOp (var __i64) (src (Ref 1)) (shape I64) (lane 0)))
       (6
        (VecReplaceLaneOp (var xmm0) (dest (Ref 4)) (lane_value (Ref 5))
         (shape I64) (lane 0)))
       (7 (Const edx 1006))
       (8 (SetGlobalOp (value (Ref 7)) (global ((name edx) (typ Int)))))
       (9 (SetGlobalOp (value (Ref 3)) (global ((name xmm2) (typ Vec)))))
       (10 (SetGlobalOp (value (Ref 6)) (global ((name xmm0) (typ Vec)))))))
     (terminator (Goto (Block 59))) (roots ()))
    |}]

let%expect_test "backward direction" =
  test_trans_block 0x0047d9d7;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (1 (Const __i32 2))
       (2 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (Const __i32 1))
       (4 (BiOp (var __i32) (op Subtract) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (6 (BiOp (var esi) (op Subtract) (lhs (Ref 5)) (rhs (Ref 4))))
       (7 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (8 (BiOp (var edi) (op Subtract) (lhs (Ref 7)) (rhs (Ref 4))))
       (9 (Memcopy (count (Ref 2)) (src (Ref 6)) (dest (Ref 8))))
       (10 (Const __i32 1))
       (11 (BiOp (var esi) (op Subtract) (lhs (Ref 6)) (rhs (Ref 10))))
       (12 (Const __i32 1))
       (13 (BiOp (var edi) (op Subtract) (lhs (Ref 8)) (rhs (Ref 12))))
       (14 (Const ecx 0))
       (15 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (16 (SetGlobalOp (value (Ref 14)) (global ((name ecx) (typ Int)))))
       (17 (SetGlobalOp (value (Ref 11)) (global ((name esi) (typ Int)))))
       (18 (SetGlobalOp (value (Ref 13)) (global ((name edi) (typ Int)))))))
     (terminator
      (Switch (cases ((Block 29) (Block 30) (Block 31) (Block 32)))
       (default (Block 33)) (switch_on (Ref 15))))
     (roots ((Ref 15))))
    |}]

let%expect_test "int3" =
  test_trans_block 0x00487ff1;
  [%expect
    {|
    ((id 10)
     (instrs
      ((0 (Const __i32 3)) (1 (OutsideContext (var esp) (typ Int)))
       (2 (Const __i32 4))
       (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
       (5 (Const __i32 4))
       (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
       (7 (Const __i32 4751340))
       (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
       (9 (CallOp (func __func47f9da__) (args ((Ref 6)))))
       (10 (ReturnedOp (var esp) (typ Int)))))
     (terminator Return) (roots ((Ref 10))))
    |}]

(* check this *)
let%expect_test "mmx stuff" =
  test_trans_block 0x00476bce;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (2 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (3 (VecSplatOp (var mm0) (value (Ref 2)) (shape I64)))
       (4 (VecConst (var mm7) (lower_bits 0) (upper_bits 0)))
       (5 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (6 (LoadOp (var __i32) (op Load32) (addr (Ref 5))))
       (7 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 6))))
       (8 (VecSplatOp (var mm2) (value (Ref 7)) (shape I64)))
       (9
        (VecExtend (var mm0) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 3))))
       (10 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (12 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 11))))
       (13 (VecSplatOp (var mm3) (value (Ref 12)) (shape I64)))
       (14
        (VecExtend (var mm2) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 8))))
       (15 (Const __i32 4835064))
       (16 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 15))))
       (17
        (SignedVecLaneBiOp (var mm2) (op VecSubSaturating) (signed true)
         (shape I16) (lhs (Ref 14)) (rhs (Ref 16))))
       (18
        (VecExtend (var mm3) (signed false) (shape I8) (half_used LowOrder)
         (operand (Ref 13))))
       (19 (Const __i32 4835064))
       (20 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 19))))
       (21
        (SignedVecLaneBiOp (var mm3) (op VecSubSaturating) (signed true)
         (shape I16) (lhs (Ref 18)) (rhs (Ref 20))))
       (22 Nop)
       (23
        (VecShuffleOp (var mm2) (arg1 (Ref 17)) (arg2 (Ref 21))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (24 Nop) (25 (Const __i32 4835072))
       (26 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 25))))
       (27 (BiOp (var mm2) (op VecMulAdd16Bit) (lhs (Ref 23)) (rhs (Ref 26))))
       (28 (Const __i32 8))
       (29
        (VecShiftLeftOp (var mm1) (operand (Ref 9)) (count (Ref 28)) (shape I16)))
       (30 Nop)
       (31
        (VecExtend (var mm1) (signed false) (shape I16) (half_used LowOrder)
         (operand (Ref 29))))
       (32
        (VecShuffleOp (var mm6) (arg1 (Ref 29)) (arg2 (Ref 4))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (33 Nop)
       (34
        (VecShuffleOp (var mm5) (arg1 (Ref 17)) (arg2 (Ref 21))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (35
        (VecLaneBiOp (var mm2) (op VecAdd) (shape I32) (lhs (Ref 27))
         (rhs (Ref 31))))
       (36 (Const __i32 4835072))
       (37 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 36))))
       (38 (BiOp (var mm5) (op VecMulAdd16Bit) (lhs (Ref 34)) (rhs (Ref 37))))
       (39 Nop)
       (40
        (VecShuffleOp (var mm3) (arg1 (Ref 21)) (arg2 (Ref 9))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (41 Nop) (42 (Const __i32 4835080))
       (43 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 42))))
       (44 (BiOp (var mm3) (op VecMulAdd16Bit) (lhs (Ref 40)) (rhs (Ref 43))))
       (45 (Const __i32 8))
       (46
        (VecShiftRightOp (var mm2) (operand (Ref 35)) (count (Ref 45))
         (shape I64) (signed true)))
       (47
        (VecLaneBiOp (var mm5) (op VecAdd) (shape I32) (lhs (Ref 38))
         (rhs (Ref 32))))
       (48
        (VecShuffleOp (var mm4) (arg1 (Ref 17)) (arg2 (Ref 9))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (49 (Const __i32 4835088))
       (50 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 49))))
       (51 (BiOp (var mm4) (op VecMulAdd16Bit) (lhs (Ref 48)) (rhs (Ref 50))))
       (52 (Const __i32 8))
       (53
        (VecShiftRightOp (var mm5) (operand (Ref 47)) (count (Ref 52))
         (shape I64) (signed true)))
       (54 (Const __i32 8))
       (55
        (VecShiftRightOp (var mm3) (operand (Ref 44)) (count (Ref 54))
         (shape I64) (signed true)))
       (56
        (VecShuffleOp (var mm7) (arg1 (Ref 17)) (arg2 (Ref 9))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (57 (Const __i32 8))
       (58
        (VecShiftRightOp (var mm4) (operand (Ref 51)) (count (Ref 57))
         (shape I64) (signed true)))
       (59 Nop) (60 (Const __i32 4835088))
       (61 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 60))))
       (62 (BiOp (var mm7) (op VecMulAdd16Bit) (lhs (Ref 56)) (rhs (Ref 61))))
       (63
        (VecShuffleOp (var mm1) (arg1 (Ref 21)) (arg2 (Ref 9))
         (control_lower_bits 1663524835064808708) (control_upper_bits 0)))
       (64 (Const __i32 4835080))
       (65 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 64))))
       (66 (BiOp (var mm1) (op VecMulAdd16Bit) (lhs (Ref 63)) (rhs (Ref 65))))
       (67
        (VecShuffleOp (var mm3) (arg1 (Ref 55)) (arg2 (Ref 46))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (68
        (VecShuffleOp (var mm6) (arg1 (Ref 55)) (arg2 (Ref 46))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (69 Nop) (70 (Const __i32 8))
       (71
        (VecShiftRightOp (var mm7) (operand (Ref 62)) (count (Ref 70))
         (shape I64) (signed true)))
       (72 (Const __i32 4847464))
       (73 (LoadOp (var __i32) (op Load32) (addr (Ref 72))))
       (74 (VecSplatOp (var __vec) (value (Ref 73)) (shape I32)))
       (75
        (VecShuffleOp (var mm4) (arg1 (Ref 58)) (arg2 (Ref 74))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (76 (Const __i32 4847464))
       (77 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 76))))
       (78
        (VecShuffleOp (var mm0) (arg1 (Ref 58)) (arg2 (Ref 77))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (79 (Const __i32 8))
       (80
        (VecShiftRightOp (var mm1) (operand (Ref 66)) (count (Ref 79))
         (shape I64) (signed true)))
       (81
        (SignedBiOp (var mm3) (op VecNarrow32Bit) (signed true) (lhs (Ref 67))
         (rhs (Ref 75))))
       (82 Nop)
       (83
        (SignedBiOp (var mm6) (op VecNarrow32Bit) (signed true) (lhs (Ref 68))
         (rhs (Ref 78))))
       (84
        (SignedBiOp (var mm3) (op VecNarrow16Bit) (signed false) (lhs (Ref 81))
         (rhs (Ref 83))))
       (85
        (VecShuffleOp (var mm2) (arg1 (Ref 80)) (arg2 (Ref 53))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (86 Nop)
       (87
        (VecShuffleOp (var mm1) (arg1 (Ref 80)) (arg2 (Ref 53))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (88 (Const __i32 4847464))
       (89 (LoadOp (var __i32) (op Load32) (addr (Ref 88))))
       (90 (VecSplatOp (var __vec) (value (Ref 89)) (shape I32)))
       (91
        (VecShuffleOp (var mm7) (arg1 (Ref 71)) (arg2 (Ref 90))
         (control_lower_bits 1374179596769034496) (control_upper_bits 0)))
       (92 (Const __i32 4847464))
       (93 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 92))))
       (94
        (VecShuffleOp (var mm4) (arg1 (Ref 71)) (arg2 (Ref 93))
         (control_lower_bits 1663540288121341188) (control_upper_bits 0)))
       (95 Nop)
       (96
        (SignedBiOp (var mm2) (op VecNarrow32Bit) (signed true) (lhs (Ref 85))
         (rhs (Ref 91))))
       (97 (Const __i32 4835096))
       (98 (LoadOp (var __vec) (op VecLoad64ZeroExtend) (addr (Ref 97))))
       (99 (BiOp (var mm3) (op VecAnd) (lhs (Ref 84)) (rhs (Ref 98))))
       (100
        (SignedBiOp (var mm1) (op VecNarrow32Bit) (signed true) (lhs (Ref 87))
         (rhs (Ref 94))))
       (101 (Const __i32 8))
       (102
        (VecShiftRightOp (var mm3) (operand (Ref 99)) (count (Ref 101))
         (shape I64) (signed false)))
       (103 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (104 (Const __i32 12))
       (105 (BiOp (var edx) (op Add) (lhs (Ref 103)) (rhs (Ref 104))))
       (106 (BiOp (var mm0) (op VecOr) (lhs (Ref 84)) (rhs (Ref 102))))
       (107
        (SignedBiOp (var mm2) (op VecNarrow16Bit) (signed false) (lhs (Ref 96))
         (rhs (Ref 100))))
       (108 (Const __i32 32))
       (109
        (VecShiftRightOp (var mm3) (operand (Ref 102)) (count (Ref 108))
         (shape I64) (signed false)))
       (110 (Const __i32 4))
       (111 (BiOp (var eax) (op Add) (lhs (Ref 0)) (rhs (Ref 110))))
       (112 (VecExtractLaneOp (var __i32) (src (Ref 106)) (shape I32) (lane 0)))
       (113
        (StoreOp (op Store32) (addr (Ref 105)) (value (Ref 112)) (offset -12)))
       (114
        (VecShuffleOp (var mm3) (arg1 (Ref 109)) (arg2 (Ref 107))
         (control_lower_bits 1374164143712502016) (control_upper_bits 0)))
       (115 (Const __i32 24))
       (116
        (VecShiftRightOp (var mm2) (operand (Ref 107)) (count (Ref 115))
         (shape I64) (signed false)))
       (117 (Const __i32 4))
       (118 (BiOp (var ecx) (op Add) (lhs (Ref 10)) (rhs (Ref 117))))
       (119 (VecExtractLaneOp (var __i32) (src (Ref 114)) (shape I32) (lane 0)))
       (120
        (StoreOp (op Store32) (addr (Ref 105)) (value (Ref 119)) (offset -8)))
       (121 (Const __i32 48))
       (122
        (VecShiftRightOp (var mm3) (operand (Ref 114)) (count (Ref 121))
         (shape I64) (signed false)))
       (123 (BiOp (var mm2) (op VecOr) (lhs (Ref 116)) (rhs (Ref 122))))
       (124 (Const __i32 4))
       (125 (BiOp (var ebx) (op Add) (lhs (Ref 5)) (rhs (Ref 124))))
       (126 (VecExtractLaneOp (var __i32) (src (Ref 123)) (shape I32) (lane 0)))
       (127
        (StoreOp (op Store32) (addr (Ref 105)) (value (Ref 126)) (offset -4)))
       (128 (Const __i32 1))
       (129 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (130 (BiOp (var edi) (op Subtract) (lhs (Ref 129)) (rhs (Ref 128))))
       (131 (SetGlobalOp (value (Ref 105)) (global ((name edx) (typ Int)))))
       (132 (SetGlobalOp (value (Ref 118)) (global ((name ecx) (typ Int)))))
       (133 (SetGlobalOp (value (Ref 123)) (global ((name mm2) (typ Vec)))))
       (134 (SetGlobalOp (value (Ref 100)) (global ((name mm1) (typ Vec)))))
       (135 (SetGlobalOp (value (Ref 111)) (global ((name eax) (typ Int)))))
       (136 (SetGlobalOp (value (Ref 83)) (global ((name mm6) (typ Vec)))))
       (137 (SetGlobalOp (value (Ref 91)) (global ((name mm7) (typ Vec)))))
       (138 (SetGlobalOp (value (Ref 130)) (global ((name edi) (typ Int)))))
       (139 (SetGlobalOp (value (Ref 125)) (global ((name ebx) (typ Int)))))
       (140 (SetGlobalOp (value (Ref 94)) (global ((name mm4) (typ Vec)))))
       (141 (SetGlobalOp (value (Ref 122)) (global ((name mm3) (typ Vec)))))
       (142 (SetGlobalOp (value (Ref 53)) (global ((name mm5) (typ Vec)))))
       (143 (SetGlobalOp (value (Ref 106)) (global ((name mm0) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 1)) (fail (Block 2)) (condition (Ref 130))))
     (roots ((Ref 130))))
    |}]

let%expect_test "xor je" =
  test_trans_block 0x0046fcb3;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (1 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (2 (BiOp (var eax) (op Xor) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (UniOp (var __i32) (op EqualsZero) (operand (Ref 2))))
       (4 (SetGlobalOp (value (Ref 2)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 11)) (fail (Block 7)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test "imul byte" =
  test_trans_block 0x0046e208;
  [%expect
    {|
    ((id 32)
     (instrs
      ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (1
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed true)
         (offset 24)))
       (2 Nop) (3 Nop) (4 Nop)
       (5
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 0)) (signed true)
         (offset 29)))
       (6 Nop) (7 Nop)
       (8 (BiOp (var __i32) (op Multiply) (lhs (Ref 1)) (rhs (Ref 5)))) (9 Nop)
       (10 Nop)
       (11 (StoreOp (op Store8) (addr (Ref 0)) (value (Ref 8)) (offset 30)))
       (12 (UniOp (var eax) (op ZeroExtendLow8) (operand (Ref 8))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (14 (BiOp (var eax) (op Multiply) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (Const __i32 7))
       (16 (BiOp (var eax) (op Add) (lhs (Ref 14)) (rhs (Ref 15)))) (17 Nop)
       (18 (Const __i32 3))
       (19
        (SignedBiOp (var eax) (op ShiftRight) (signed false) (lhs (Ref 16))
         (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 19)) (offset 12)))
       (21 (OutsideContext (var esp) (typ Int)))
       (22 (LoadOp (var ebx) (op Load32) (addr (Ref 21)))) (23 (Const __i32 4))
       (24 (BiOp (var esp) (op Add) (lhs (Ref 21)) (rhs (Ref 23))))
       (25 (LoadOp (var __i32) (op Load32) (addr (Ref 24))))
       (26 (OutsideContext (var __ret_addr__) (typ Int)))
       (27 (BiOp (var __i32) (op Equal) (lhs (Ref 25)) (rhs (Ref 26))))
       (28 (AssertOp (condition (Ref 27)))) (29 (Const __i32 4))
       (30 (BiOp (var esp) (op Add) (lhs (Ref 24)) (rhs (Ref 29))))
       (31 (SetGlobalOp (value (Ref 19)) (global ((name eax) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 22)) (global ((name ebx) (typ Int)))))))
     (terminator Return) (roots ((Ref 26) (Ref 30))))
    |}]

let%expect_test "tail indirect call" =
  test_trans_block 0x0047efac;
  [%expect
    {|
    ((id 8)
     (instrs
      ((0 (Const __i32 0)) (1 (OutsideContext (var esp) (typ Int)))
       (2 (Const __i32 4))
       (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
       (5 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (6 (LoadOp (var eax) (op Load32) (addr (Ref 5)) (offset 20)))
       (7 (Const __i32 4))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
       (9 (Const __i32 4714417))
       (10 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 9))))
       (11 (SetGlobalOp (value (Ref 6)) (global ((name eax) (typ Int)))))
       (12 (CallOp (func __func47f816__) (args ((Ref 8)))))
       (13 (ReturnedOp (var esp) (typ Int)))
       (14 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int))))) (15 Nop)
       (16 (LoadOp (var ebx) (op Load32) (addr (Ref 14)) (offset 4)))
       (17 (LoadOp (var edi) (op Load32) (addr (Ref 14)) (offset 8)))
       (18 (LoadOp (var esi) (op Load32) (addr (Ref 14)) (offset 12)))
       (19 (LoadOp (var eax) (op Load32) (addr (Ref 13)) (offset 8)))
       (20 (Const __i32 1)) (21 (Const __i32 0))
       (22
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 19))
         (rhs (Ref 20))))
       (23 (BiOp (var __i32) (op Add) (lhs (Ref 22)) (rhs (Ref 21))))
       (24 (BiOp (var eax) (op Add) (lhs (Ref 23)) (rhs (Ref 19))))
       (25 (LoadOp (var esp) (op Load32) (addr (Ref 14)) (offset 16)))
       (26 (Const __i32 4))
       (27 (BiOp (var esp) (op Add) (lhs (Ref 25)) (rhs (Ref 26))))
       (28 (LoadOp (var __i32) (op Load32) (addr (Ref 14)) (offset 20)))
       (29 (SetGlobalOp (value (Ref 14)) (global ((name edx) (typ Int)))))
       (30 (SetGlobalOp (value (Ref 18)) (global ((name esi) (typ Int)))))
       (31 (SetGlobalOp (value (Ref 24)) (global ((name eax) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 17)) (global ((name edi) (typ Int)))))
       (33 (SetGlobalOp (value (Ref 16)) (global ((name ebx) (typ Int)))))
       (34 (CallIndirectOp (table_index (Ref 28)) (args ((Ref 27)))))
       (35 (ReturnedOp (var esp) (typ Int)))))
     (terminator Return) (roots ((Ref 35))))
    |}]

let%expect_test "nonzero switch" =
  test_trans_block 0x0046af8f;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (1 (Const __i32 20))
       (2 (BiOp (var __i32) (op Subtract) (lhs (Ref 0)) (rhs (Ref 1))))))
     (terminator
      (Switch
       (cases
        ((Block 6) (Block 8) (Block 10) (Block 12) (Block 14) (Block 16)
         (Block 18) (Block 20) (Block 22) (Block 24) (Block 26)))
       (default (Block 84)) (switch_on (Ref 2))))
     (roots ((Ref 2))))
    |}]

let%expect_test "fistp dword" =
  test_trans_block 0x00465929;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -32)))
       (3 (Const __i32 0))
       (4 (BiOp (var __i32) (op And) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 4)) (offset -32)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset -36)))
       (7 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 0)) (offset -36)))
       (8 (UniOp (var __fl) (op Int64ToFloatSigned) (operand (Ref 7))))
       (9 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (10 (OutsideContext (var esp) (typ Int))) (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 9))))
       (14 (Const __i32 4))
       (15 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 9))))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (18 (BiOp (var __fl) (op FloatMult) (lhs (Ref 8)) (rhs (Ref 17))))
       (19 (Const __i32 4819536))
       (20 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 19))))
       (21 (BiOp (var __fl) (op FloatSub) (lhs (Ref 18)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 21)) (offset -8)))
       (23 (StoreOp (op FloatStore64) (addr (Ref 15)) (value (Ref 21))))
       (24 (Const __i32 4))
       (25 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 24))))
       (26 (Const __i32 4610332))
       (27 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 26)))) (28 Nop)
       (29 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))
       (30 (CallOp (func __func47ee10__) (args ((Ref 25)))))
       (31 (ReturnedOp (var esp) (typ Int))) (32 Nop) (33 (Const __i32 4))
       (34 (BiOp (var esp) (op Add) (lhs (Ref 31)) (rhs (Ref 33))))
       (35
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (36 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 35))))
       (37 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (38 (StoreOp (op Store32) (addr (Ref 37)) (value (Ref 36)) (offset -12)))
       (39 Nop) (40 (Const __i32 4))
       (41 (BiOp (var esp) (op Add) (lhs (Ref 34)) (rhs (Ref 40))))
       (42 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 37)) (offset -12)))
       (43 (UniOp (var __i32) (op FloatToInt32) (operand (Ref 42))))
       (44 (StoreOp (op Store32) (addr (Ref 37)) (value (Ref 43)) (offset -20)))
       (45 (LoadOp (var eax) (op Load32) (addr (Ref 37)) (offset -20)))
       (46 (Const __i32 1))
       (47 (BiOp (var ecx) (op Add) (lhs (Ref 45)) (rhs (Ref 46))))
       (48 (Const __i32 0))
       (49
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
         (lhs (Ref 45)) (rhs (Ref 48))))
       (50 (Const __i32 -8))
       (51 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 35)) (rhs (Ref 50))))
       (52 (SetGlobalOp (value (Ref 47)) (global ((name ecx) (typ Int)))))
       (53 (SetGlobalOp (value (Ref 45)) (global ((name eax) (typ Int)))))
       (54
        (SetGlobalOp (value (Ref 51)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 4)) (condition (Ref 49))))
     (roots ((Ref 41) (Ref 49))))
    |}]

let%expect_test "fsubrp" =
  test_trans_block 0x0046336d;
  [%expect
    {|
    ((id 37)
     (instrs
      ((0 (FloatConst __fl 1))
       (1
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (2 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 1))))
       (3 (BiOp (var __fl) (op FloatSub) (lhs (Ref 0)) (rhs (Ref 2))))
       (4 (StoreOp (op FloatStore64) (addr (Ref 1)) (value (Ref 3))))))
     (terminator (Goto (Block 38))) (roots ()))
    |}]

let%expect_test "psrlq, andpd, psubd" =
  test_trans_block 0x0047ee50;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (1 (OutsideContext (var esp) (typ Int)))
       (2 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 1)) (offset 4)))
       (3
        (VecReplaceLaneOp (var xmm0) (dest (Ref 0)) (lane_value (Ref 2))
         (shape I64) (lane 0)))
       (4 (Const __i32 4785856))
       (5 (LoadOp (var xmm2) (op VecLoad128) (addr (Ref 4)))) (6 Nop) (7 Nop)
       (8 (Const __i32 52))
       (9
        (VecShiftRightOp (var xmm0) (operand (Ref 3)) (count (Ref 8)) (shape I64)
         (signed false)))
       (10 (VecExtractLaneOp (var eax) (src (Ref 9)) (shape I32) (lane 0)))
       (11 (Const __i32 4785904))
       (12 (LoadOp (var __vec) (op VecLoad128) (addr (Ref 11))))
       (13 (BiOp (var xmm0) (op VecAnd) (lhs (Ref 9)) (rhs (Ref 12))))
       (14
        (VecLaneBiOp (var xmm2) (op VecSub) (shape I32) (lhs (Ref 5))
         (rhs (Ref 13))))
       (15 (VecExtractLaneOp (var __i32) (src (Ref 14)) (shape I32) (lane 0)))
       (16
        (VecShiftRightOp (var xmm1) (operand (Ref 3)) (count (Ref 15))
         (shape I64) (signed false)))
       (17 (Const __i32 2048))
       (18 (BiOp (var __i32) (op And) (lhs (Ref 10)) (rhs (Ref 17))))
       (19 (SetGlobalOp (value (Ref 14)) (global ((name xmm2) (typ Vec)))))
       (20 (SetGlobalOp (value (Ref 10)) (global ((name eax) (typ Int)))))
       (21 (SetGlobalOp (value (Ref 13)) (global ((name xmm0) (typ Vec)))))
       (22 (SetGlobalOp (value (Ref 3)) (global ((name xmm7) (typ Vec)))))
       (23 (SetGlobalOp (value (Ref 16)) (global ((name xmm1) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 12)) (fail (Block 6)) (condition (Ref 18))))
     (roots ((Ref 1) (Ref 18))))
    |}]

let%expect_test "movq, psllq, cmpltpd" =
  test_trans_block 0x0047eed2;
  [%expect
    {|
    ((id 12)
     (instrs
      ((0 (VecConst (var __vec) (lower_bits 0) (upper_bits 0)))
       (1 (OutsideContext (var esp) (typ Int)))
       (2 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 1)) (offset 4)))
       (3
        (VecReplaceLaneOp (var xmm0) (dest (Ref 0)) (lane_value (Ref 2))
         (shape I64) (lane 0)))
       (4 (GetGlobalOp (var xmm1) (global ((name xmm1) (typ Vec)))))
       (5 (GetGlobalOp (var xmm2) (global ((name xmm2) (typ Vec)))))
       (6 (VecExtractLaneOp (var __i32) (src (Ref 5)) (shape I32) (lane 0)))
       (7
        (VecShiftLeftOp (var xmm1) (operand (Ref 4)) (count (Ref 6)) (shape I64)))
       (8 Nop)
       (9
        (SignedVecLaneBiOp (var xmm0) (op VecLessThan) (signed true) (shape F64)
         (lhs (Ref 3)) (rhs (Ref 7))))
       (10 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (11 (Const __i32 3071))
       (12
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 10))
         (rhs (Ref 11))))
       (13 (SetGlobalOp (value (Ref 3)) (global ((name xmm3) (typ Vec)))))
       (14 (SetGlobalOp (value (Ref 9)) (global ((name xmm0) (typ Vec)))))
       (15 (SetGlobalOp (value (Ref 7)) (global ((name xmm1) (typ Vec)))))))
     (terminator
      (Branch (succeed (Block 16)) (fail (Block 13)) (condition (Ref 12))))
     (roots ((Ref 1) (Ref 12))))
    |}]

let%expect_test "branch return" =
  test_trans_block 0x0047ee10;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (Const __i32 20336900))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2)))) (4 (Const __i32 0))
       (5 (BiOp (var __i32) (op Equal) (lhs (Ref 3)) (rhs (Ref 4))))))
     (terminator
      (Branch (succeed (Block 17)) (fail (Block 1)) (condition (Ref 5))))
     (roots ((Ref 0) (Ref 1) (Ref 5))))
    |}]

let%expect_test "and jbe" =
  test_trans_block 0x0046632e;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
       (3 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (4 (Const __i32 4))
       (5 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 4))))
       (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 3))))
       (7 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int))))) (8 Nop)
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 7)) (offset 4)))
       (10 (Const __i32 0))
       (11 (BiOp (var __i32) (op And) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 11)) (offset 4)))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 7)))) (14 (Const __i32 0))
       (15 (BiOp (var __i32) (op And) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 15)))) (17 Nop)
       (18 (Const __i32 65535))
       (19 (BiOp (var ecx) (op And) (lhs (Ref 2)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 2)) (offset 8)))
       (21 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))
       (22 (SetGlobalOp (value (Ref 19)) (global ((name ecx) (typ Int)))))
       (23 (SetGlobalOp (value (Ref 7)) (global ((name esi) (typ Int)))))
       (24 (SetGlobalOp (value (Ref 2)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 23)) (fail (Block 1)) (condition (Ref 21))))
     (roots ((Ref 1) (Ref 5) (Ref 21))))
    |}]

(*let%expect_est "fprem1 / sahf jp" =*)
(*  test_trans_block 0x0048b987;*)
(*  [%expect {||}]*)
(**)
(*let%expect_test "fptan" =*)
(*  test_trans_block 0x0048b952;*)
(*  [%expect {||}]*)
(**)
(*let%expect_test "fnstcw" =*)
(*  test_trans_block 0x0048b93f;*)
(*  [%expect {||}]*)

let%expect_test "frndint" =
  test_trans_block 0x0044f059;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const __i32 4956072))
       (1 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0))))
       (2 (UniOp (var __fl) (op FloatRound) (operand (Ref 1))))
       (3 (Const __i32 4819536))
       (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3))))
       (5 (BiOp (var __fl) (op FloatSub) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (Const __i32 4956100))
       (7 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 6))))
       (8 (UniOp (var __fl) (op FloatRound) (operand (Ref 7))))
       (9 (Const __i32 4819536))
       (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9))))
       (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (Const __i32 4956076))
       (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12))))
       (14 (UniOp (var __fl) (op FloatRound) (operand (Ref 13))))
       (15 (Const __i32 4819536))
       (16 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 15))))
       (17 (BiOp (var __fl) (op FloatSub) (lhs (Ref 14)) (rhs (Ref 16))))
       (18 (Const __i32 4956132))
       (19 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 18))))
       (20 (UniOp (var __fl) (op FloatRound) (operand (Ref 19))))
       (21 (Const __i32 4819536))
       (22 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 21))))
       (23 (BiOp (var __fl) (op FloatSub) (lhs (Ref 20)) (rhs (Ref 22))))
       (24 (Const __i32 4956132))
       (25 (StoreOp (op Store32) (addr (Ref 24)) (value (Ref 23))))
       (26 (Const __i32 4956160))
       (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 23))))
       (28 (Const __i32 4956076))
       (29 (StoreOp (op Store32) (addr (Ref 28)) (value (Ref 17))))
       (30 (Const __i32 4956104))
       (31 (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 17))))
       (32 (Const __i32 4956100))
       (33 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 11))))
       (34 (Const __i32 4956156))
       (35 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 11))))
       (36 (Const __i32 4956072))
       (37 (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 5))))
       (38 (Const __i32 4956128))
       (39 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 5)))) (40 Nop)))
     (terminator (Goto (Block 2))) (roots ()))
    |}]

let%expect_test "test eax,eax jae" =
  test_trans_block 0x0045dbcd;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -52)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -44)))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 108)))
       (4 (BiOp (var eax) (op Subtract) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 0))
       (6 (SetGlobalOp (value (Ref 1)) (global ((name edx) (typ Int)))))
       (7 (SetGlobalOp (value (Ref 4)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 8)) (fail (Block 6)) (condition (Ref 5))))
     (roots ((Ref 5))))
    |}]

let%expect_test "test eax,eax jbe" =
  test_trans_block 0x0047e73c;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 16)))
       (2 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 1)) (offset 28)))
       (3 (UniOp (var __i32) (op EqualsZero) (operand (Ref 1))))
       (4 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 16)) (fail (Block 3)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test "shld" =
  test_trans_block 0x00481fa8;
  [%expect
    {|
    ((id 211)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -92)))
       (2 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -88)))
       (3 (Const __i32 3))
       (4 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 29))
       (6
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 1))
         (rhs (Ref 5))))
       (7 (BiOp (var ecx) (op Or) (lhs (Ref 4)) (rhs (Ref 6))))
       (8 (Const __i32 3))
       (9 (BiOp (var eax) (op ShiftLeft) (lhs (Ref 1)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 9)) (offset -92)))
       (11 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -88)))
       (12 (SetGlobalOp (value (Ref 7)) (global ((name ecx) (typ Int)))))
       (13 (SetGlobalOp (value (Ref 9)) (global ((name eax) (typ Int)))))))
     (terminator (Goto (Block 217))) (roots ()))
    |}]

let%expect_test "repne scasb" =
  test_trans_block 0x0047dcc4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int))))) (1 Nop)
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (LoadOp (var edi) (op Load32) (addr (Ref 2)) (offset 8))) (4 Nop)
       (5 (Const eax 0)) (6 (Const __i32 0))
       (7 (CallOp (func __find_byte__) (args ((Ref 6) (Ref 3) (Ref 0)))))
       (8 (ReturnedOp (var ecx) (typ Int))) (9 (Const __i32 0))
       (10 (BiOp (var ecx) (op Subtract) (lhs (Ref 9)) (rhs (Ref 8))))
       (11 (BiOp (var ecx) (op Add) (lhs (Ref 10)) (rhs (Ref 0)))) (12 Nop)
       (13 (LoadOp (var esi) (op Load32) (addr (Ref 2)) (offset 12)))
       (14 (CallOp (func __byte_diff__) (args ((Ref 13) (Ref 3) (Ref 11)))))
       (15 (ReturnedOp (var esi) (typ Int)))
       (16 (ReturnedOp (var edi) (typ Int)))
       (17 (ReturnedOp (var ecx) (typ Int))) (18 Nop) (19 Nop)
       (20
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 15)) (signed true)
         (offset -1)))
       (21 Nop)
       (22 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 20)) (rhs (Ref 5))))
       (23 (Const ecx 0))
       (24
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 16)) (signed false)
         (offset -1)))
       (25 (UniOp (var __i32) (op ZeroExtendLow8) (operand (Ref 20)))) (26 Nop)
       (27
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 25))
         (rhs (Ref 24))))
       (28 (SetGlobalOp (value (Ref 23)) (global ((name ecx) (typ Int)))))
       (29 (SetGlobalOp (value (Ref 15)) (global ((name esi) (typ Int)))))
       (30 (SetGlobalOp (value (Ref 22)) (global ((name eax) (typ Int)))))
       (31 (SetGlobalOp (value (Ref 16)) (global ((name edi) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 0)) (global ((name ebx) (typ Int)))))
       (33 Nop) (34 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 24))))
       (35 (BiOp (var __i32) (op Equal) (lhs (Ref 20)) (rhs (Ref 34))))
       (36 (DupVar (var __input_compare_arg__) (src (Ref 35)) (typ Int)))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 2)) (condition (Ref 27))))
     (roots ((Ref 27) (Ref 36))))
    |}]

let%expect_test "jecxz" =
  test_trans_block 0x0047dcb9;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
       (7 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (8 (Const __i32 4))
       (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
       (11 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (12 (Const __i32 4))
       (13 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 11))))
       (15 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15))))
       (19 (LoadOp (var ecx) (op Load32) (addr (Ref 4)) (offset 16)))
       (20 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))
       (21 (SetGlobalOp (value (Ref 19)) (global ((name ecx) (typ Int)))))
       (22 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 1)) (condition (Ref 20))))
     (roots ((Ref 1) (Ref 17) (Ref 20))))
    |}]

let%expect_test "double fadd" =
  test_trans_block 0x00444017;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (Const __i32 6447736))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0))))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1))))
       (3 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2)) (offset -156)))
       (5 (Const __i32 5724880))
       (6 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 5))))
       (7 (Const __i32 5724884))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (Const __i32 4819536))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
       (12 (BiOp (var __fl) (op FloatSub) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 12)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 13)) (offset -20)))
       (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3)) (offset -20)))
       (16 (Const __i32 4819532))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 16))))
       (18 (Landmine (var eax) (typ Int))) (19 Nop)
       (20 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 15)) (rhs (Ref 17))))
       (21 (UniOp (var __i32) (op EqualsZero) (operand (Ref 20)))) (22 Nop)
       (23 (SetGlobalOp (value (Ref 2)) (global ((name edx) (typ Int)))))
       (24 (SetGlobalOp (value (Ref 1)) (global ((name ecx) (typ Int)))))
       (25 (SetGlobalOp (value (Ref 18)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 21))))
     (roots ((Ref 21))))
    |}]

let%expect_test "rcr" =
  test_trans_block 0x00482a92;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (1 (Const __i32 1))
       (2
        (SignedBiOp (var ecx) (op ShiftRight) (signed false) (lhs (Ref 0))
         (rhs (Ref 1))))
       (3 Nop) (4 Nop) (5 (Const __i32 31))
       (6 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 5))))
       (7 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (8 (Const __i32 1))
       (9
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 7))
         (rhs (Ref 8))))
       (10 (BiOp (var ebx) (op Or) (lhs (Ref 6)) (rhs (Ref 9))))
       (11 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (12 (Const __i32 1))
       (13
        (SignedBiOp (var edx) (op ShiftRight) (signed false) (lhs (Ref 11))
         (rhs (Ref 12))))
       (14 Nop) (15 Nop) (16 (Const __i32 31))
       (17 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 11)) (rhs (Ref 16))))
       (18 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (19 (Const __i32 1))
       (20
        (SignedBiOp (var __i32) (op ShiftRight) (signed false) (lhs (Ref 18))
         (rhs (Ref 19))))
       (21 (BiOp (var eax) (op Or) (lhs (Ref 17)) (rhs (Ref 20))))
       (22 (BiOp (var ecx) (op Or) (lhs (Ref 2)) (rhs (Ref 2))))
       (23 (SetGlobalOp (value (Ref 13)) (global ((name edx) (typ Int)))))
       (24 (SetGlobalOp (value (Ref 22)) (global ((name ecx) (typ Int)))))
       (25 (SetGlobalOp (value (Ref 21)) (global ((name eax) (typ Int)))))
       (26 (SetGlobalOp (value (Ref 10)) (global ((name ebx) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 4)) (condition (Ref 22))))
     (roots ((Ref 22))))
    |}]

let%expect_test "dumb div" =
  test_trans_block 0x00482a7c;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 20)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 16))) (3 Nop)
       (4
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 2))
         (rhs (Ref 1))))
       (5
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 2))
         (rhs (Ref 1))))
       (6 Nop) (7 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
       (8 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 7))))
       (9 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 5))))
       (10 (LongConst __i64 32))
       (11 (BiOp (var __i64) (op LongShiftLeft) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (BiOp (var __i64) (op LongOr) (lhs (Ref 8)) (rhs (Ref 11))))
       (13 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (14
        (SignedBiOp (var __i64) (op LongDivide) (signed false) (lhs (Ref 12))
         (rhs (Ref 13))))
       (15 (UniOp (var eax) (op LongToInt32) (operand (Ref 14)))) (16 Nop)
       (17 Nop) (18 Nop)
       (19 (SetGlobalOp (value (Ref 4)) (global ((name edx) (typ Int)))))
       (20 (SetGlobalOp (value (Ref 1)) (global ((name ecx) (typ Int)))))
       (21 (SetGlobalOp (value (Ref 15)) (global ((name eax) (typ Int)))))
       (22 (SetGlobalOp (value (Ref 4)) (global ((name ebx) (typ Int)))))))
     (terminator (Goto (Block 10))) (roots ((Ref 0))))
    |}]

let%expect_test "mul" =
  test_trans_block 0x004816d4;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 4)))
       (2 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 1))))
       (3 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (4 (UniOp (var __i64) (op Int32ToLongUnsigned) (operand (Ref 3))))
       (5 (BiOp (var __i64) (op LongMultiply) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (UniOp (var eax) (op LongToInt32) (operand (Ref 5))))
       (7 (LongConst __i64 32))
       (8
        (SignedBiOp (var __i64) (op LongShiftRight) (signed false) (lhs (Ref 5))
         (rhs (Ref 7))))
       (9 (UniOp (var edx) (op LongToInt32) (operand (Ref 8))))
       (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0))))
       (11 (OutsideContext (var __ret_addr__) (typ Int)))
       (12 (BiOp (var __i32) (op Equal) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (AssertOp (condition (Ref 12)))) (14 (Const __i32 20))
       (15 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 14))))
       (16 (SetGlobalOp (value (Ref 9)) (global ((name edx) (typ Int)))))
       (17 (SetGlobalOp (value (Ref 6)) (global ((name eax) (typ Int)))))))
     (terminator Return) (roots ((Ref 11) (Ref 15))))
    |}]

let%expect_test "stosw" =
  test_trans_block 0x00439633;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
       (7 (Const __i32 80))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 9))))
       (13 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (14 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 13)) (offset -76)))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 4)) (offset -24)))
       (16 (Const __i32 0))
       (17 (BiOp (var __i32) (op And) (lhs (Ref 15)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 17)) (offset -24)))
       (19 (LoadOp (var __i32) (op Load32) (addr (Ref 4)) (offset -20)))
       (20 (Const __i32 0))
       (21 (BiOp (var __i32) (op And) (lhs (Ref 19)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 21)) (offset -20)))
       (23 (LoadOp (var __i32) (op Load32) (addr (Ref 4)) (offset -28)))
       (24 (Const __i32 0))
       (25 (BiOp (var __i32) (op And) (lhs (Ref 23)) (rhs (Ref 24))))
       (26 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 25)) (offset -28)))
       (27 (Const __i32 -28))
       (28 (BiOp (var eax) (op Add) (lhs (Ref 4)) (rhs (Ref 27))))
       (29 (Const __i32 4))
       (30 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 29))))
       (31 (StoreOp (op Store32) (addr (Ref 30)) (value (Ref 28))))
       (32 (Const __i32 0)) (33 (Const __i32 4))
       (34 (BiOp (var esp) (op Subtract) (lhs (Ref 30)) (rhs (Ref 33))))
       (35 (StoreOp (op Store32) (addr (Ref 34)) (value (Ref 32))))
       (36 (Const __i32 0)) (37 (Const __i32 4))
       (38 (BiOp (var esp) (op Subtract) (lhs (Ref 34)) (rhs (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 38)) (value (Ref 36))))
       (40 (LoadOp (var eax) (op Load32) (addr (Ref 4)) (offset -76)))
       (41 (LoadOp (var eax) (op Load32) (addr (Ref 40)) (offset 8)))
       (42 (LoadOp (var ecx) (op Load32) (addr (Ref 4)) (offset -76)))
       (43 (LoadOp (var ecx) (op Load32) (addr (Ref 42)) (offset 8)))
       (44 (LoadOp (var eax) (op Load32) (addr (Ref 41)))) (45 (Const __i32 4))
       (46 (BiOp (var esp) (op Subtract) (lhs (Ref 38)) (rhs (Ref 45))))
       (47 (StoreOp (op Store32) (addr (Ref 46)) (value (Ref 43))))
       (48 (Const __i32 64))
       (49 (BiOp (var __i32) (op Add) (lhs (Ref 44)) (rhs (Ref 48))))
       (50 (Const __i32 4))
       (51 (BiOp (var esp) (op Subtract) (lhs (Ref 46)) (rhs (Ref 50))))
       (52 (Const __i32 4429352))
       (53 (StoreOp (op Store32) (addr (Ref 51)) (value (Ref 52))))
       (54 (SetGlobalOp (value (Ref 43)) (global ((name ecx) (typ Int)))))
       (55 (SetGlobalOp (value (Ref 44)) (global ((name eax) (typ Int)))))
       (56 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))
       (57 (CallIndirectOp (table_index (Ref 49)) (args ((Ref 51)))))
       (58 (ReturnedOp (var esp) (typ Int))) (59 (Const eax 0))
       (60 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (61 (Const __i32 -16))
       (62 (BiOp (var edi) (op Add) (lhs (Ref 60)) (rhs (Ref 61))))
       (63 (StoreOp (op Store32) (addr (Ref 62)) (value (Ref 59))))
       (64 (Const __i32 4))
       (65 (BiOp (var edi) (op Add) (lhs (Ref 62)) (rhs (Ref 64))))
       (66 (StoreOp (op Store32) (addr (Ref 65)) (value (Ref 59))))
       (67 (Const __i32 4))
       (68 (BiOp (var edi) (op Add) (lhs (Ref 65)) (rhs (Ref 67))))
       (69 (StoreOp (op Store32) (addr (Ref 68)) (value (Ref 59))))
       (70 (Const __i32 4))
       (71 (BiOp (var edi) (op Add) (lhs (Ref 68)) (rhs (Ref 70))))
       (72 (Const __i32 0))
       (73 (StoreOp (op Store16) (addr (Ref 71)) (value (Ref 72))))
       (74 (Const __i32 2))
       (75 (BiOp (var edi) (op Add) (lhs (Ref 71)) (rhs (Ref 74))))
       (76 (Const __i32 4812696))
       (77 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 76)) (signed true)))
       (78 Nop) (79 Nop) (80 Nop)
       (81 (StoreOp (op Store16) (addr (Ref 60)) (value (Ref 77)) (offset -16)))
       (82 (Const __i32 54))
       (83 (StoreOp (op Store32) (addr (Ref 60)) (value (Ref 82)) (offset -6)))
       (84 (LoadOp (var eax) (op Load32) (addr (Ref 60)) (offset -6)))
       (85 (StoreOp (op Store32) (addr (Ref 60)) (value (Ref 84)) (offset -14)))
       (86 (LoadOp (var eax) (op Load32) (addr (Ref 60)) (offset -76)))
       (87 (LoadOp (var eax) (op Load32) (addr (Ref 86)) (offset 232)))
       (88 (StoreOp (op Store32) (addr (Ref 60)) (value (Ref 87)) (offset -80)))
       (89 (LoadOp (var __i32) (op Load32) (addr (Ref 60)) (offset -80)))
       (90 (Const __i32 22))
       (91 (BiOp (var __i32) (op Equal) (lhs (Ref 89)) (rhs (Ref 90))))
       (92 (SetGlobalOp (value (Ref 87)) (global ((name eax) (typ Int)))))
       (93 (SetGlobalOp (value (Ref 75)) (global ((name edi) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 91))))
     (roots ((Ref 1) (Ref 58) (Ref 91))))
    |}]

let%expect_test "tail intrinsic" =
  test_trans_block 0x0047d136;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (CallOp (func USER32.dll_WINNLSEnableIME) (args ((Ref 0)))))
       (3 (ReturnedOp (var esp) (typ Int)))))
     (terminator Return) (roots ((Ref 1) (Ref 3))))
    |}]

let%expect_test "movsw" =
  test_trans_block 0x004399e1;
  [%expect
    {|
    ((id 9)
     (instrs
      ((0 Nop) (1 (Const __i32 1)) (2 (Const __i32 5724808))
       (3 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 1)))) (4 Nop)
       (5 (Const __i32 1)) (6 (Const __i32 5724809))
       (7 (StoreOp (op Store8) (addr (Ref 6)) (value (Ref 5))))
       (8 (Const __i32 5724810))
       (9 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 8)) (signed true)))
       (10 Nop) (11 Nop) (12 (Const __i32 0))
       (13 (BiOp (var __i32) (op And) (lhs (Ref 9)) (rhs (Ref 12))))
       (14 (Const __i32 5724810))
       (15 (StoreOp (op Store8) (addr (Ref 14)) (value (Ref 13))))
       (16 (Const __i32 5724811))
       (17 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 16)) (signed true)))
       (18 Nop) (19 Nop) (20 (Const __i32 0))
       (21 (BiOp (var __i32) (op And) (lhs (Ref 17)) (rhs (Ref 20))))
       (22 (Const __i32 5724811))
       (23 (StoreOp (op Store8) (addr (Ref 22)) (value (Ref 21))))
       (24 (Const esi 4845120)) (25 (Const edi 5724776))
       (26 (LoadOp (var __i32) (op Load32) (addr (Ref 24))))
       (27 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 26)))) (28 Nop)
       (29 (Const edi 5724780)) (30 Nop) (31 (Const esi 4845124))
       (32 (LoadOp (var __i32) (op Load32) (addr (Ref 31))))
       (33 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 32)))) (34 Nop)
       (35 (Const edi 5724784)) (36 Nop) (37 (Const esi 4845128))
       (38 (LoadOp (var __i32) (op Load32) (addr (Ref 37))))
       (39 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 38)))) (40 Nop)
       (41 (Const edi 5724788)) (42 Nop) (43 (Const esi 4845132))
       (44 (LoadOp (var __i32) (op Load32) (addr (Ref 43))))
       (45 (StoreOp (op Store32) (addr (Ref 41)) (value (Ref 44)))) (46 Nop)
       (47 (Const edi 5724792)) (48 Nop) (49 (Const esi 4845136))
       (50 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 49)) (signed true)))
       (51 Nop) (52 (StoreOp (op Store16) (addr (Ref 47)) (value (Ref 50))))
       (53 Nop) (54 (Const edi 5724794)) (55 Nop) (56 (Const esi 4845138))
       (57 Nop) (58 (Const __i32 2)) (59 (Const __i32 5724812))
       (60 (StoreOp (op Store8) (addr (Ref 59)) (value (Ref 58))))
       (61 (Const __i32 5724813))
       (62 (SignedLoadOp (var __i32) (op Load8) (addr (Ref 61)) (signed true)))
       (63 Nop) (64 Nop) (65 (Const __i32 0))
       (66 (BiOp (var __i32) (op And) (lhs (Ref 62)) (rhs (Ref 65))))
       (67 (Const __i32 5724813))
       (68 (StoreOp (op Store8) (addr (Ref 67)) (value (Ref 66)))) (69 Nop)
       (70 (Const __i32 1)) (71 (Const __i32 5724814))
       (72 (StoreOp (op Store8) (addr (Ref 71)) (value (Ref 70))))
       (73 (SetGlobalOp (value (Ref 56)) (global ((name esi) (typ Int)))))
       (74 (SetGlobalOp (value (Ref 54)) (global ((name edi) (typ Int)))))))
     (terminator (Goto (Block 30))) (roots ()))
    |}]

let%expect_test "sub jne" =
  test_trans_block 0x0047dc43;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int))))) (1 Nop)
       (2 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (3 (StoreOp (op Store8) (addr (Ref 2)) (value (Ref 0))))
       (4 (Const __i32 1))
       (5 (BiOp (var edi) (op Add) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (7 (Const __i32 1))
       (8 (BiOp (var edx) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (SetGlobalOp (value (Ref 8)) (global ((name edx) (typ Int)))))
       (10 (SetGlobalOp (value (Ref 5)) (global ((name edi) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 7)) (fail (Block 8)) (condition (Ref 8))))
     (roots ((Ref 8))))
    |}]

let%expect_test "rep stosd (nonzero)" =
  test_trans_block 0x0042d5ee;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const ecx 180))
       (1 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (2 (Const __i32 4294967295))
       (3 (BiOp (var eax) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (Const edi 5724496))
       (5 (CallOp (func __int_memset__) (args ((Ref 4) (Ref 3) (Ref 0)))))
       (6 (Const ecx 0))
       (7 (SetGlobalOp (value (Ref 6)) (global ((name ecx) (typ Int)))))
       (8 (SetGlobalOp (value (Ref 3)) (global ((name eax) (typ Int)))))
       (9 (SetGlobalOp (value (Ref 4)) (global ((name edi) (typ Int)))))))
     (terminator (Goto (Block 2))) (roots ()))
    |}]

let%expect_test "rep movsb" =
  test_trans_block 0x004446bb;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (3 (Const __i32 4))
       (4 (BiOp (var __i32) (op Multiply) (lhs (Ref 1)) (rhs (Ref 3))))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 4)) (rhs (Ref 2))))
       (6 (LoadOp (var ecx) (op Load32) (addr (Ref 5)) (offset 36)))
       (7 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 6)) (offset -24)))
       (8 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -24)))
       (9 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -8)))
       (10 (LoadOp (var eax) (op Load32) (addr (Ref 9)) (offset 4)))
       (11 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (12 (Const __i32 4))
       (13 (BiOp (var __i32) (op Multiply) (lhs (Ref 11)) (rhs (Ref 12))))
       (14 (BiOp (var __i32) (op Add) (lhs (Ref 13)) (rhs (Ref 10))))
       (15 (LoadOp (var esi) (op Load32) (addr (Ref 14)) (offset 56)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -272)))
       (17 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -28)))
       (18 (BiOp (var __i32) (op Add) (lhs (Ref 16)) (rhs (Ref 17))))
       (19 (Const __i32 -84))
       (20 (BiOp (var edi) (op Add) (lhs (Ref 18)) (rhs (Ref 19)))) (21 Nop)
       (22 Nop) (23 (Const __i32 2))
       (24
        (SignedBiOp (var ecx) (op ShiftRight) (signed false) (lhs (Ref 8))
         (rhs (Ref 23))))
       (25 (Const __i32 2))
       (26 (BiOp (var __i32) (op ShiftLeft) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (Memcopy (count (Ref 26)) (src (Ref 15)) (dest (Ref 20))))
       (28 (BiOp (var esi) (op Add) (lhs (Ref 15)) (rhs (Ref 26))))
       (29 (BiOp (var edi) (op Add) (lhs (Ref 20)) (rhs (Ref 26)))) (30 Nop)
       (31 Nop) (32 (Const __i32 3))
       (33 (BiOp (var ecx) (op And) (lhs (Ref 8)) (rhs (Ref 32))))
       (34 (Memcopy (count (Ref 33)) (src (Ref 28)) (dest (Ref 29))))
       (35 (BiOp (var esi) (op Add) (lhs (Ref 28)) (rhs (Ref 33))))
       (36 (BiOp (var edi) (op Add) (lhs (Ref 29)) (rhs (Ref 33)))) (37 Nop)
       (38 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (39 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -272)))
       (40 (Const __i32 4))
       (41 (BiOp (var __i32) (op Multiply) (lhs (Ref 38)) (rhs (Ref 40))))
       (42 (BiOp (var __i32) (op Add) (lhs (Ref 41)) (rhs (Ref 0))))
       (43 (StoreOp (op Store32) (addr (Ref 42)) (value (Ref 39)) (offset -208)))
       (44 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -272)))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (46 (BiOp (var eax) (op Add) (lhs (Ref 44)) (rhs (Ref 45))))
       (47 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 46)) (offset -272)))
       (48 (SetGlobalOp (value (Ref 39)) (global ((name edx) (typ Int)))))
       (49 (SetGlobalOp (value (Ref 38)) (global ((name ecx) (typ Int)))))
       (50 (SetGlobalOp (value (Ref 35)) (global ((name esi) (typ Int)))))
       (51 (SetGlobalOp (value (Ref 46)) (global ((name eax) (typ Int)))))
       (52 (SetGlobalOp (value (Ref 36)) (global ((name edi) (typ Int)))))))
     (terminator (Goto (Block 21))) (roots ()))
    |}]

let%expect_test "repe cmpsd" =
  test_trans_block 0x00444521;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (Const ecx 14))
       (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1)) (offset -8)))
       (3 (LoadOp (var edi) (op Load32) (addr (Ref 2)) (offset 4)))
       (4 (Const __i32 112))
       (5 (BiOp (var edi) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (Const esi 5724776)) (7 (Const eax 0))
       (8 (CallOp (func __int_diff__) (args ((Ref 6) (Ref 5) (Ref 0)))))
       (9 (ReturnedOp (var esi) (typ Int))) (10 (ReturnedOp (var edi) (typ Int)))
       (11 (ReturnedOp (var ecx) (typ Int)))
       (12 (LoadOp (var __i32) (op Load32) (addr (Ref 9))))
       (13 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (14 (BiOp (var __i32) (op Equal) (lhs (Ref 12)) (rhs (Ref 13))))
       (15 (SetGlobalOp (value (Ref 2)) (global ((name edx) (typ Int)))))
       (16 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))
       (17 (SetGlobalOp (value (Ref 9)) (global ((name esi) (typ Int)))))
       (18 (SetGlobalOp (value (Ref 7)) (global ((name eax) (typ Int)))))
       (19 (SetGlobalOp (value (Ref 10)) (global ((name edi) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 5)) (fail (Block 4)) (condition (Ref 14))))
     (roots ((Ref 14))))
    |}]

let%expect_test "and jns" =
  test_trans_block 0x0043dbb0;
  [%expect
    {|
    ((id 18)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var edx) (op Load32) (addr (Ref 1)) (offset 836)))
       (3 (Const __i32 2147483649))
       (4 (BiOp (var edx) (op And) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 4))
         (rhs (Ref 5))))
       (7 (SetGlobalOp (value (Ref 4)) (global ((name edx) (typ Int)))))
       (8 (SetGlobalOp (value (Ref 1)) (global ((name ecx) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 6))))
     (roots ((Ref 6))))
    |}]

let%expect_test "fidivr" =
  test_trans_block 0x00414d50;
  [%expect
    {|
    ((id 818)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 11196)))
       (3 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 2))))
       (4 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1656)))
       (5 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 4))))
       (6 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 5)) (rhs (Ref 3))))
       (7 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 6)) (offset -616)))
       (8 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 8)) (offset 11196)))
       (10 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 9))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -1660)))
       (12 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 11))))
       (13 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 12)) (rhs (Ref 10))))
       (14 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 13)) (offset -620)))
       (15 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -60)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -620)))
       (17 (Const __i32 4))
       (18 (BiOp (var __i32) (op Multiply) (lhs (Ref 15)) (rhs (Ref 17))))
       (19
        (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 16)) (offset 4848708)))
       (20 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (21 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -616)))
       (22 (Const __i32 4))
       (23 (BiOp (var __i32) (op Multiply) (lhs (Ref 20)) (rhs (Ref 22))))
       (24
        (StoreOp (op Store32) (addr (Ref 23)) (value (Ref 21)) (offset 4848676)))
       (25 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (26
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 25)) (signed false)
         (offset 10)))
       (27 Nop) (28 (Const __i32 8))
       (29 (BiOp (var ecx) (op And) (lhs (Ref 26)) (rhs (Ref 28))))
       (30 (UniOp (var __i32) (op EqualsZero) (operand (Ref 29)))) (31 Nop)
       (32 (SetGlobalOp (value (Ref 21)) (global ((name edx) (typ Int)))))
       (33 (SetGlobalOp (value (Ref 29)) (global ((name ecx) (typ Int)))))
       (34 (SetGlobalOp (value (Ref 25)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 820)) (fail (Block 819)) (condition (Ref 30))))
     (roots ((Ref 30))))
    |}]

let%expect_test "tib offset 0" =
  test_trans_block 0x0043009a;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
       (7 (Const __i32 -1)) (8 (Const __i32 4))
       (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
       (11 (Const __i32 4768955)) (12 (Const __i32 4))
       (13 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 13)) (value (Ref 11))))
       (15 (GetGlobalOp (var eax) (global ((name __seh_frame__) (typ Int)))))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 13)) (rhs (Ref 16))))
       (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 15))))
       (19
        (SetGlobalOp (value (Ref 17)) (global ((name __seh_frame__) (typ Int)))))
       (20 (Const __i32 20))
       (21 (BiOp (var esp) (op Subtract) (lhs (Ref 17)) (rhs (Ref 20))))
       (22 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (23 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 22)) (offset -28)))
       (24 (Const __i32 32)) (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 24))))
       (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 26)) (rhs (Ref 28))))
       (30 (Const __i32 4391088))
       (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
       (32 (SetGlobalOp (value (Ref 15)) (global ((name eax) (typ Int)))))
       (33 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))
       (34 (CallOp (func __func47d441__) (args ((Ref 29)))))
       (35 (ReturnedOp (var esp) (typ Int))) (36 (Const __i32 4))
       (37 (BiOp (var esp) (op Add) (lhs (Ref 35)) (rhs (Ref 36))))
       (38 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (39 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (40 (StoreOp (op Store32) (addr (Ref 39)) (value (Ref 38)) (offset -24)))
       (41 (Const __i32 0))
       (42 (StoreOp (op Store32) (addr (Ref 39)) (value (Ref 41)) (offset -4)))
       (43 (LoadOp (var __i32) (op Load32) (addr (Ref 39)) (offset -24)))
       (44 (Const __i32 0))
       (45 (BiOp (var __i32) (op Equal) (lhs (Ref 43)) (rhs (Ref 44))))))
     (terminator
      (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 45))))
     (roots ((Ref 1) (Ref 37) (Ref 45))))
    |}]

let%expect_test "tail call" =
  test_trans 0x0047d43c;
  [%expect
    {|
    ((name func_47d43c)
     (signature
      ((args (((name esp) (typ Int)))) (returns (((name esp) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (CallOp (func __func47d285__) (args ((Ref 0)))))
          (3 (ReturnedOp (var esp) (typ Int)))))
        (terminator Return) (roots ((Ref 1) (Ref 3))))))
     (locals
      ((__ret_addr__ ((name __ret_addr__) (typ Int)))
       (esp ((name esp) (typ Int))))))
    (parent ((0 -1)))
    (preorder ((0 0)))
    (inv_preorder ((0 0)))
    (semidom ((0 -1)))
    (ancestor ((0 -1)))
    (label ((0 0)))
    (graph ((0 ())))
    (preds ((0 ())))
    (idoms1 ((0 0)))
    (idoms2 ((0 0)))
    ((rpnum ((0 0))) (dom_tree ((0 (0)))))
    (preds ((0 ())))
    (merge_blocks ())
    ((x 0) (children (0)) (children_within ()))
    ((x 0) (ys ()) (c ()))
    (WasmSeq (WasmCode 0) WasmReturn)
    |}]

let%expect_test "shl reg" =
  test_trans_block 0x0040f3de;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
         (offset 8)))
       (2 Nop) (3 (Const edx 1))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 12)))
       (5 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 4))))
       (6 (BiOp (var edx) (op ShiftLeft) (lhs (Ref 3)) (rhs (Ref 5))))
       (7 (BiOp (var eax) (op And) (lhs (Ref 1)) (rhs (Ref 6))))
       (8 (SetGlobalOp (value (Ref 6)) (global ((name edx) (typ Int)))))
       (9 (SetGlobalOp (value (Ref 4)) (global ((name ecx) (typ Int)))))
       (10 (SetGlobalOp (value (Ref 7)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 7))))
     (roots ((Ref 7))))
    |}]

let%expect_test "cld" =
  test_trans_block 0x0048c237;
  [%expect
    {|
    ((id 20)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (Const __i32 8))
       (2 (BiOp (var esi) (op Add) (lhs (Ref 0)) (rhs (Ref 1))))
       (3 (Const __i32 -134))
       (4 (BiOp (var edi) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (6 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 5))))
       (7 (Const __i32 4))
       (8 (BiOp (var edi) (op Add) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (Const __i32 4))
       (10 (BiOp (var esi) (op Add) (lhs (Ref 2)) (rhs (Ref 9))))
       (11 (LoadOp (var __i32) (op Load32) (addr (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var edi) (op Add) (lhs (Ref 8)) (rhs (Ref 13))))
       (15 (Const __i32 4))
       (16 (BiOp (var esi) (op Add) (lhs (Ref 10)) (rhs (Ref 15))))
       (17 (GetGlobalOp (var ebx) (global ((name ebx) (typ Int)))))
       (18
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 17)) (signed true)
         (offset 12)))
       (19 Nop) (20 Nop) (21 (Const __i32 1))
       (22 (BiOp (var __i32) (op Equal) (lhs (Ref 18)) (rhs (Ref 21))))
       (23 (SetGlobalOp (value (Ref 16)) (global ((name esi) (typ Int)))))
       (24 (SetGlobalOp (value (Ref 14)) (global ((name edi) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 22))))
     (roots ((Ref 22))))
    |}]

let%expect_test "fscale/fabs/fcomp sahf jae" =
  test_trans_block 0x0048c1e9;
  [%expect
    {|
    ((id 15)
     (instrs
      ((0 (Const __i32 4))
       (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -142)))
       (3 (Const __i32 4820792))
       (4 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 3))))
       (5
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
       (7 (CallOp (func __float_scale__) (args ((Ref 6) (Ref 4)))))
       (8 (ReturnedOp (var __fl) (typ Float)))
       (9 (UniOp (var __fl) (op FloatAbs) (operand (Ref 8))))
       (10 (Const __i32 4820776))
       (11 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 10))))
       (12 (Landmine (var eax) (typ Int)))
       (13
        (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 9))
         (rhs (Ref 11))))
       (14 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 8))))
       (15 (SetGlobalOp (value (Ref 12)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 19)) (fail (Block 16)) (condition (Ref 13))))
     (roots ((Ref 13))))
    |}]

let%expect_test "fscale/fabs/fcomp sahf jbe" =
  test_trans_block 0x0048c217;
  [%expect
    {|
    ((id 17)
     (instrs
      ((0 (Const __i32 3))
       (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -142)))
       (3 (Const __i32 4820784))
       (4 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 3))))
       (5
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (6 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 5))))
       (7 (CallOp (func __float_scale__) (args ((Ref 6) (Ref 4)))))
       (8 (ReturnedOp (var __fl) (typ Float)))
       (9 (UniOp (var __fl) (op FloatAbs) (operand (Ref 8))))
       (10 (Const __i32 4820768))
       (11 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 10))))
       (12 (Landmine (var eax) (typ Int)))
       (13 (BiOp (var __i32) (op FloatGreaterThan) (lhs (Ref 9)) (rhs (Ref 11))))
       (14 (UniOp (var __i32) (op EqualsZero) (operand (Ref 13))))
       (15 (StoreOp (op FloatStore64) (addr (Ref 5)) (value (Ref 8))))
       (16 (SetGlobalOp (value (Ref 12)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 19)) (fail (Block 18)) (condition (Ref 14))))
     (roots ((Ref 14))))
    |}]

let%expect_test "and je" =
  test_trans_block 0x0048c178;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed true)
         (offset -164)))
       (2 Nop) (3 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (4 Nop) (5 Nop) (6 Nop) (7 (Const __i32 32))
       (8 (BiOp (var __i32) (op And) (lhs (Ref 1)) (rhs (Ref 7))))
       (9 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 8)) (rhs (Ref 3))))
       (10 (UniOp (var __i32) (op SignExtend16) (operand (Ref 8))))
       (11 (SetGlobalOp (value (Ref 9)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 7)) (condition (Ref 10))))
     (roots ((Ref 10))))
    |}]

let%expect_test "or je" =
  test_trans_block 0x0048c15b;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (1 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (2 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 0))))
       (3 (BiOp (var __i32) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 3)) (rhs (Ref 0))))
       (5 (UniOp (var __i32) (op SignExtendLow8) (operand (Ref 3))))
       (6 (UniOp (var __i32) (op EqualsZero) (operand (Ref 5))))
       (7 (SetGlobalOp (value (Ref 4)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 6))))
     (roots ((Ref 6))))
    |}]

let%expect_test "xchg" =
  test_trans_block 0x0047f3b0;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (1 (Const __i32 0))
       (2 (BiOp (var eax) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
       (3 (OutsideContext (var esp) (typ Int)))
       (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
       (5 (Const __i32 4))
       (6 (BiOp (var eax) (op Add) (lhs (Ref 4)) (rhs (Ref 5)))) (7 Nop)
       (8 Nop) (9 Nop) (10 (LoadOp (var eax) (op Load32) (addr (Ref 3))))
       (11 (Const __i32 4))
       (12 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
       (14 (LoadOp (var __i32) (op Load32) (addr (Ref 12))))
       (15 (OutsideContext (var __ret_addr__) (typ Int)))
       (16 (BiOp (var __i32) (op Equal) (lhs (Ref 14)) (rhs (Ref 15))))
       (17 (AssertOp (condition (Ref 16)))) (18 (Const __i32 4))
       (19 (BiOp (var esp) (op Add) (lhs (Ref 12)) (rhs (Ref 18))))
       (20 (SetGlobalOp (value (Ref 10)) (global ((name eax) (typ Int)))))))
     (terminator Return) (roots ((Ref 15) (Ref 19))))
    |}]

let%expect_test "fsqrt" =
  test_trans_block 0x00461f7b;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (CallOp (func __float_sqrt__) (args ((Ref 1)))))
       (3 (ReturnedOp (var __fl) (typ Float))) (4 (Const __i32 4819540))
       (5 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 4))))
       (6 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 5)) (rhs (Ref 3))))
       (7 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (11 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 9)) (offset -12)))
       (12 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7)) (offset 4)))
       (13 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 12))))
       (14 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 13)) (offset -8)))
       (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7)) (offset 8)))
       (16 (BiOp (var __fl) (op FloatMult) (lhs (Ref 6)) (rhs (Ref 15))))
       (17 (Const __i32 -12))
       (18 (BiOp (var esi) (op Add) (lhs (Ref 10)) (rhs (Ref 17))))
       (19 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 16)) (offset -4)))
       (20 (Const __i32 -8))
       (21 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 0)) (rhs (Ref 20))))
       (22 (SetGlobalOp (value (Ref 18)) (global ((name esi) (typ Int)))))
       (23
        (SetGlobalOp (value (Ref 21)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 3))) (roots ()))
    |}]

let%expect_test "movsd" =
  test_trans_block 0x00461f64;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int))))) (1 Nop)
       (2 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3))))
       (5 (Const __i32 4))
       (6 (BiOp (var edi) (op Add) (lhs (Ref 0)) (rhs (Ref 5))))
       (7 (Const __i32 4))
       (8 (BiOp (var esi) (op Add) (lhs (Ref 2)) (rhs (Ref 7))))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 8))))
       (10 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 9))))
       (11 (Const __i32 4))
       (12 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 11))))
       (13 (Const __i32 4))
       (14 (BiOp (var esi) (op Add) (lhs (Ref 8)) (rhs (Ref 13))))
       (15 (LoadOp (var __i32) (op Load32) (addr (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 15))))
       (17 (Const __i32 4))
       (18 (BiOp (var edi) (op Add) (lhs (Ref 12)) (rhs (Ref 17))))
       (19 (Const __i32 4))
       (20 (BiOp (var esi) (op Add) (lhs (Ref 14)) (rhs (Ref 19))))
       (21 (SetGlobalOp (value (Ref 20)) (global ((name esi) (typ Int)))))
       (22 (SetGlobalOp (value (Ref 18)) (global ((name edi) (typ Int)))))))
     (terminator (Goto (Block 8))) (roots ()))
    |}]

let%expect_test "sbb" =
  test_trans_block 0x0048b8f3;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 1))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 2))))
       (5 (Const __i32 2147483647))
       (6 (BiOp (var ecx) (op Add) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (Const __i32 0))
       (8
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 6))
         (rhs (Ref 4))))
       (9 (BiOp (var __i32) (op Add) (lhs (Ref 8)) (rhs (Ref 7))))
       (10 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (11 (BiOp (var eax) (op Subtract) (lhs (Ref 10)) (rhs (Ref 9))))
       (12 (LoadOp (var edx) (op Load32) (addr (Ref 2)) (offset 20)))
       (13 (Const __i32 0))
       (14
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 11))
         (rhs (Ref 10))))
       (15 (BiOp (var __i32) (op Add) (lhs (Ref 14)) (rhs (Ref 13))))
       (16 (BiOp (var edx) (op Subtract) (lhs (Ref 12)) (rhs (Ref 15))))
       (17 (Const __i32 -8))
       (18 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 0)) (rhs (Ref 17))))
       (19 (SetGlobalOp (value (Ref 16)) (global ((name edx) (typ Int)))))
       (20 (SetGlobalOp (value (Ref 6)) (global ((name ecx) (typ Int)))))
       (21 (SetGlobalOp (value (Ref 11)) (global ((name eax) (typ Int)))))
       (22
        (SetGlobalOp (value (Ref 18)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6))) (roots ((Ref 2))))
    |}]

let%expect_test "adc" =
  test_trans_block 0x0048b8db;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 1))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 2))))
       (5 (Const __i32 2147483648))
       (6 (BiOp (var ecx) (op Xor) (lhs (Ref 4)) (rhs (Ref 5))))
       (7 (Const __i32 2147483647))
       (8 (BiOp (var ecx) (op Add) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (Const __i32 0))
       (10
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 8))
         (rhs (Ref 6))))
       (11 (BiOp (var __i32) (op Add) (lhs (Ref 10)) (rhs (Ref 9))))
       (12 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (13 (BiOp (var eax) (op Add) (lhs (Ref 11)) (rhs (Ref 12))))
       (14 (LoadOp (var edx) (op Load32) (addr (Ref 2)) (offset 20)))
       (15 (Const __i32 0))
       (16
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 13))
         (rhs (Ref 11))))
       (17 (BiOp (var __i32) (op Add) (lhs (Ref 16)) (rhs (Ref 15))))
       (18 (BiOp (var edx) (op Add) (lhs (Ref 17)) (rhs (Ref 14))))
       (19 (Const __i32 -8))
       (20 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 0)) (rhs (Ref 19))))
       (21 (SetGlobalOp (value (Ref 18)) (global ((name edx) (typ Int)))))
       (22 (SetGlobalOp (value (Ref 8)) (global ((name ecx) (typ Int)))))
       (23 (SetGlobalOp (value (Ref 13)) (global ((name eax) (typ Int)))))
       (24
        (SetGlobalOp (value (Ref 20)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator (Goto (Block 6))) (roots ((Ref 2))))
    |}]

let%expect_test "test reflexive jns" =
  test_trans_block 0x0048b8c7;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0))))
       (2 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 0)) (offset -8)))
       (3 (BiOp (var __fl) (op FloatSub) (lhs (Ref 2)) (rhs (Ref 1))))
       (4 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
       (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 4))
         (rhs (Ref 5))))
       (7 (StoreOp (op FloatStore64) (addr (Ref 0)) (value (Ref 3)) (offset -8)))
       (8 (Const __i32 -8))
       (9 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 0)) (rhs (Ref 8))))
       (10
        (SetGlobalOp (value (Ref 9)) (global ((name __fpuStack__) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 6))))
     (roots ((Ref 6))))
    |}]

let%expect_test "fistp" =
  test_trans_block 0x0048b8af;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
       (7 (Const __i32 32))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (Const __i32 4294967280))
       (10 (BiOp (var esp) (op And) (lhs (Ref 8)) (rhs (Ref 9))))
       (11
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (12 (LoadOp (var __fl) (op FloatLoad64) (addr (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 12)) (offset 24)))
       (14 (UniOp (var __i64) (op FloatToLong) (operand (Ref 12))))
       (15
        (StoreOp (op LongStore64) (addr (Ref 10)) (value (Ref 14)) (offset 16)))
       (16 (LoadOp (var __i64) (op LongLoad64) (addr (Ref 10)) (offset 16)))
       (17 (UniOp (var __fl) (op Int64ToFloatSigned) (operand (Ref 16))))
       (18 (LoadOp (var edx) (op Load32) (addr (Ref 10)) (offset 24)))
       (19 (LoadOp (var eax) (op Load32) (addr (Ref 10)) (offset 16)))
       (20 (UniOp (var __i32) (op EqualsZero) (operand (Ref 19))))
       (21 (StoreOp (op FloatStore64) (addr (Ref 11)) (value (Ref 12))))
       (22
        (StoreOp (op FloatStore64) (addr (Ref 11)) (value (Ref 17)) (offset 8)))
       (23 (Const __i32 8))
       (24 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 11)) (rhs (Ref 23))))
       (25 (SetGlobalOp (value (Ref 18)) (global ((name edx) (typ Int)))))
       (26 (SetGlobalOp (value (Ref 19)) (global ((name eax) (typ Int)))))
       (27
        (SetGlobalOp (value (Ref 24)) (global ((name __fpuStack__) (typ Int)))))
       (28 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 20))))
     (roots ((Ref 1) (Ref 10) (Ref 20))))
    |}]

let%expect_test "dec/dec js" =
  test_trans_block 0x0047d173;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (Const __i32 1))
       (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset -28)))
       (3 (BiOp (var __i32) (op Subtract) (lhs (Ref 2)) (rhs (Ref 0))))
       (4 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 3)) (offset -28)))
       (5 (Const __i32 0))
       (6
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 3))
         (rhs (Ref 5))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 6))))
     (roots ((Ref 6))))
    |}]

let%expect_test "jb" =
  test_trans_block 0x0044e4f5;
  [%expect
    {|
    ((id 1)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (Const __i32 50))
       (3
        (SignedBiOp (var __i32) (op LessThan) (signed false) (lhs (Ref 1))
         (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test "fsubr" =
  test_trans_block 0x00404715;
  [%expect
    {|
    ((id 7)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
         (offset 37)))
       (3 Nop) (4 (Const __i32 2))
       (5 (BiOp (var ecx) (op ShiftLeft) (lhs (Ref 2)) (rhs (Ref 4))))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -64)))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -64)))
       (8 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 7))))
       (9 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9)) (offset 8)))
       (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 10)) (rhs (Ref 8))))
       (12 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -60)))
       (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 11)) (offset 1044)))
       (14 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (15 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (16 (LoadOp (var eax) (op Load32) (addr (Ref 15)) (offset 12)))
       (17 (StoreOp (op Store32) (addr (Ref 14)) (value (Ref 16)) (offset 1048)))
       (18 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -60)))
       (19 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (20 (LoadOp (var eax) (op Load32) (addr (Ref 19)) (offset 20)))
       (21 (StoreOp (op Store32) (addr (Ref 18)) (value (Ref 20)) (offset 1028)))
       (22 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (23 (Const __i32 4973576))
       (24 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 23))))
       (25 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 22)) (offset 8)))
       (26 (BiOp (var __fl) (op FloatSub) (lhs (Ref 24)) (rhs (Ref 25))))
       (27 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 26)) (offset -16)))
       (28 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (29 (Const __i32 4973580))
       (30 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 29))))
       (31 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 28)) (offset 12)))
       (32 (BiOp (var __fl) (op FloatSub) (lhs (Ref 30)) (rhs (Ref 31))))
       (33 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 32)) (offset -12)))
       (34 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (35 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (36 (BiOp (var __fl) (op FloatMult) (lhs (Ref 34)) (rhs (Ref 35))))
       (37 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (38 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -12)))
       (39 (BiOp (var __fl) (op FloatMult) (lhs (Ref 37)) (rhs (Ref 38))))
       (40 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 36)) (rhs (Ref 39))))
       (41 (Const __i32 4)) (42 (OutsideContext (var esp) (typ Int)))
       (43 (BiOp (var esp) (op Subtract) (lhs (Ref 42)) (rhs (Ref 41))))
       (44 (Const __i32 4212587))
       (45 (StoreOp (op Store32) (addr (Ref 43)) (value (Ref 44))))
       (46
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (47
        (StoreOp (op FloatStore64) (addr (Ref 46)) (value (Ref 40)) (offset 8)))
       (48 (Const __i32 8))
       (49 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 46)) (rhs (Ref 48))))
       (50 (SetGlobalOp (value (Ref 28)) (global ((name edx) (typ Int)))))
       (51 (SetGlobalOp (value (Ref 22)) (global ((name ecx) (typ Int)))))
       (52 (SetGlobalOp (value (Ref 20)) (global ((name eax) (typ Int)))))
       (53
        (SetGlobalOp (value (Ref 49)) (global ((name __fpuStack__) (typ Int)))))
       (54 (CallOp (func __func48b8a0__) (args ((Ref 43)))))
       (55 (ReturnedOp (var esp) (typ Int)))
       (56 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (57 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (58 (StoreOp (op Store32) (addr (Ref 57)) (value (Ref 56)) (offset -8)))
       (59 (LoadOp (var __i32) (op Load32) (addr (Ref 57)) (offset -8)))
       (60 (Const __i32 4096))
       (61
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 59))
         (rhs (Ref 60))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 8)) (condition (Ref 61))))
     (roots ((Ref 55) (Ref 61))))
    |}]

let%expect_test "rep movsd" =
  test_trans_block 0x00403ad5;
  [%expect
    {|
    ((id 3)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var esi) (op Load32) (addr (Ref 0)) (offset -604)))
       (2 (Const __i32 5888))
       (3 (BiOp (var esi) (op Add) (lhs (Ref 1)) (rhs (Ref 2)))) (4 Nop)
       (5 (Const __i32 -600))
       (6 (BiOp (var edi) (op Add) (lhs (Ref 0)) (rhs (Ref 5)))) (7 Nop)
       (8 (Const __i32 588))
       (9 (Memcopy (count (Ref 8)) (src (Ref 3)) (dest (Ref 6))))
       (10 (BiOp (var esi) (op Add) (lhs (Ref 3)) (rhs (Ref 8))))
       (11 (BiOp (var edi) (op Add) (lhs (Ref 6)) (rhs (Ref 8)))) (12 Nop)
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -152)))
       (14 (Const __i32 4096))
       (15 (BiOp (var eax) (op Or) (lhs (Ref 13)) (rhs (Ref 14))))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 15)) (offset -152)))
       (17 (Const __i32 -600))
       (18 (BiOp (var ecx) (op Add) (lhs (Ref 0)) (rhs (Ref 17))))
       (19 (OutsideContext (var esp) (typ Int))) (20 (Const __i32 4))
       (21 (BiOp (var esp) (op Subtract) (lhs (Ref 19)) (rhs (Ref 20))))
       (22 (StoreOp (op Store32) (addr (Ref 21)) (value (Ref 18))))
       (23 (Const __i32 4955716))
       (24 (LoadOp (var ecx) (op Load32) (addr (Ref 23)))) (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 25))))
       (27 (Const __i32 4209397))
       (28 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 27))))
       (29 (SetGlobalOp (value (Ref 24)) (global ((name ecx) (typ Int)))))
       (30 (SetGlobalOp (value (Ref 10)) (global ((name esi) (typ Int)))))
       (31 (SetGlobalOp (value (Ref 15)) (global ((name eax) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 11)) (global ((name edi) (typ Int)))))
       (33 (CallOp (func __func44f770__) (args ((Ref 26)))))
       (34 (ReturnedOp (var esp) (typ Int)))))
     (terminator (Goto (Block 4))) (roots ((Ref 34))))
    |}]

let%expect_test "fabs" =
  test_trans_block 0x004023c9;
  [%expect
    {|
    ((id 23)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -8)))
       (2 (Const __i32 588))
       (3 (BiOp (var eax) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -40)))
       (5 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (6 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 5)) (offset 3396)))
       (7 (Const __i32 4819588))
       (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
       (9 (BiOp (var __fl) (op FloatSub) (lhs (Ref 6)) (rhs (Ref 8))))
       (10 (Const __i32 4973576))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
       (12 (BiOp (var __fl) (op FloatSub) (lhs (Ref 9)) (rhs (Ref 11))))
       (13 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 12)) (offset -36)))
       (14 (UniOp (var __fl) (op FloatAbs) (operand (Ref 12))))
       (15 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 14)) (offset -44)))
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 14)) (offset -4)))
       (17 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -4)))
       (18 (Const __i32 4819720))
       (19 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 18))))
       (20 (Landmine (var eax) (typ Int))) (21 Nop)
       (22 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 17)) (rhs (Ref 19))))
       (23 (UniOp (var __i32) (op EqualsZero) (operand (Ref 22)))) (24 Nop)
       (25 (SetGlobalOp (value (Ref 4)) (global ((name ecx) (typ Int)))))
       (26 (SetGlobalOp (value (Ref 20)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 25)) (fail (Block 24)) (condition (Ref 23))))
     (roots ((Ref 23))))
    |}]

let%expect_test "jae" =
  test_trans_block 0x004027fc;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (Const __i32 10))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed false)
         (lhs (Ref 1)) (rhs (Ref 2))))))
     (terminator
      (Branch (succeed (Block 9)) (fail (Block 6)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test "rep stosd (zeroed)" =
  test_trans_block 0x0040118c;
  [%expect
    {|
    ((id 0)
     (instrs
      ((0 (OutsideContext (var esp) (typ Int)))
       (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
       (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (3 (Const __i32 4))
       (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
       (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
       (7 (Const __i32 36))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
       (9 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
       (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 9))))
       (13 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (14 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 13)) (offset -36)))
       (15 (LoadOp (var ecx) (op Load32) (addr (Ref 4)) (offset -36)))
       (16 (Const __i32 4))
       (17 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 16))))
       (18 (Const __i32 4198781))
       (19 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 18))))
       (20 (SetGlobalOp (value (Ref 15)) (global ((name ecx) (typ Int)))))
       (21 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))
       (22 (CallOp (func __func4011b0__) (args ((Ref 17)))))
       (23 (ReturnedOp (var esp) (typ Int))) (24 Nop) (25 Nop)
       (26 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (27 (LoadOp (var edi) (op Load32) (addr (Ref 26)) (offset -36))) (28 Nop)
       (29 (Const __i32 588)) (30 (Const __i32 0))
       (31 (Memset (count (Ref 29)) (value (Ref 30)) (dest (Ref 27))))
       (32 (Const ecx 0))
       (33 (LoadOp (var eax) (op Load32) (addr (Ref 26)) (offset -36))) (34 Nop)
       (35 (Const __i32 4294967295))
       (36 (StoreOp (op Store16) (addr (Ref 33)) (value (Ref 35)) (offset 468)))
       (37 (LoadOp (var eax) (op Load32) (addr (Ref 26)) (offset -36)))
       (38 (LoadOp (var edi) (op Load32) (addr (Ref 23)))) (39 Nop) (40 Nop)
       (41 Nop) (42 (LoadOp (var ebp) (op Load32) (addr (Ref 26))))
       (43 (Const __i32 4))
       (44 (BiOp (var esp) (op Add) (lhs (Ref 26)) (rhs (Ref 43))))
       (45 (LoadOp (var __i32) (op Load32) (addr (Ref 44))))
       (46 (BiOp (var __i32) (op Equal) (lhs (Ref 45)) (rhs (Ref 1))))
       (47 (AssertOp (condition (Ref 46)))) (48 (Const __i32 4))
       (49 (BiOp (var esp) (op Add) (lhs (Ref 44)) (rhs (Ref 48))))
       (50 (SetGlobalOp (value (Ref 32)) (global ((name ecx) (typ Int)))))
       (51 (SetGlobalOp (value (Ref 37)) (global ((name eax) (typ Int)))))
       (52 (SetGlobalOp (value (Ref 38)) (global ((name edi) (typ Int)))))
       (53 (SetGlobalOp (value (Ref 42)) (global ((name ebp) (typ Int)))))))
     (terminator Return) (roots ((Ref 1) (Ref 49))))
    |}]

let%expect_test "fild/fiadd" =
  test_trans_block 0x00453df6;
  [%expect
    {|
    ((id 510)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset 8)))
       (2
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 1)) (signed false)
         (offset 559)))
       (3 Nop) (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 4)) (signed false)
         (offset 555)))
       (6 Nop) (7 (BiOp (var eax) (op Subtract) (lhs (Ref 2)) (rhs (Ref 5))))
       (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -792)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -792)))
       (10 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 9))))
       (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -16)))
       (12 (BiOp (var __fl) (op FloatMult) (lhs (Ref 10)) (rhs (Ref 11))))
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (14
        (SignedLoadOp (var __i32) (op Load8) (addr (Ref 13)) (signed false)
         (offset 555)))
       (15 Nop)
       (16 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 14)) (offset -796)))
       (17 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -796)))
       (18 (UniOp (var __fl) (op Int32ToFloatSigned) (operand (Ref 17))))
       (19 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 12)) (rhs (Ref 18))))
       (20 (Const __i32 4)) (21 (OutsideContext (var esp) (typ Int)))
       (22 (BiOp (var esp) (op Subtract) (lhs (Ref 21)) (rhs (Ref 20))))
       (23 (Const __i32 4537877))
       (24 (StoreOp (op Store32) (addr (Ref 22)) (value (Ref 23))))
       (25
        (GetGlobalOp (var __fpuStack__) (global ((name __fpuStack__) (typ Int)))))
       (26
        (StoreOp (op FloatStore64) (addr (Ref 25)) (value (Ref 19)) (offset 8)))
       (27 (Const __i32 8))
       (28 (BiOp (var __fpuStack__) (op Add) (lhs (Ref 25)) (rhs (Ref 27))))
       (29 (SetGlobalOp (value (Ref 5)) (global ((name edx) (typ Int)))))
       (30 (SetGlobalOp (value (Ref 14)) (global ((name ecx) (typ Int)))))
       (31 (SetGlobalOp (value (Ref 13)) (global ((name eax) (typ Int)))))
       (32
        (SetGlobalOp (value (Ref 28)) (global ((name __fpuStack__) (typ Int)))))
       (33 (CallOp (func __func48b8a0__) (args ((Ref 22)))))
       (34 (ReturnedOp (var esp) (typ Int)))
       (35 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (36 (LoadOp (var edx) (op Load32) (addr (Ref 35)) (offset 8)))
       (37 (GetGlobalOp (var eax) (global ((name eax) (typ Int))))) (38 Nop)
       (39 (StoreOp (op Store8) (addr (Ref 36)) (value (Ref 37)) (offset 443)))
       (40 (SetGlobalOp (value (Ref 36)) (global ((name edx) (typ Int)))))))
     (terminator (Goto (Block 513))) (roots ((Ref 34))))
    |}]

let%expect_test "div" =
  test_trans_block 0x00452f62;
  [%expect
    {|
    ((id 341)
     (instrs
      ((0 (Const ecx 4849184)) (1 (Const __i32 4))
       (2 (OutsideContext (var esp) (typ Int)))
       (3 (BiOp (var esp) (op Subtract) (lhs (Ref 2)) (rhs (Ref 1))))
       (4 (Const __i32 4534107))
       (5 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 4))))
       (6 (SetGlobalOp (value (Ref 0)) (global ((name ecx) (typ Int)))))
       (7 (CallOp (func __func4318d0__) (args ((Ref 3)))))
       (8 (ReturnedOp (var esp) (typ Int))) (9 Nop)
       (10 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (11 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (12 (LoadOp (var __i32) (op Load32) (addr (Ref 11)) (offset -248)))
       (13
        (SignedBiOp (var eax) (op Divide) (signed false) (lhs (Ref 10))
         (rhs (Ref 12))))
       (14
        (SignedBiOp (var edx) (op Remainder) (signed false) (lhs (Ref 10))
         (rhs (Ref 12))))
       (15 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 14)) (offset -608)))
       (16 (SetGlobalOp (value (Ref 14)) (global ((name edx) (typ Int)))))
       (17 (SetGlobalOp (value (Ref 13)) (global ((name eax) (typ Int)))))))
     (terminator (Goto (Block 343))) (roots ((Ref 8))))
    |}]

let%expect_test "fdiv" =
  test_trans_block 0x00452949;
  [%expect
    {|
    ((id 279)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -500)))
       (2 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 0)) (offset -504)))
       (3 (BiOp (var __fl) (op FloatDiv) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -508)))
       (5 (Const __i32 0)) (6 (OutsideContext (var esp) (typ Int)))
       (7 (Const __i32 4))
       (8 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 7))))
       (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 5))))
       (10 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (11
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 10)) (signed true)
         (offset 6)))
       (12 Nop) (13 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (14 (BiOp (var eax) (op MergeTrunc16) (lhs (Ref 11)) (rhs (Ref 13))))
       (15 (Const __i32 4))
       (16 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 15))))
       (17 (StoreOp (op Store32) (addr (Ref 16)) (value (Ref 14))))
       (18 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (19 (Const __i32 8))
       (20 (BiOp (var ecx) (op Add) (lhs (Ref 18)) (rhs (Ref 19))))
       (21 (Const __i32 4))
       (22 (BiOp (var esp) (op Subtract) (lhs (Ref 16)) (rhs (Ref 21))))
       (23 (StoreOp (op Store32) (addr (Ref 22)) (value (Ref 20))))
       (24 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (25 (Const __i32 4))
       (26 (BiOp (var esp) (op Subtract) (lhs (Ref 22)) (rhs (Ref 25))))
       (27 (Const __i32 4532585))
       (28 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 27)))) (29 Nop)
       (30 (SetGlobalOp (value (Ref 10)) (global ((name edx) (typ Int)))))
       (31 (SetGlobalOp (value (Ref 24)) (global ((name ecx) (typ Int)))))
       (32 (SetGlobalOp (value (Ref 14)) (global ((name eax) (typ Int)))))
       (33 (CallOp (func __func450c10__) (args ((Ref 26)))))
       (34 (ReturnedOp (var esp) (typ Int)))
       (35 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (36 (LoadOp (var edx) (op Load32) (addr (Ref 35)) (offset -508)))
       (37 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (38 (StoreOp (op Store32) (addr (Ref 37)) (value (Ref 36))))
       (39 (SetGlobalOp (value (Ref 36)) (global ((name edx) (typ Int)))))))
     (terminator (Goto (Block 481))) (roots ((Ref 34))))
    |}]

let%expect_test "cdq/idiv" =
  test_trans_block 0x004529e3;
  [%expect
    {|
    ((id 286)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -512))) (2 Nop)
       (3 Nop) (4 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -516)))
       (5 Nop)
       (6
        (SignedBiOp (var edx) (op Remainder) (signed true) (lhs (Ref 1))
         (rhs (Ref 4))))
       (7 Nop) (8 (Const __i32 0)) (9 (OutsideContext (var esp) (typ Int)))
       (10 (Const __i32 4))
       (11 (BiOp (var esp) (op Subtract) (lhs (Ref 9)) (rhs (Ref 10))))
       (12 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 8))))
       (13 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (14
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 13)) (signed true)
         (offset 6)))
       (15 Nop) (16 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
       (17 (BiOp (var ecx) (op MergeTrunc16) (lhs (Ref 14)) (rhs (Ref 16))))
       (18 (Const __i32 4))
       (19 (BiOp (var esp) (op Subtract) (lhs (Ref 11)) (rhs (Ref 18))))
       (20 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 17))))
       (21 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -4)))
       (22 (Const __i32 8))
       (23 (BiOp (var edx) (op Add) (lhs (Ref 21)) (rhs (Ref 22))))
       (24 (Const __i32 4))
       (25 (BiOp (var esp) (op Subtract) (lhs (Ref 19)) (rhs (Ref 24))))
       (26 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 23))))
       (27 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (28 (Const __i32 4))
       (29 (BiOp (var esp) (op Subtract) (lhs (Ref 25)) (rhs (Ref 28))))
       (30 (Const __i32 4532736))
       (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
       (32 (SetGlobalOp (value (Ref 23)) (global ((name edx) (typ Int)))))
       (33 (SetGlobalOp (value (Ref 27)) (global ((name ecx) (typ Int)))))
       (34 (SetGlobalOp (value (Ref 6)) (global ((name esi) (typ Int)))))
       (35 (SetGlobalOp (value (Ref 13)) (global ((name eax) (typ Int)))))
       (36 (CallOp (func __func450ca0__) (args ((Ref 29)))))
       (37 (ReturnedOp (var esp) (typ Int)))
       (38 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
       (39 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
       (40 (StoreOp (op Store32) (addr (Ref 39)) (value (Ref 38))))))
     (terminator (Goto (Block 481))) (roots ((Ref 37))))
    |}]

let%expect_test "imul" =
  test_trans_block 0x004539dd;
  [%expect
    {|
    ((id 491)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -12)))
       (2 (Const __i32 12))
       (3 (BiOp (var eax) (op Multiply) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5 (Const edx 0))
       (6 (BiOp (var __i32) (op Add) (lhs (Ref 3)) (rhs (Ref 4))))
       (7 (LoadOp (var __i32) (op Load32) (addr (Ref 6)) (offset 140)))
       (8 (Const __i32 0))
       (9
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 7))
         (rhs (Ref 8))))
       (10 (BiOp (var edx) (op MergeTruncLow8) (lhs (Ref 9)) (rhs (Ref 5))))
       (11 (UniOp (var __i32) (op EqualsZero) (operand (Ref 10))))
       (12 (SetGlobalOp (value (Ref 10)) (global ((name edx) (typ Int)))))
       (13 (SetGlobalOp (value (Ref 4)) (global ((name ecx) (typ Int)))))
       (14 (SetGlobalOp (value (Ref 3)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 513)) (fail (Block 492)) (condition (Ref 11))))
     (roots ((Ref 11))))
    |}]

let%expect_test "float jp 0x5" =
  test_trans_block 0x00451bfe;
  [%expect
    {|
    ((id 148)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 1)) (offset 40)))
       (3 (Const __i32 4819532))
       (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 3))))
       (5 (Landmine (var eax) (typ Int))) (6 Nop)
       (7 (BiOp (var __i32) (op FloatLessThan) (lhs (Ref 2)) (rhs (Ref 4))))
       (8 (UniOp (var __i32) (op EqualsZero) (operand (Ref 7)))) (9 Nop)
       (10 (SetGlobalOp (value (Ref 1)) (global ((name ecx) (typ Int)))))
       (11 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 150)) (fail (Block 149)) (condition (Ref 8))))
     (roots ((Ref 8))))
    |}]

let%expect_test "jle" =
  test_trans_block 0x004536f0;
  [%expect
    {|
    ((id 450)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -728)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -732)))
       (3
        (SignedBiOp (var __i32) (op LessThanEqual) (signed true) (lhs (Ref 1))
         (rhs (Ref 2))))
       (4 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 452)) (fail (Block 451)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test "jumptable" =
  test_trans_block 0x00450dec;
  [%expect
    {|
    ((id 6)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -312)))
       (2 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
     (terminator
      (Switch
       (cases
        ((Block 7) (Block 481) (Block 7) (Block 8) (Block 9) (Block 22)
         (Block 23) (Block 74) (Block 13) (Block 20) (Block 21) (Block 29)
         (Block 31) (Block 32) (Block 42) (Block 52) (Block 69) (Block 73)
         (Block 98) (Block 97) (Block 96) (Block 125) (Block 481) (Block 141)
         (Block 124) (Block 30) (Block 142) (Block 143) (Block 151) (Block 140)
         (Block 59) (Block 167) (Block 168) (Block 169) (Block 185) (Block 189)
         (Block 193) (Block 206) (Block 216) (Block 220) (Block 294) (Block 298)
         (Block 302) (Block 306) (Block 310) (Block 314) (Block 318) (Block 322)
         (Block 326) (Block 330) (Block 224) (Block 231) (Block 238) (Block 245)
         (Block 252) (Block 259) (Block 266) (Block 273) (Block 280) (Block 287)
         (Block 337) (Block 344) (Block 348) (Block 352) (Block 356) (Block 360)
         (Block 364) (Block 368) (Block 372) (Block 381) (Block 390) (Block 399)
         (Block 408) (Block 417) (Block 426) (Block 435) (Block 444) (Block 453)
         (Block 462) (Block 471) (Block 115) (Block 159) (Block 163)))
       (default (Block 528)) (switch_on (Ref 1))))
     (roots ((Ref 1))))
    |}]

let%expect_test "ja" =
  test_trans_block 0x00450de0;
  [%expect
    {|
    ((id 5)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (SignedLoadOp (var __i32) (op Load16) (addr (Ref 1)) (signed true)))
       (3 Nop)
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 2)) (offset -312)))
       (5 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset -312)))
       (6 (Const __i32 1))
       (7 (BiOp (var edx) (op Add) (lhs (Ref 5)) (rhs (Ref 6))))
       (8 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 7)) (offset -312)))
       (9 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -312)))
       (10 (Const __i32 82))
       (11
        (SignedBiOp (var __i32) (op GreaterThan) (signed false) (lhs (Ref 9))
         (rhs (Ref 10))))
       (12 (SetGlobalOp (value (Ref 7)) (global ((name edx) (typ Int)))))
       (13 (SetGlobalOp (value (Ref 2)) (global ((name ecx) (typ Int)))))
       (14 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 481)) (fail (Block 6)) (condition (Ref 11))))
     (roots ((Ref 11))))
    |}]

let%expect_test "jg" =
  test_trans_block 4525496;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
       (2 (LoadOp (var ecx) (op Load32) (addr (Ref 1)) (offset 480)))
       (3 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 2)) (offset -4)))
       (4 (LoadOp (var edx) (op Load32) (addr (Ref 0)) (offset 8)))
       (5 (LoadOp (var eax) (op Load32) (addr (Ref 4)) (offset 56)))
       (6 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 5)) (offset -56)))
       (7 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
       (8
        (SignedLoadOp (var __i32) (op Load16) (addr (Ref 7)) (signed true)
         (offset 4)))
       (9 Nop) (10 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -56)))
       (11
        (SignedBiOp (var __i32) (op GreaterThan) (signed true) (lhs (Ref 8))
         (rhs (Ref 10))))
       (12 (SetGlobalOp (value (Ref 8)) (global ((name edx) (typ Int)))))
       (13 (SetGlobalOp (value (Ref 7)) (global ((name ecx) (typ Int)))))
       (14 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 482)) (fail (Block 5)) (condition (Ref 11))))
     (roots ((Ref 11))))
    |}]

let%expect_test "jge" =
  test_trans_block 4380548;
  [%expect
    {|
    ((id 2)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -4)))
       (2 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -16)))
       (3
        (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true) (lhs (Ref 1))
         (rhs (Ref 2))))
       (4 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 4)) (fail (Block 3)) (condition (Ref 3))))
     (roots ((Ref 3))))
    |}]

let%expect_test _ =
  test_trans_block 0x0040127a;
  [%expect
    {|
    ((id 4)
     (instrs
      ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
       (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset -24)))
       (2 (Const __i32 1))
       (3 (BiOp (var eax) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
       (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -24)))
       (5 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -24)))
       (6 (Const __i32 0))
       (7
        (SignedBiOp (var __i32) (op LessThan) (signed true) (lhs (Ref 5))
         (rhs (Ref 6))))
       (8 (SetGlobalOp (value (Ref 3)) (global ((name eax) (typ Int)))))))
     (terminator
      (Branch (succeed (Block 6)) (fail (Block 5)) (condition (Ref 7))))
     (roots ((Ref 7))))
    |}]

let%expect_test _ =
  test_trans 0x47ea7d;
  [%expect {|
    ((name func_47ea7d)
     (signature
      ((args (((name esp) (typ Int)))) (returns (((name esp) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (Const __i32 96)) (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2))))
          (6 (Const __i32 4785824)) (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 6))))
          (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 10))))
          (12 (Const __i32 4713092))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (CallOp (func __func480624__) (args ((Ref 11)))))
          (15 (ReturnedOp (var esp) (typ Int))) (16 (Const edi 148)) (17 Nop)
          (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 18))))
          (20 (Const __i32 4713104))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22 (SetGlobalOp (value (Ref 16)) (global ((name eax) (typ Int)))))
          (23 (SetGlobalOp (value (Ref 16)) (global ((name edi) (typ Int)))))
          (24 (CallOp (func __func47f3a0__) (args ((Ref 19)))))
          (25 (ReturnedOp (var esp) (typ Int)))
          (26 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (27
           (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 25)) (offset -24)))
          (28 Nop) (29 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
          (30 (StoreOp (op Store32) (addr (Ref 25)) (value (Ref 29))))
          (31 (Const __i32 4))
          (32 (BiOp (var esp) (op Subtract) (lhs (Ref 25)) (rhs (Ref 31))))
          (33 (StoreOp (op Store32) (addr (Ref 32)) (value (Ref 25))))
          (34 (Const __i32 4))
          (35 (BiOp (var esp) (op Subtract) (lhs (Ref 32)) (rhs (Ref 34))))
          (36 (Const __i32 4713117))
          (37 (StoreOp (op Store32) (addr (Ref 35)) (value (Ref 36))))
          (38 (SetGlobalOp (value (Ref 25)) (global ((name esi) (typ Int)))))
          (39 (CallOp (func KERNEL32.dll_GetVersionExA) (args ((Ref 35)))))
          (40 (ReturnedOp (var esp) (typ Int)))
          (41 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (42 (LoadOp (var ecx) (op Load32) (addr (Ref 41)) (offset 16)))
          (43 (Const __i32 4847496))
          (44 (StoreOp (op Store32) (addr (Ref 43)) (value (Ref 42))))
          (45 (LoadOp (var eax) (op Load32) (addr (Ref 41)) (offset 4)))
          (46 (Const __i32 4847508))
          (47 (StoreOp (op Store32) (addr (Ref 46)) (value (Ref 45))))
          (48 (LoadOp (var edx) (op Load32) (addr (Ref 41)) (offset 8)))
          (49 (Const __i32 4847512))
          (50 (StoreOp (op Store32) (addr (Ref 49)) (value (Ref 48))))
          (51 (LoadOp (var esi) (op Load32) (addr (Ref 41)) (offset 12)))
          (52 (Const __i32 32767))
          (53 (BiOp (var esi) (op And) (lhs (Ref 51)) (rhs (Ref 52))))
          (54 (Const __i32 4847500))
          (55 (StoreOp (op Store32) (addr (Ref 54)) (value (Ref 53))))
          (56 (Const __i32 2))
          (57 (BiOp (var __i32) (op Equal) (lhs (Ref 42)) (rhs (Ref 56))))
          (58 (SetGlobalOp (value (Ref 48)) (global ((name edx) (typ Int)))))
          (59 (SetGlobalOp (value (Ref 42)) (global ((name ecx) (typ Int)))))
          (60 (SetGlobalOp (value (Ref 53)) (global ((name esi) (typ Int)))))
          (61 (SetGlobalOp (value (Ref 45)) (global ((name eax) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 2)) (fail (Block 1)) (condition (Ref 57))))
        (roots ((Ref 1) (Ref 40) (Ref 57))))
       ((id 1)
        (instrs
         ((0 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (1 (Const __i32 32768))
          (2 (BiOp (var esi) (op Or) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (Const __i32 4847500))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2))))
          (5 (SetGlobalOp (value (Ref 2)) (global ((name esi) (typ Int)))))))
        (terminator (Goto (Block 2))) (roots ()))
       ((id 2)
        (instrs
         ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (1 (Const __i32 8))
          (2 (BiOp (var eax) (op ShiftLeft) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (GetGlobalOp (var edx) (global ((name edx) (typ Int)))))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (Const __i32 4847504))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))
          (7 (Const esi 0)) (8 (OutsideContext (var esp) (typ Int)))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 9))))
          (11 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 7))))
          (12 (Const __i32 4772232))
          (13 (LoadOp (var edi) (op Load32) (addr (Ref 12))))
          (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Subtract) (lhs (Ref 10)) (rhs (Ref 14))))
          (16 (Const __i32 4713200))
          (17 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 16))))
          (18 (SetGlobalOp (value (Ref 7)) (global ((name esi) (typ Int)))))
          (19 (SetGlobalOp (value (Ref 4)) (global ((name eax) (typ Int)))))
          (20 (SetGlobalOp (value (Ref 13)) (global ((name edi) (typ Int)))))
          (21 (CallIndirectOp (table_index (Ref 13)) (args ((Ref 15)))))
          (22 (ReturnedOp (var esp) (typ Int)))
          (23 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (24
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 23)) (signed true)))
          (25 Nop) (26 Nop) (27 (Const __i32 23117))
          (28 (BiOp (var __i32) (op NotEqual) (lhs (Ref 24)) (rhs (Ref 27))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 3)) (condition (Ref 28))))
        (roots ((Ref 22) (Ref 28))))
       ((id 3)
        (instrs
         ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (1 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 60)))
          (2 (BiOp (var ecx) (op Add) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (LoadOp (var __i32) (op Load32) (addr (Ref 2))))
          (4 (Const __i32 17744))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (SetGlobalOp (value (Ref 2)) (global ((name ecx) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 4)) (condition (Ref 5))))
        (roots ((Ref 5))))
       ((id 4)
        (instrs
         ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset 24)))
          (2 Nop) (3 (Const __i32 267))
          (4 (BiOp (var __i32) (op Equal) (lhs (Ref 1)) (rhs (Ref 3))))
          (5 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 9)) (fail (Block 5)) (condition (Ref 4))))
        (roots ((Ref 4))))
       ((id 5)
        (instrs
         ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (1 (Const __i32 523))
          (2 (BiOp (var __i32) (op Equal) (lhs (Ref 0)) (rhs (Ref 1))))))
        (terminator
         (Branch (succeed (Block 7)) (fail (Block 6)) (condition (Ref 2))))
        (roots ((Ref 2))))
       ((id 6)
        (instrs
         ((0 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -28)))))
        (terminator (Goto (Block 12))) (roots ()))
       ((id 7)
        (instrs
         ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 132)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 8)) (condition (Ref 3))))
        (roots ((Ref 3))))
       ((id 8)
        (instrs
         ((0 (Const eax 0))
          (1 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 248)))
          (3 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (4 (SetGlobalOp (value (Ref 0)) (global ((name eax) (typ Int)))))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 2)) (rhs (Ref 3))))
          (6 (DupVar (var __input_compare_arg__) (src (Ref 5)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 6))))
       ((id 9)
        (instrs
         ((0 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset 116)))
          (2 (Const __i32 14))
          (3
           (SignedBiOp (var __i32) (op LessThanEqual) (signed false)
            (lhs (Ref 1)) (rhs (Ref 2))))))
        (terminator
         (Branch (succeed (Block 6)) (fail (Block 10)) (condition (Ref 3))))
        (roots ((Ref 3))))
       ((id 10)
        (instrs
         ((0 (Const eax 0))
          (1 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (2 (LoadOp (var __i32) (op Load32) (addr (Ref 1)) (offset 232)))
          (3 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (4 (SetGlobalOp (value (Ref 0)) (global ((name eax) (typ Int)))))
          (5 (BiOp (var __i32) (op NotEqual) (lhs (Ref 2)) (rhs (Ref 3))))
          (6 (DupVar (var __input_compare_arg__) (src (Ref 5)) (typ Int)))))
        (terminator (Goto (Block 11))) (roots ((Ref 6))))
       ((id 11)
        (instrs
         ((0 (OutsideContext (var __input_compare_arg__) (typ Int)))
          (1 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (2 (BiOp (var eax) (op MergeTruncLow8) (lhs (Ref 0)) (rhs (Ref 1))))
          (3 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 2)) (offset -28)))
          (5 (SetGlobalOp (value (Ref 2)) (global ((name eax) (typ Int)))))))
        (terminator (Goto (Block 12))) (roots ((Ref 0))))
       ((id 12)
        (instrs
         ((0 (Const __i32 1)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713286))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func48068a__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (15 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 14)) (fail (Block 13)) (condition (Ref 14))))
        (roots ((Ref 13) (Ref 14))))
       ((id 13)
        (instrs
         ((0 (Const __i32 28)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713298))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea59__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 14))) (roots ((Ref 13))))
       ((id 14)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713304))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func __func481649__) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 16)) (fail (Block 15)) (condition (Ref 7))))
        (roots ((Ref 6) (Ref 7))))
       ((id 15)
        (instrs
         ((0 (Const __i32 16)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713315))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea59__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 16))) (roots ((Ref 13))))
       ((id 16)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713321))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func __func483560__) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (8 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 7)) (offset -4)))
          (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 10))))
          (12 (Const __i32 4713329))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (CallOp (func __func483362__) (args ((Ref 11)))))
          (15 (ReturnedOp (var esp) (typ Int)))
          (16 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (17 (Const __i32 0))
          (18
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 16)) (rhs (Ref 17))))))
        (terminator
         (Branch (succeed (Block 18)) (fail (Block 17)) (condition (Ref 18))))
        (roots ((Ref 15) (Ref 18))))
       ((id 17)
        (instrs
         ((0 (Const __i32 27)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713340))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea34__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 18))) (roots ((Ref 13))))
       ((id 18)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713346))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func KERNEL32.dll_GetCommandLineA) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (8 (Const __i32 20337236))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 7))))
          (10 (Const __i32 4))
          (11 (BiOp (var esp) (op Subtract) (lhs (Ref 6)) (rhs (Ref 10))))
          (12 (Const __i32 4713357))
          (13 (StoreOp (op Store32) (addr (Ref 11)) (value (Ref 12))))
          (14 (CallOp (func __func483240__) (args ((Ref 11)))))
          (15 (ReturnedOp (var esp) (typ Int)))
          (16 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (17 (Const __i32 4847472))
          (18 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 16))))
          (19 (Const __i32 4))
          (20 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 19))))
          (21 (Const __i32 4713367))
          (22 (StoreOp (op Store32) (addr (Ref 20)) (value (Ref 21))))
          (23 (CallOp (func __func48319e__) (args ((Ref 20)))))
          (24 (ReturnedOp (var esp) (typ Int)))
          (25 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (26 (Const __i32 0))
          (27
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 25)) (rhs (Ref 26))))))
        (terminator
         (Branch (succeed (Block 20)) (fail (Block 19)) (condition (Ref 27))))
        (roots ((Ref 24) (Ref 27))))
       ((id 19)
        (instrs
         ((0 (Const __i32 8)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713378))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea34__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 20))) (roots ((Ref 13))))
       ((id 20)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713384))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func __func482f6b__) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (8 (Const __i32 0))
          (9
           (SignedBiOp (var __i32) (op GreaterThanEqual) (signed true)
            (lhs (Ref 7)) (rhs (Ref 8))))))
        (terminator
         (Branch (succeed (Block 22)) (fail (Block 21)) (condition (Ref 9))))
        (roots ((Ref 6) (Ref 9))))
       ((id 21)
        (instrs
         ((0 (Const __i32 9)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713395))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea34__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 22))) (roots ((Ref 13))))
       ((id 22)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713401))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func __func47f891__) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))
          (7 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (8 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 7)) (offset -32)))
          (10 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (11 (BiOp (var __i32) (op Equal) (lhs (Ref 7)) (rhs (Ref 10))))))
        (terminator
         (Branch (succeed (Block 24)) (fail (Block 23)) (condition (Ref 11))))
        (roots ((Ref 6) (Ref 11))))
       ((id 23)
        (instrs
         ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713414))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47ea34__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))
          (11 (LoadOp (var ecx) (op Load32) (addr (Ref 10))))
          (12 (Const __i32 4))
          (13 (BiOp (var esp) (op Add) (lhs (Ref 10)) (rhs (Ref 12))))
          (14 (SetGlobalOp (value (Ref 11)) (global ((name ecx) (typ Int)))))))
        (terminator (Goto (Block 24))) (roots ((Ref 13))))
       ((id 24)
        (instrs
         ((0 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (1 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (2 (StoreOp (op Store32) (addr (Ref 1)) (value (Ref 0)) (offset -56)))
          (3 (Const __i32 -100))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 1)) (rhs (Ref 3))))
          (5 (OutsideContext (var esp) (typ Int))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Subtract) (lhs (Ref 5)) (rhs (Ref 6))))
          (8 (StoreOp (op Store32) (addr (Ref 7)) (value (Ref 4))))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 7)) (rhs (Ref 9))))
          (11 (Const __i32 4713427))
          (12 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 11))))
          (13 (SetGlobalOp (value (Ref 4)) (global ((name eax) (typ Int)))))
          (14 (CallOp (func KERNEL32.dll_GetStartupInfoA) (args ((Ref 10)))))
          (15 (ReturnedOp (var esp) (typ Int))) (16 (Const __i32 4))
          (17 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 16))))
          (18 (Const __i32 4713433))
          (19 (StoreOp (op Store32) (addr (Ref 17)) (value (Ref 18))))
          (20 (CallOp (func __func482f02__) (args ((Ref 17)))))
          (21 (ReturnedOp (var esp) (typ Int)))
          (22 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (23 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (24
           (StoreOp (op Store32) (addr (Ref 23)) (value (Ref 22)) (offset -104)))
          (25
           (SignedLoadOp (var __i32) (op Load8) (addr (Ref 23)) (signed false)
            (offset -56)))
          (26 Nop) (27 Nop) (28 (Const __i32 1))
          (29 (BiOp (var __i32) (op And) (lhs (Ref 25)) (rhs (Ref 28))))
          (30 (UniOp (var __i32) (op EqualsZero) (operand (Ref 29))))))
        (terminator
         (Branch (succeed (Block 26)) (fail (Block 25)) (condition (Ref 30))))
        (roots ((Ref 21) (Ref 30))))
       ((id 25)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (1
           (SignedLoadOp (var __i32) (op Load16) (addr (Ref 0)) (signed false)
            (offset -52)))
          (2 Nop)
          (3 (SetGlobalOp (value (Ref 1)) (global ((name eax) (typ Int)))))))
        (terminator (Goto (Block 27))) (roots ()))
       ((id 26)
        (instrs
         ((0 (Const __i32 10)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (LoadOp (var eax) (op Load32) (addr (Ref 3)))) (6 (Const __i32 4))
          (7 (BiOp (var esp) (op Add) (lhs (Ref 3)) (rhs (Ref 6))))
          (8 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))))
        (terminator (Goto (Block 27))) (roots ((Ref 7))))
       ((id 27)
        (instrs
         ((0 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (6 (LoadOp (var __i32) (op Load32) (addr (Ref 5)) (offset -104)))
          (7 (Const __i32 4))
          (8 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 7))))
          (9 (StoreOp (op Store32) (addr (Ref 8)) (value (Ref 6))))
          (10 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (11 (Const __i32 4))
          (12 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 11))))
          (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 10))))
          (14 (Const __i32 4))
          (15 (BiOp (var esp) (op Subtract) (lhs (Ref 12)) (rhs (Ref 14))))
          (16 (StoreOp (op Store32) (addr (Ref 15)) (value (Ref 10))))
          (17 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
          (18 (Const __i32 4))
          (19 (BiOp (var esp) (op Subtract) (lhs (Ref 15)) (rhs (Ref 18))))
          (20 (Const __i32 4713462))
          (21 (StoreOp (op Store32) (addr (Ref 19)) (value (Ref 20))))
          (22 (CallIndirectOp (table_index (Ref 17)) (args ((Ref 19)))))
          (23 (ReturnedOp (var esp) (typ Int)))
          (24 (GetGlobalOp (var eax) (global ((name eax) (typ Int)))))
          (25 (Const __i32 4))
          (26 (BiOp (var esp) (op Subtract) (lhs (Ref 23)) (rhs (Ref 25))))
          (27 (StoreOp (op Store32) (addr (Ref 26)) (value (Ref 24))))
          (28 (Const __i32 4))
          (29 (BiOp (var esp) (op Subtract) (lhs (Ref 26)) (rhs (Ref 28))))
          (30 (Const __i32 4713465))
          (31 (StoreOp (op Store32) (addr (Ref 29)) (value (Ref 30))))
          (32 (CallOp (func __func434020__) (args ((Ref 29)))))
          (33 (ReturnedOp (var esp) (typ Int)))
          (34 (GetGlobalOp (var eax) (global ((name eax) (typ Int))))) (35 Nop)
          (36 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (37
           (StoreOp (op Store32) (addr (Ref 36)) (value (Ref 34)) (offset -108)))
          (38 (LoadOp (var __i32) (op Load32) (addr (Ref 36)) (offset -28)))
          (39 (GetGlobalOp (var esi) (global ((name esi) (typ Int)))))
          (40 (BiOp (var __i32) (op NotEqual) (lhs (Ref 38)) (rhs (Ref 39))))
          (41 (SetGlobalOp (value (Ref 34)) (global ((name edi) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 29)) (fail (Block 28)) (condition (Ref 40))))
        (roots ((Ref 33) (Ref 40))))
       ((id 28)
        (instrs
         ((0 (GetGlobalOp (var edi) (global ((name edi) (typ Int)))))
          (1 (OutsideContext (var esp) (typ Int))) (2 (Const __i32 4))
          (3 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 3)) (value (Ref 0))))
          (5 (Const __i32 4))
          (6 (BiOp (var esp) (op Subtract) (lhs (Ref 3)) (rhs (Ref 5))))
          (7 (Const __i32 4713481))
          (8 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 7))))
          (9 (CallOp (func __func47f9c9__) (args ((Ref 6)))))
          (10 (ReturnedOp (var esp) (typ Int)))))
        (terminator (Goto (Block 29))) (roots ((Ref 10))))
       ((id 29)
        (instrs
         ((0 (Const __i32 4)) (1 (OutsideContext (var esp) (typ Int)))
          (2 (BiOp (var esp) (op Subtract) (lhs (Ref 1)) (rhs (Ref 0))))
          (3 (Const __i32 4713486))
          (4 (StoreOp (op Store32) (addr (Ref 2)) (value (Ref 3))))
          (5 (CallOp (func __func47f9eb__) (args ((Ref 2)))))
          (6 (ReturnedOp (var esp) (typ Int)))))
        (terminator (Goto (Block 30))) (roots ((Ref 6))))
       ((id 30)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (1 (LoadOp (var __i32) (op Load32) (addr (Ref 0)) (offset -4)))
          (2 (Const __i32 4294967295))
          (3 (BiOp (var __i32) (op Or) (lhs (Ref 1)) (rhs (Ref 2))))
          (4 (StoreOp (op Store32) (addr (Ref 0)) (value (Ref 3)) (offset -4)))
          (5 (GetGlobalOp (var edi) (global ((name edi) (typ Int))))) (6 Nop)
          (7 (Const __i32 -124))
          (8 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 7))))
          (9 (Const __i32 4))
          (10 (BiOp (var esp) (op Subtract) (lhs (Ref 8)) (rhs (Ref 9))))
          (11 (Const __i32 4713545))
          (12 (StoreOp (op Store32) (addr (Ref 10)) (value (Ref 11))))
          (13 (SetGlobalOp (value (Ref 5)) (global ((name eax) (typ Int)))))
          (14 (CallOp (func __func48065f__) (args ((Ref 10)))))
          (15 (ReturnedOp (var esp) (typ Int)))
          (16 (LoadOp (var __i32) (op Load32) (addr (Ref 15))))
          (17 (OutsideContext (var __ret_addr__) (typ Int)))
          (18 (BiOp (var __i32) (op Equal) (lhs (Ref 16)) (rhs (Ref 17))))
          (19 (AssertOp (condition (Ref 18)))) (20 (Const __i32 4))
          (21 (BiOp (var esp) (op Add) (lhs (Ref 15)) (rhs (Ref 20))))))
        (terminator Return) (roots ((Ref 17) (Ref 21))))))
     (locals
      ((__input_compare_arg__ ((name __input_compare_arg__) (typ Int)))
       (__ret_addr__ ((name __ret_addr__) (typ Int)))
       (esp ((name esp) (typ Int))))))
    (parent
     ((0 -1) (1 0) (2 1) (3 2) (4 3) (5 4) (6 5) (7 5) (8 26) (9 4) (10 29)
      (11 27) (12 6) (13 7) (14 8) (15 9) (16 10) (17 11) (18 12) (19 13)
      (20 14) (21 15) (22 16) (23 17) (24 18) (25 19) (26 19) (27 20) (28 21)
      (29 22) (30 23)))
    (preorder
     ((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 12) (8 13) (9 14) (10 15)
      (11 16) (12 17) (13 18) (14 19) (15 20) (16 21) (17 22) (18 23) (19 24)
      (20 25) (21 27) (22 28) (23 29) (24 30) (25 26) (26 7) (27 8) (28 11)
      (29 9) (30 10)))
    (inv_preorder
     ((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 26) (8 27) (9 29) (10 30)
      (11 28) (12 7) (13 8) (14 9) (15 10) (16 11) (17 12) (18 13) (19 14)
      (20 15) (21 16) (22 17) (23 18) (24 19) (25 20) (26 25) (27 21) (28 22)
      (29 23) (30 24)))
    (semidom
     ((0 -1) (1 0) (2 0) (3 2) (4 3) (5 4) (6 2) (7 5) (8 26) (9 4) (10 29)
      (11 4) (12 4) (13 7) (14 7) (15 9) (16 9) (17 11) (18 11) (19 13) (20 13)
      (21 15) (22 15) (23 17) (24 17) (25 19) (26 19) (27 19) (28 21) (29 21)
      (30 23)))
    (ancestor
     ((0 -1) (1 0) (2 1) (3 2) (4 3) (5 4) (6 5) (7 3) (8 4) (9 2) (10 3)
      (11 4) (12 6) (13 7) (14 8) (15 9) (16 10) (17 11) (18 12) (19 13)
      (20 14) (21 15) (22 16) (23 17) (24 18) (25 19) (26 18) (27 20) (28 21)
      (29 22) (30 23)))
    (label
     ((0 0) (1 0) (2 0) (3 2) (4 3) (5 4) (6 2) (7 4) (8 7) (9 7) (10 9)
      (11 9) (12 11) (13 11) (14 13) (15 13) (16 15) (17 15) (18 17) (19 17)
      (20 19) (21 19) (22 21) (23 21) (24 23) (25 19) (26 4) (27 5) (28 4)
      (29 3) (30 4)))
    (graph
     ((0 (1 2)) (1 (2)) (2 (3 6)) (3 (4 6)) (4 (5 9)) (5 (6 7)) (6 (12))
      (7 (6 8)) (8 (11)) (9 (6 10)) (10 (11)) (11 (12)) (12 (13 14)) (13 (14))
      (14 (15 16)) (15 (16)) (16 (17 18)) (17 (18)) (18 (19 20)) (19 (20))
      (20 (21 22)) (21 (22)) (22 (23 24)) (23 (24)) (24 (25 26)) (25 (27))
      (26 (27)) (27 (28 29)) (28 (29)) (29 (30)) (30 ())))
    (preds
     ((0 ()) (1 (0)) (2 (0 1)) (3 (2)) (4 (3)) (5 (4)) (6 (2 3 5 7 9)) (7 (5))
      (8 (7)) (9 (4)) (10 (9)) (11 (8 10)) (12 (6 11)) (13 (12)) (14 (12 13))
      (15 (14)) (16 (14 15)) (17 (16)) (18 (16 17)) (19 (18)) (20 (18 19))
      (21 (20)) (22 (20 21)) (23 (22)) (24 (22 23)) (25 (24)) (26 (24))
      (27 (25 26)) (28 (27)) (29 (27 28)) (30 (29))))
    (idoms1
     ((0 0) (1 0) (2 0) (3 2) (4 3) (5 4) (6 2) (7 5) (8 26) (9 4) (10 29)
      (11 4) (12 2) (13 7) (14 7) (15 9) (16 9) (17 11) (18 11) (19 13) (20 13)
      (21 15) (22 15) (23 17) (24 17) (25 19) (26 19) (27 19) (28 21) (29 21)
      (30 23)))
    (idoms2
     ((0 0) (1 0) (2 0) (3 2) (4 3) (5 4) (6 2) (7 5) (8 7) (9 4) (10 9)
      (11 4) (12 2) (13 12) (14 12) (15 14) (16 14) (17 16) (18 16) (19 18)
      (20 18) (21 20) (22 20) (23 22) (24 22) (25 24) (26 24) (27 24) (28 27)
      (29 27) (30 29)))
    ((rpnum
      ((0 0) (1 1) (2 2) (3 3) (4 4) (5 7) (6 11) (7 8) (8 9) (9 5) (10 6)
       (11 10) (12 12) (13 13) (14 14) (15 15) (16 16) (17 17) (18 18) (19 19)
       (20 20) (21 21) (22 22) (23 23) (24 24) (25 26) (26 25) (27 27) (28 28)
       (29 29) (30 30)))
     (dom_tree
      ((0 (2 1 0)) (1 ()) (2 (12 6 3)) (3 (4)) (4 (11 5 9)) (5 (7)) (6 ())
       (7 (8)) (8 ()) (9 (10)) (10 ()) (11 ()) (12 (14 13)) (13 ()) (14 (16 15))
       (15 ()) (16 (18 17)) (17 ()) (18 (20 19)) (19 ()) (20 (22 21)) (21 ())
       (22 (24 23)) (23 ()) (24 (27 25 26)) (25 ()) (26 ()) (27 (29 28))
       (28 ()) (29 (30)) (30 ()))))
    (preds
     ((0 ()) (1 (0)) (2 (0 1)) (3 (2)) (4 (3)) (5 (4)) (6 (2 3 5 7 9)) (7 (5))
      (8 (7)) (9 (4)) (10 (9)) (11 (8 10)) (12 (6 11)) (13 (12)) (14 (12 13))
      (15 (14)) (16 (14 15)) (17 (16)) (18 (16 17)) (19 (18)) (20 (18 19))
      (21 (20)) (22 (20 21)) (23 (22)) (24 (22 23)) (25 (24)) (26 (24))
      (27 (25 26)) (28 (27)) (29 (27 28)) (30 (29))))
    (merge_blocks (2 6 11 12 14 16 18 20 22 24 27 29))
    ((x 0) (children (2 1 0)) (children_within (2)))
    ((x 0) (ys (2)) (c ()))
    ((x 0) (ys ()) (c ((BlockFollowedBy 2))))
    ((src 0) (target 2) (c (IfThenElse (BlockFollowedBy 2))))
    ((x 1) (children ()) (children_within ()))
    ((x 1) (ys ()) (c (IfThenElse (BlockFollowedBy 2))))
    ((src 1) (target 2) (c (IfThenElse (BlockFollowedBy 2))))
    ((x 2) (children (12 6 3)) (children_within (12 6)))
    ((x 2) (ys (12 6)) (c ()))
    ((x 2) (ys (6)) (c ((BlockFollowedBy 12))))
    ((x 2) (ys ()) (c ((BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 2) (target 6)
     (c (IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 3) (children (4)) (children_within ()))
    ((x 3) (ys ()) (c (IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 3) (target 6)
     (c (IfThenElse IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 4) (children (11 5 9)) (children_within (11)))
    ((x 4) (ys (11))
     (c (IfThenElse IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 4) (ys ())
     (c
      ((BlockFollowedBy 11) IfThenElse IfThenElse (BlockFollowedBy 6)
       (BlockFollowedBy 12))))
    ((x 9) (children (10)) (children_within ()))
    ((x 9) (ys ())
     (c
      (IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse (BlockFollowedBy 6)
       (BlockFollowedBy 12))))
    ((src 9) (target 6)
     (c
      (IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse
       (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 10) (children ()) (children_within ()))
    ((x 10) (ys ())
     (c
      (IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse
       (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 10) (target 11)
     (c
      (IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse
       (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 5) (children (7)) (children_within ()))
    ((x 5) (ys ())
     (c
      (IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse (BlockFollowedBy 6)
       (BlockFollowedBy 12))))
    ((x 7) (children (8)) (children_within ()))
    ((x 7) (ys ())
     (c
      (IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse
       (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 7) (target 6)
     (c
      (IfThenElse IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse
       IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 8) (children ()) (children_within ()))
    ((x 8) (ys ())
     (c
      (IfThenElse IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse
       IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 8) (target 11)
     (c
      (IfThenElse IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse
       IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 5) (target 6)
     (c
      (IfThenElse IfThenElse (BlockFollowedBy 11) IfThenElse IfThenElse
       (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 11) (children ()) (children_within ()))
    ((x 11) (ys ())
     (c (IfThenElse IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((src 11) (target 12)
     (c (IfThenElse IfThenElse (BlockFollowedBy 6) (BlockFollowedBy 12))))
    ((x 6) (children ()) (children_within ()))
    ((x 6) (ys ()) (c ((BlockFollowedBy 12))))
    ((src 6) (target 12) (c ((BlockFollowedBy 12))))
    ((x 12) (children (14 13)) (children_within (14)))
    ((x 12) (ys (14)) (c ()))
    ((x 12) (ys ()) (c ((BlockFollowedBy 14))))
    ((src 12) (target 14) (c (IfThenElse (BlockFollowedBy 14))))
    ((x 13) (children ()) (children_within ()))
    ((x 13) (ys ()) (c (IfThenElse (BlockFollowedBy 14))))
    ((src 13) (target 14) (c (IfThenElse (BlockFollowedBy 14))))
    ((x 14) (children (16 15)) (children_within (16)))
    ((x 14) (ys (16)) (c ()))
    ((x 14) (ys ()) (c ((BlockFollowedBy 16))))
    ((src 14) (target 16) (c (IfThenElse (BlockFollowedBy 16))))
    ((x 15) (children ()) (children_within ()))
    ((x 15) (ys ()) (c (IfThenElse (BlockFollowedBy 16))))
    ((src 15) (target 16) (c (IfThenElse (BlockFollowedBy 16))))
    ((x 16) (children (18 17)) (children_within (18)))
    ((x 16) (ys (18)) (c ()))
    ((x 16) (ys ()) (c ((BlockFollowedBy 18))))
    ((src 16) (target 18) (c (IfThenElse (BlockFollowedBy 18))))
    ((x 17) (children ()) (children_within ()))
    ((x 17) (ys ()) (c (IfThenElse (BlockFollowedBy 18))))
    ((src 17) (target 18) (c (IfThenElse (BlockFollowedBy 18))))
    ((x 18) (children (20 19)) (children_within (20)))
    ((x 18) (ys (20)) (c ()))
    ((x 18) (ys ()) (c ((BlockFollowedBy 20))))
    ((src 18) (target 20) (c (IfThenElse (BlockFollowedBy 20))))
    ((x 19) (children ()) (children_within ()))
    ((x 19) (ys ()) (c (IfThenElse (BlockFollowedBy 20))))
    ((src 19) (target 20) (c (IfThenElse (BlockFollowedBy 20))))
    ((x 20) (children (22 21)) (children_within (22)))
    ((x 20) (ys (22)) (c ()))
    ((x 20) (ys ()) (c ((BlockFollowedBy 22))))
    ((src 20) (target 22) (c (IfThenElse (BlockFollowedBy 22))))
    ((x 21) (children ()) (children_within ()))
    ((x 21) (ys ()) (c (IfThenElse (BlockFollowedBy 22))))
    ((src 21) (target 22) (c (IfThenElse (BlockFollowedBy 22))))
    ((x 22) (children (24 23)) (children_within (24)))
    ((x 22) (ys (24)) (c ()))
    ((x 22) (ys ()) (c ((BlockFollowedBy 24))))
    ((src 22) (target 24) (c (IfThenElse (BlockFollowedBy 24))))
    ((x 23) (children ()) (children_within ()))
    ((x 23) (ys ()) (c (IfThenElse (BlockFollowedBy 24))))
    ((src 23) (target 24) (c (IfThenElse (BlockFollowedBy 24))))
    ((x 24) (children (27 25 26)) (children_within (27)))
    ((x 24) (ys (27)) (c ()))
    ((x 24) (ys ()) (c ((BlockFollowedBy 27))))
    ((x 26) (children ()) (children_within ()))
    ((x 26) (ys ()) (c (IfThenElse (BlockFollowedBy 27))))
    ((src 26) (target 27) (c (IfThenElse (BlockFollowedBy 27))))
    ((x 25) (children ()) (children_within ()))
    ((x 25) (ys ()) (c (IfThenElse (BlockFollowedBy 27))))
    ((src 25) (target 27) (c (IfThenElse (BlockFollowedBy 27))))
    ((x 27) (children (29 28)) (children_within (29)))
    ((x 27) (ys (29)) (c ()))
    ((x 27) (ys ()) (c ((BlockFollowedBy 29))))
    ((src 27) (target 29) (c (IfThenElse (BlockFollowedBy 29))))
    ((x 28) (children ()) (children_within ()))
    ((x 28) (ys ()) (c (IfThenElse (BlockFollowedBy 29))))
    ((src 28) (target 29) (c (IfThenElse (BlockFollowedBy 29))))
    ((x 29) (children (30)) (children_within ()))
    ((x 29) (ys ()) (c ()))
    ((x 30) (children ()) (children_within ()))
    ((x 30) (ys ()) (c ()))
    (WasmSeq (WasmBlock (WasmIf 0 (WasmBr 1) (WasmSeq (WasmCode 1) (WasmBr 1))))
     (WasmSeq
      (WasmBlock
       (WasmSeq
        (WasmBlock
         (WasmIf 2 (WasmBr 1)
          (WasmIf 3 (WasmBr 2)
           (WasmSeq
            (WasmBlock
             (WasmIf 4 (WasmIf 9 (WasmBr 5) (WasmSeq (WasmCode 10) (WasmBr 2)))
              (WasmIf 5 (WasmIf 7 (WasmBr 6) (WasmSeq (WasmCode 8) (WasmBr 3)))
               (WasmBr 5))))
            (WasmSeq (WasmCode 11) (WasmBr 3))))))
        (WasmSeq (WasmCode 6) (WasmBr 0))))
      (WasmSeq
       (WasmBlock (WasmIf 12 (WasmBr 1) (WasmSeq (WasmCode 13) (WasmBr 1))))
       (WasmSeq
        (WasmBlock (WasmIf 14 (WasmBr 1) (WasmSeq (WasmCode 15) (WasmBr 1))))
        (WasmSeq
         (WasmBlock (WasmIf 16 (WasmBr 1) (WasmSeq (WasmCode 17) (WasmBr 1))))
         (WasmSeq
          (WasmBlock (WasmIf 18 (WasmBr 1) (WasmSeq (WasmCode 19) (WasmBr 1))))
          (WasmSeq
           (WasmBlock (WasmIf 20 (WasmBr 1) (WasmSeq (WasmCode 21) (WasmBr 1))))
           (WasmSeq
            (WasmBlock (WasmIf 22 (WasmBr 1) (WasmSeq (WasmCode 23) (WasmBr 1))))
            (WasmSeq
             (WasmBlock
              (WasmIf 24 (WasmSeq (WasmCode 26) (WasmBr 1))
               (WasmSeq (WasmCode 25) (WasmBr 1))))
             (WasmSeq
              (WasmBlock
               (WasmIf 27 (WasmBr 1) (WasmSeq (WasmCode 28) (WasmBr 1))))
              (WasmSeq (WasmCode 29) (WasmSeq (WasmCode 30) WasmReturn))))))))))))
    |}]

let%expect_test _ =
  test_trans 4429197;
  [%expect
    {|
    ((name func_43958d)
     (signature
      ((args (((name esp) (typ Int)))) (returns (((name esp) (typ Int))))))
     (blocks
      (((id 0)
        (instrs
         ((0 (OutsideContext (var esp) (typ Int)))
          (1 (LoadOp (var __ret_addr__) (op Load32) (addr (Ref 0))))
          (2 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Subtract) (lhs (Ref 0)) (rhs (Ref 3))))
          (5 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 2)))) (6 Nop)
          (7 (GetGlobalOp (var ecx) (global ((name ecx) (typ Int)))))
          (8 (Const __i32 4))
          (9 (BiOp (var esp) (op Subtract) (lhs (Ref 4)) (rhs (Ref 8))))
          (10 (StoreOp (op Store32) (addr (Ref 9)) (value (Ref 7))))
          (11 (StoreOp (op Store32) (addr (Ref 4)) (value (Ref 7)) (offset -4)))
          (12 (LoadOp (var eax) (op Load32) (addr (Ref 4)) (offset -4)))
          (13 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 12)) (offset 376)))
          (14 (Const __i32 4819544))
          (15 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 14))))
          (16 (Landmine (var eax) (typ Int))) (17 Nop)
          (18
           (BiOp (var __i32) (op FloatLessThanEqual) (lhs (Ref 13))
            (rhs (Ref 15))))
          (19 (UniOp (var __i32) (op EqualsZero) (operand (Ref 18)))) (20 Nop)
          (21 (SetGlobalOp (value (Ref 16)) (global ((name eax) (typ Int)))))
          (22 (SetGlobalOp (value (Ref 4)) (global ((name ebp) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 4)) (fail (Block 1)) (condition (Ref 19))))
        (roots ((Ref 1) (Ref 9) (Ref 19))))
       ((id 1)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (2 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset -4)))
          (3 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 1))))
          (4 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 2)) (offset 376)))
          (5 (BiOp (var __fl) (op FloatAdd) (lhs (Ref 3)) (rhs (Ref 4))))
          (6 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (7 (StoreOp (op Store32) (addr (Ref 6)) (value (Ref 5))))
          (8 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (9 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 8))))
          (10 (Const __i32 4819540))
          (11 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 10))))
          (12 (Landmine (var eax) (typ Int))) (13 Nop)
          (14
           (BiOp (var __i32) (op FloatGreaterThanEqual) (lhs (Ref 9))
            (rhs (Ref 11))))
          (15 (UniOp (var __i32) (op EqualsZero) (operand (Ref 14)))) (16 Nop)
          (17 (SetGlobalOp (value (Ref 2)) (global ((name ecx) (typ Int)))))
          (18 (SetGlobalOp (value (Ref 12)) (global ((name eax) (typ Int)))))))
        (terminator
         (Branch (succeed (Block 3)) (fail (Block 2)) (condition (Ref 15))))
        (roots ((Ref 15))))
       ((id 2)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (LoadOp (var eax) (op Load32) (addr (Ref 1)))) (3 (Const __i32 1))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))
          (7 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (8 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 7))))
          (9 (Const __i32 4819540))
          (10 (LoadOp (var __fl) (op FloatLoad32) (addr (Ref 9))))
          (11 (BiOp (var __fl) (op FloatSub) (lhs (Ref 8)) (rhs (Ref 10))))
          (12 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 12)))
          (13 (StoreOp (op Store32) (addr (Ref 12)) (value (Ref 11)))) (14 Nop)
          (15 (SetGlobalOp (value (Ref 5)) (global ((name ecx) (typ Int)))))
          (16 (SetGlobalOp (value (Ref 12)) (global ((name eax) (typ Int)))))))
        (terminator (Goto (Block 3))) (roots ()))
       ((id 3) (instrs ()) (terminator (Goto (Block 5))) (roots ()))
       ((id 4)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int)))))
          (1 (LoadOp (var eax) (op Load32) (addr (Ref 0)) (offset 8)))
          (2 (LoadOp (var eax) (op Load32) (addr (Ref 1)))) (3 (Const __i32 1))
          (4 (BiOp (var eax) (op Add) (lhs (Ref 2)) (rhs (Ref 3))))
          (5 (LoadOp (var ecx) (op Load32) (addr (Ref 0)) (offset 8)))
          (6 (StoreOp (op Store32) (addr (Ref 5)) (value (Ref 4))))
          (7 (SetGlobalOp (value (Ref 5)) (global ((name ecx) (typ Int)))))
          (8 (SetGlobalOp (value (Ref 4)) (global ((name eax) (typ Int)))))))
        (terminator (Goto (Block 5))) (roots ()))
       ((id 5)
        (instrs
         ((0 (GetGlobalOp (var ebp) (global ((name ebp) (typ Int))))) (1 Nop)
          (2 (LoadOp (var ebp) (op Load32) (addr (Ref 0)))) (3 (Const __i32 4))
          (4 (BiOp (var esp) (op Add) (lhs (Ref 0)) (rhs (Ref 3))))
          (5 (LoadOp (var __i32) (op Load32) (addr (Ref 4))))
          (6 (OutsideContext (var __ret_addr__) (typ Int)))
          (7 (BiOp (var __i32) (op Equal) (lhs (Ref 5)) (rhs (Ref 6))))
          (8 (AssertOp (condition (Ref 7)))) (9 (Const __i32 12))
          (10 (BiOp (var esp) (op Add) (lhs (Ref 4)) (rhs (Ref 9))))
          (11 (SetGlobalOp (value (Ref 2)) (global ((name ebp) (typ Int)))))))
        (terminator Return) (roots ((Ref 6) (Ref 10))))))
     (locals
      ((__ret_addr__ ((name __ret_addr__) (typ Int)))
       (esp ((name esp) (typ Int))))))
    (parent ((0 -1) (1 0) (2 1) (3 2) (4 0) (5 3)))
    (preorder ((0 0) (1 1) (2 2) (3 3) (4 5) (5 4)))
    (inv_preorder ((0 0) (1 1) (2 2) (3 3) (4 5) (5 4)))
    (semidom ((0 -1) (1 0) (2 1) (3 1) (4 0) (5 0)))
    (ancestor ((0 -1) (1 0) (2 1) (3 2) (4 -1) (5 3)))
    (label ((0 0) (1 0) (2 1) (3 1) (4 0) (5 0)))
    (graph ((0 (1 4)) (1 (2 3)) (2 (3)) (3 (5)) (4 (5)) (5 ())))
    (preds ((0 ()) (1 (0)) (2 (1)) (3 (1 2)) (4 (0)) (5 (3 4))))
    (idoms1 ((0 0) (1 0) (2 1) (3 1) (4 0) (5 0)))
    (idoms2 ((0 0) (1 0) (2 1) (3 1) (4 0) (5 0)))
    ((rpnum ((0 0) (1 2) (2 3) (3 4) (4 1) (5 5)))
     (dom_tree ((0 (5 1 4 0)) (1 (3 2)) (2 ()) (3 ()) (4 ()) (5 ()))))
    (preds ((0 ()) (1 (0)) (2 (1)) (3 (1 2)) (4 (0)) (5 (3 4))))
    (merge_blocks (3 5))
    ((x 0) (children (5 1 4 0)) (children_within (5)))
    ((x 0) (ys (5)) (c ()))
    ((x 0) (ys ()) (c ((BlockFollowedBy 5))))
    ((x 4) (children ()) (children_within ()))
    ((x 4) (ys ()) (c (IfThenElse (BlockFollowedBy 5))))
    ((src 4) (target 5) (c (IfThenElse (BlockFollowedBy 5))))
    ((x 1) (children (3 2)) (children_within (3)))
    ((x 1) (ys (3)) (c (IfThenElse (BlockFollowedBy 5))))
    ((x 1) (ys ()) (c ((BlockFollowedBy 3) IfThenElse (BlockFollowedBy 5))))
    ((src 1) (target 3)
     (c (IfThenElse (BlockFollowedBy 3) IfThenElse (BlockFollowedBy 5))))
    ((x 2) (children ()) (children_within ()))
    ((x 2) (ys ())
     (c (IfThenElse (BlockFollowedBy 3) IfThenElse (BlockFollowedBy 5))))
    ((src 2) (target 3)
     (c (IfThenElse (BlockFollowedBy 3) IfThenElse (BlockFollowedBy 5))))
    ((x 3) (children ()) (children_within ()))
    ((x 3) (ys ()) (c (IfThenElse (BlockFollowedBy 5))))
    ((src 3) (target 5) (c (IfThenElse (BlockFollowedBy 5))))
    ((x 5) (children ()) (children_within ()))
    ((x 5) (ys ()) (c ()))
    (WasmSeq
     (WasmBlock
      (WasmIf 0 (WasmSeq (WasmCode 4) (WasmBr 1))
       (WasmSeq
        (WasmBlock (WasmIf 1 (WasmBr 1) (WasmSeq (WasmCode 2) (WasmBr 1))))
        (WasmSeq (WasmCode 3) (WasmBr 1)))))
     (WasmSeq (WasmCode 5) WasmReturn))
    |}]

let%expect_test "graph" =
  let module IntSet = Set.Make (Int) in
  let graph =
    [
      [1];
      [ 2; 3 ];
      [ 4; 5 ];
      [ 8 ];
      [ 3; 7 ];
      [ 6 ];
      [ 7 ];
      [ 8 ];
      [];
    ]
    |> Array.Permissioned.of_list_map ~f:IntSet.of_list
  in
  print_s @@ sexp_of_array sexp_of_int @@ fst
  @@ Mir.Structure_cfg.find_idoms graph;
  [%expect {|
    (parent ((0 -1) (1 0) (2 1) (3 3) (4 2) (5 2) (6 7) (7 3) (8 4)))
    (preorder ((0 0) (1 1) (2 2) (3 4) (4 3) (5 8) (6 7) (7 5) (8 6)))
    (inv_preorder ((0 0) (1 1) (2 2) (3 4) (4 3) (5 7) (6 8) (7 6) (8 5)))
    (semidom ((0 -1) (1 0) (2 1) (3 1) (4 2) (5 2) (6 7) (7 2) (8 2)))
    (ancestor ((0 -1) (1 0) (2 1) (3 3) (4 2) (5 1) (6 1) (7 2) (8 4)))
    (label ((0 0) (1 0) (2 1) (3 2) (4 1) (5 2) (6 2) (7 2) (8 2)))
    (graph
     ((0 (1)) (1 (2 3)) (2 (4 5)) (3 (8)) (4 (3 7)) (5 (6)) (6 (7)) (7 (8))
      (8 ())))
    (preds
     ((0 ()) (1 (0)) (2 (1)) (3 (1 4)) (4 (2)) (5 (2)) (6 (5)) (7 (4 6))
      (8 (3 7))))
    (idoms1 ((0 0) (1 0) (2 1) (3 1) (4 2) (5 2) (6 7) (7 2) (8 1)))
    (idoms2 ((0 0) (1 0) (2 1) (3 1) (4 2) (5 2) (6 5) (7 2) (8 1)))
    (0 0 1 1 2 2 5 2 1)
    |}]
