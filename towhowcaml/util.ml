open! Core
open Mir
open Radatnet

type intrinsic = { addr : int; signature : func_sig; name : string }
[@@deriving sexp]

external int32_to_float : int -> (float[@unboxed])
  = "towhowcaml_int32_to_double_byte" "towhowcaml_int32_to_double"
[@@noalloc]

let addr_to_func_name = Printf.sprintf "__func%x__"
let addr_to_index_func : ident = "__addrToIndex__"
let fpu_stack_pointer_global = {name="__fpuStack__"; typ = Int}
let input_compare_arg : ident = "__input_compare_arg__"
let float_sqrt_func : ident = "__float_sqrt__"
let float_sine_func : ident = "__float_sine__"
let float_cosine_func : ident = "__float_cosine__"
let float_tan_func : ident = "__float_tan__"
let float_scale_func : ident = "__float_scale__"
let seh_frame_global  = {name="__seh_frame__"; typ=Int}
let int_diff_func : ident = "__int_diff__"
let byte_diff_func : ident = "__byte_diff__"
let int_memset_func : ident = "__int_memset__"
let ret_addr_local : ident = "__ret_addr__"
let find_byte_func : ident = "__find_byte__"
let load_big_float_func : ident = "__load_big_float__"
let store_big_float_func : ident = "__store_big_float__"

let xmm_reg_to_global : X86reg.sse -> variable = function 
  | `xmm0 -> {name="__xmm0_global__"; typ=Vec}
  | `xmm1 -> {name="__xmm1_global__"; typ=Vec}
  | `xmm2 -> {name="__xmm2_global__"; typ=Vec}
  | `xmm3 -> {name="__xmm3_global__"; typ=Vec}
  | `xmm4 -> {name="__xmm4_global__"; typ=Vec}
  | `xmm5 -> {name="__xmm5_global__"; typ=Vec}
  | `xmm6 -> {name="__xmm6_global__"; typ=Vec}
  | `xmm7 -> {name="__xmm7_global__"; typ=Vec}

let std_call =
  {
    args = [ { name = X86reg.to_ident `esp; typ = Int } ];
    returns =
      [
        { name = X86reg.to_ident `eax; typ = Int };
        { name = X86reg.to_ident `esp; typ = Int };
        { name = X86reg.to_ident `edx; typ = Int };
      ];
  }

let fast_call =
  X86reg.
    {
      args =
        [
          { name = to_ident `ecx; typ = Int };
          { name = X86reg.to_ident `esp; typ = Int };
          { name = to_ident `edx; typ = Int };
        ];
      returns =
        [
          { name = X86reg.to_ident `eax; typ = Int };
          { name = X86reg.to_ident `esp; typ = Int };
          { name = X86reg.to_ident `edx; typ = Int };
        ];
    }

let used_locals =
  Map.of_alist_exn (module String)
  @@ [ (input_compare_arg, Int); (ret_addr_local, Int) ]
  @ List.map
      ~f:(fun r -> (X86reg.to_ident r, Int))
      [ `eax; `ebx; `ecx; `edx; `esi; `edi; `ebp; `esp ]
  @ List.map
      ~f:(fun r -> (X86reg.to_ident r, Vec))
      [
        `mm0;
        `mm1;
        `mm2;
        `mm3;
        `mm4;
        `mm5;
        `mm6;
        `mm7;
        `xmm0;
        `xmm1;
        `xmm2;
        `xmm3;
        `xmm4;
        `xmm5;
        `xmm6;
        `xmm7;
      ]

let global_of_tib_offset = function
  | 0 -> seh_frame_global
  | o -> failwithf "invalid tib offset: %d" o ()
