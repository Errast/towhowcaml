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
let fpu_stack_pointer_global : ident = "__fpuStack__"
let input_compare_arg : ident = "__input_compare_arg__"
let float_sqrt_func : ident = "__float_sqrt__"
let float_sine_func : ident = "__float_sine__"
let float_cosine_func : ident = "__float_cosine__"
let float_scale_func : ident = "__float_scale__"
let seh_frame_global : ident = "__seh_frame__"
let dword_diff_func : ident = "__dword_diff__"
let dword_memset_func : ident = "__dword_memset__"

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
  @@ [ (input_compare_arg, Int) ]
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
