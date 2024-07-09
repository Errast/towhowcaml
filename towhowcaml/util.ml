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
let fpu_stack_pointer : ident = "__fpuStack__" 
let input_compare_arg : ident = "__input_compare_arg__"
let float_sqrt_func : ident = "__float_sqrt__"
let float_sine_func : ident = "__float_sine__"
let float_cosine_func : ident = "__float_cosine__"
let float_tan_func : ident = "__float_tan__"
let float_scale_func : ident = "__float_scale__"
let seh_frame_global = { name = "__seh_frame__"; typ = Int }
let int_diff_func : ident = "__int_diff__"
let byte_diff_func : ident = "__byte_diff__"
let int_memset_func : ident = "__int_memset__"
let ret_addr_local : ident = "__ret_addr__"
let find_byte_func : ident = "__find_byte__"
let load_big_float_func : ident = "__load_big_float__"
let store_big_float_func : ident = "__store_big_float__"
let float_pow_func : ident = "__float_pow__"
let float_rem_func : ident = "__float_rem__"
let float_mod_func : ident = "__float_mod__"

let std_call =
  X86reg.
    {
      args = [ { name = to_ident `esp; typ = Int } ];
      returns = [ { name = to_ident `esp; typ = Int } ];
    }

let fast_call =
  X86reg.
    {
      args = [ { name = to_ident `esp; typ = Int } ];
      returns = [ { name = to_ident `esp; typ = Int } ];
    }

let used_locals =
  let open Builder in
  Map.of_alist_exn (module String)
  @@ List.map ~f:(fun l -> (l.name, l))
  @@ [
       { name = input_compare_arg; scope = `Local; typ = Int };
       { name = ret_addr_local; scope = `Local; typ = Int };
       { name = X86reg.to_ident `esp; scope = `Local; typ = Int };
       { name = fpu_stack_pointer; scope = `Global; typ = Int };
     ]
  @ List.map
      ~f:(fun r -> { name = X86reg.to_ident r; scope = `Global; typ = Int })
      [ `eax; `ebx; `ecx; `edx; `esi; `edi; `ebp ]
  @ List.map
      ~f:(fun r -> { name = X86reg.to_ident r; scope = `Global; typ = Vec })
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
