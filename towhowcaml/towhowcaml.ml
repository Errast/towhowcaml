open! Core
module Status_flags = Status_flags
module Func_translator = Func_translator
module Instr_translator = Instr_translator

let ignroe_funcs =
  Hash_set.of_list
    (module Core.Int)
    [
      (*  PUSHF, why *)
      0x00470750;
      (*  Problems with blocks *)
      0x4857ce;
      0x48BCDD;
      0x48BB0A;
      0x4674A8;
      0x477509;
      0x482B7E;
      0x483A4D;
      0x48C32C;
      0x48C133;
      (*  Direction flag *)
      0x0047d4b0;
      (*  Store fpu status word twice *)
      0x484685;
      (*  je right after call *)
      0x47f1ad;
      (*  Weird status word stuff + xltab *)
      (*  Actually looks at float status word *)
      0x00483ae7;
      (* fnstcw, a lot of references *)
      0x0048bcaa;
      (* ffree  *)
      0x00461aa2;
      (* switch table madness *)
      0x0047d4e0;
      (* stmxcsr can't be good *)
      0x0048ba20;
      (* input condition on first block *)
      0x0048b93d;
      4766733;
      4766557;
      0x0047ecbd;
    ]

let block_mods :
    (int, Radatnet.func_block array -> Radatnet.func_block array) Hashtbl.t =
  let fix_tail index bs =
    bs.(index) <- { (bs.(index) : Radatnet.func_block) with jump_to = None };
    bs
  and slice start stop array = Array.slice array start stop
  and ( >> ) l r x = r x |> l in
  Hashtbl.of_alist_exn
    (module Int)
    [
      (0x0048bb5d, fix_tail 5);
      (0x0048b93d, fix_tail 6);
      (0x0048bc0d, fix_tail 5);
      (0x0047d437, fix_tail 0);
      (0x0047dd00, slice 1 0);
      (0x0047ddc0, slice 0 1 >> fix_tail 0);
      (0x0046b699, fix_tail 0);
      (0x0047f150, slice 0 6 >> fix_tail 4);
      (0x00478491, fix_tail 14);
    ]

let contig_exceptions = [| 0x0047d285; 4724623; 0x0047ea7d; 0x0047ee10 |]

let make_intrinsics c =
  let open Core in
  let intrinsics = Hashtbl.create (module Int) in
  Radatnet.Commands.get_imports c
  |> List.iter ~f:(fun import ->
         Hashtbl.add_exn intrinsics ~key:import.table_address
           ~data:
             Util.
               {
                 name = import.lib_name ^ "_" ^ import.name;
                 addr = import.table_address;
                 signature = Util.std_call;
               });
  intrinsics

let make_block c block =
  let open Radatnet.Types in
  let open Func_translator in
  let open Radatnet.Commands in
  let terminator =
    match block with
    | { jump_to = Some next; fail_to = None; switch_to = None; _ } -> Goto next
    | { jump_to = Some succeed; fail_to = Some fail; switch_to = None; _ } ->
        Branch { succeed; fail }
    | { jump_to = None; fail_to = None; switch_to = Some { cases; offset }; _ }
      ->
        Switch { offset; cases }
    | { jump_to = None; fail_to = None; switch_to = None; _ } -> Return
    | _ ->
        Core.raise_s
          [%message
            "can't understand block terminator"
              (block : Radatnet.Types.func_block)]
  in
  let ops = Core.Array.map block.instrs ~f:(analyze_opcode c) in
  { offset = block.addr; ops; terminator }

let func_name c addr =
  Printf.sprintf "func_%x" @@ Radatnet.Commands.get_surrounding_func c addr

let translate_func c addr ~intrinsics =
  let module Cmd = Radatnet.Commands in
  let blocks = Cmd.get_func_blocks c addr in
  let blocks =
    Hashtbl.find block_mods addr
    |> Option.value_map ~default:blocks ~f:(fun f -> f blocks)
  in
  (* no non-contiguous blocks *)
  if not @@ Array.mem contig_exceptions addr ~equal:( = ) then
    Array.fold blocks ~init:blocks.(0).addr ~f:(fun a b ->
        if b.addr <> a then
          raise_s [%message "noncontiguous" ~addr:(b.addr : int)];
        a + b.size)
    |> ignore;
  let name = func_name c addr in
  let blocks = Array.map ~f:(make_block c) blocks in
  let res = Func_translator.translate ~intrinsics ~blocks ~name in
  res

let main c =
  let module Cmd = Radatnet.Commands in
  let funcs = Cmd.list_functions c in
  let intrinsics = make_intrinsics c in
  let start = Time_ns.now () in
  let _ =
    Array.to_sequence_mutable funcs
    |> Sequence.filter ~f:(fun f -> not @@ Hash_set.mem ignroe_funcs f)
    |> Sequence.filter_map ~f:(fun f ->
           try Some (translate_func c ~intrinsics f)
           with exn ->
             Exn.raise_with_original_backtrace
               (Exn.create_s [%message "oops" ~func_addr:(f : int) (exn : exn)])
               (Backtrace.Exn.most_recent ()))
    |> Sequence.to_list_rev
  in
  let stop = Time_ns.now () in
  Printf.printf "Elapsed: %f"
    Time_ns.(
      Span.( - ) (to_span_since_epoch stop) (to_span_since_epoch start)
      |> Span.to_sec)
