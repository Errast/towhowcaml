open! Core
module Status_flags = Status_flags
module Func_translator = Func_translator
module Instr_translator = Instr_translator

let ignroe_funcs =
  Core.Hash_set.of_list
    (module Core.Int)
    [
      (*  PUSHF, why *)
      0x00470750;
      (*  Problems with blocks *)
      0x4857ce;
      0x48BCDD;
      0x48BB0A;
      0x47D437;
      0x47DD00;
      0x46B699;
      0x4674A8;
      0x478491;
      0x477509;
      0x47E0D1;
      0x482B7E;
      0x483A4D;
      0x483C50;
      0x47F80D;
      0x47DDD0;
      0x48C32C;
      0x48C133;
      0x48BB5D;
      0x48B93D;
      0x48B93D;
      0x48BA78;
      (*  For some reason 0x483d4e isn't a block *)
      0x48BC0D;
      0x47ECBD;
      (*  Direction flag *)
      0x0047d4b0;
      (*  Store fpu status word twice *)
      0x484685;
      (*  FLD after FCOMP before FNSTSW *)
      0x484220;
      (*  je right after call *)
      0x47f1ad;
      (*  Weird status word stuff + xltab *)
      0x483B35;
      (*  Actually looks at float status word *)
      0x00483ae7;
      (* just weird *)
      4212368;
      (* fnstcw, a lot of references *)
      0x0048bcaa;
      (* ffree  *)
      0x00461aa2;
    ]

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

let detect_illegal_edx (block : Mir.Block.t) =
  let recent = ref Mir.Instr.Ref.invalid in
  let bad = ref Mir.Instr.Ref.invalid in
  Array.existsi block.instrs
    ~f:
      Mir.Instr.(
        fun i instr ->
          match instr with
          | CallOp _ | CallIndirectOp _ ->
              bad := !recent;
              false
          | _ ->
              if
                not @@ phys_equal instr
                @@ replace_instr_ref ~from:!bad ~into:Ref.invalid instr
              then true
              else (
                if Poly.(assignment_var instr = Some { name = "edx" }) then
                  recent := Ref.Ref i;
                false))

let translate_func c addr ~intrinsics =
  let module Cmd = Radatnet.Commands in
  let blocks = Cmd.get_func_blocks c addr in
  let name = func_name c addr in
  let blocks = Array.map ~f:(make_block c) blocks in
  let res = Func_translator.translate ~intrinsics ~blocks ~name in
  if Array.exists ~f:detect_illegal_edx res.blocks then failwith "edx thing"
  else res

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
