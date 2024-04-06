module Status_flags = Status_flags

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
    ]

let main () =
  let c = Radatnet.Core.create () in
  let module Cmd = Radatnet.Commands in
  print_string @@ Sys.getcwd ();
  Cmd.open_file c "../Touhou7/th07.exe";
  Cmd.analyze_all c Cmd.LevelThree;
  let funcs = Cmd.list_functions c in
  let start = Sys.time () in
  List.iter
    (fun f ->
      if not @@ Core.Hash_set.mem ignroe_funcs f then
        let f =
          Cmd.seek c f;
          Cmd.disassemble_function c
        in
        f.ops
        |> List.iter (fun o ->
               Cmd.seek c (o : Radatnet.Types.instr_info).offset;
               Cmd.analyze_opcode c |> ignore))
    funcs;
  let stop = Sys.time () in
  Printf.printf "Elapsed: %f" (stop -. start)

let () = main ()
