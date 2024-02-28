open Sexplib

let main () = let c = Radatnet.create () in
         let module Cmd = Radatnet.Commands in
         Cmd.open_file c "../th07.exe";
         Cmd.analyze_all c Cmd.LevelThree;
         let funcs = Cmd.list_functions c in
         List.map (Func_translator.translate c) funcs |> ignore;
         ()

let () = main ()
