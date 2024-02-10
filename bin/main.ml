let () = print_endline "Hello, World!"
let () = let c = Radatnet.create () in
         let module Cmd = Radatnet.Commands in
         Cmd.open_file c "../th07.exe";
         Cmd.analyze_all c Cmd.LevelTwo;
         Cmd.list_functions c |> List.fold_left (fun () -> print_int) ();
         Radatnet.destroy c