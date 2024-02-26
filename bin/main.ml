open Sexplib

let () = main ()
let main () = let c = Radatnet.create () in
         let module Cmd = Radatnet.Commands in
         Cmd.open_file c "../th07.exe";
         Cmd.analyze_all c Radatnet.Types.LevelThree;
         Cmd.get_func_blocks c |> Radatnet.Types.sexp_of_func_blocks |> Sexp.to_string |> print_string
