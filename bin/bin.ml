let () =
  let c = Radatnet.Core.create () in
  Radatnet.Commands.open_file c "/home/errast/code/Touhou7/th07.exe";
  Radatnet.Commands.analyze_all c LevelFour;
  Towhowcaml.main c
