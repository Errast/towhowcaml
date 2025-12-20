open! Core

let parse str =
  Lexing.from_string ~with_positions:true str |> Parser.main Lexer.token

let fadd_faddp_fiadd =
  parse
    {|
IF Instruction = FIADD
    THEN
        DEST := DEST + ConvertToDoubleExtendedPrecisionFP(SRC);
    ELSE (* Source operand is floating-point value *)
        DEST := DEST + SRC;
FI;
IF Instruction = FADDP
    THEN
        PopRegisterStack;
FI;|}

let add = parse {|
  OF := AddSignedOverflowed(DEST,SRC);
  CF := AddUnsignedOverflow(DEST,SRC);
  DEST := DEST + SRC;
  SF := SignBit(DEST);
  ZF := DEST = 0;
  PF := ParityBit(DEST);
  (* No one cares about AF *)
  |}

let get_pseudocode : Radatnet.X86_instr.t -> Types.statement list = function
  | FADD | FADDP | FIADD -> fadd_faddp_fiadd
  | ADD -> add
  | instr ->
      raise_s
        [%message "unimplemented instr" ~instr:(instr : Radatnet.X86_instr.t)]
