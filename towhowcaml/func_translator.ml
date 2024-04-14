open! Core
open Radatnet
open Types
open Mir
module AP = Array
module Vec = Mir.Vec

type jump_case = { num : int; target : int } [@@deriving sexp]

type terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of { offset : int; cases : jump_case list }
  | Return
  | Trap
[@@deriving sexp]

type block = {
  offset : int;
  terminator : terminator;
  ops : Radatnet.Types.opcode array;
}
[@@deriving sexp]

type block_data = {
  id : int;
  builder : Mir.Builder.t;
  state : Instr_translator.state;
  terminator : Block.terminator;
  mutable output_condition_op : X86_instr.t;
}

type context = {
  intrinsics : (int, Util.intrinsic) Hashtbl.t;
  inverted_cfg : int list array;
  raw_blocks : block array;
  trap_block : branch_target option;
}
[@@deriving sexp_of]

let offset_to_block_id c offset =
  AP.binary_search c.raw_blocks
    ~compare:(fun b o -> compare_int b.offset o)
    `First_equal_to offset
  |> Option.value_exn

let add_block_branches c id (block : block) =
  let add_branch b =
    let b_id = offset_to_block_id c b in
    AP.set c.inverted_cfg b_id (id :: AP.get c.inverted_cfg b_id)
  in
  match block.terminator with
  | Goto b -> add_branch b
  | Branch { succeed; fail; _ } ->
      add_branch succeed;
      add_branch fail
  | Switch { cases; _ } ->
      let[@warning "-8"] (Some (Block trap_block)) = c.trap_block in
      add_branch trap_block;
      List.fold ~f:(fun () { target; _ } -> add_branch target) ~init:() cases
  | Return | Trap -> ()

let translate_block c id block =
  let state = Instr_translator.initial_state () in
  let builder = Builder.create Util.used_locals in
  if id = 0 then
    Builder.get_global builder Util.stack_pointer_global Int
      ~varName:(Radatnet.X86reg.to_ident `esp)
    |> ignore;

  AP.foldi
    ~f:(fun i () op ->
      if i <> AP.length block.ops - 1 then
        Instr_translator.translate c.intrinsics builder state op)
    ~init:() block.ops;

  let terminator =
    if Poly.(block.terminator = Trap) then (
      Builder.unreachable builder ();
      Block.Goto Return)
    else
      let term_op = AP.last block.ops in
      let term_found =
        Instr_translator.translate_terminator c.intrinsics builder state term_op
      in
      match (term_found, block.terminator) with
      | Nothing, Goto next_addr ->
          let next_block = id + 1 in
          assert (
            next_block < AP.length c.raw_blocks
            && (AP.get c.raw_blocks next_block).offset = next_addr);
          Goto (Block next_block)
      | Unconditional { target }, Goto next_addr when next_addr = target ->
          let next_block = offset_to_block_id c target in
          Goto (Block next_block)
      | Conditional { target; condition }, Branch { succeed; fail }
        when target = succeed ->
          let succeed_block = offset_to_block_id c succeed in
          let fail_block = offset_to_block_id c fail in
          assert (fail_block = id + 1);
          Branch
            {
              succeed = Block succeed_block;
              fail = Block fail_block;
              condition;
            }
      | Switch { switch_on; table_addr }, Switch { offset; cases }
        when table_addr > 0 && term_op.address = offset ->
          let default_case = Option.value_exn c.trap_block in
          let cases =
            List.mapi
              ~f:(fun i case ->
                assert (i = case.num);
                Block (offset_to_block_id c case.target))
              cases
          in
          Switch { cases; default = default_case; switch_on }
      | _ ->
          raise_s
            [%message
              "Invalid terminator"
                (term_op : opcode)
                (term_found : Instr_translator.term_trans_result)
                ~term_op:(block.terminator : terminator)]
  in
  assert (not state.backward_direction);
  { id; builder; state; terminator; output_condition_op = INVALID }

let add_input_blocks inverted_cfg block_data b =
  let parents = inverted_cfg.(b.id) in
  assert (not @@ List.is_empty parents);
  let condition_instr = b.state.input_condition_instr in
  List.fold
    ~f:(fun () p ->
      let parent = block_data.(p) in
      if Poly.(parent.output_condition_op <> condition_instr) then (
        assert (Poly.(parent.output_condition_op = INVALID));
        assert (Poly.(parent.state.compare_instr <> INVALID));
        Instr_translator.translate_output_condition parent.builder parent.state
          condition_instr;
        parent.output_condition_op <- condition_instr))
    ~init:() parents

let add_used_local used_locals ident typ =
  assert (
    Poly.(
      (Hashtbl.find_or_add
         ~default:(fun () -> { name = ident; typ })
         used_locals ident)
        .typ = typ))

let to_mir_block used_locals b =
  let instrs, current_vars, _, roots = Mir.Builder.deconstruct b.builder in
  let roots =
    Hashtbl.fold
      ~f:(fun ~key ~data roots ->
        if Mir.Builder.is_temp key then roots
        else (
          add_used_local used_locals key data.typ;
          Set.add roots data.index))
      ~init:roots current_vars
  in
  Block.{ id = b.id; terminator = b.terminator; instrs; roots }

let trap_block_arr =
  [|
    {
      offset = Int.max_value;
      terminator = Trap;
      ops = AP.init 0 ~f:(fun _ -> failwith "");
    };
  |]

let translate ~intrinsics ~name ~(blocks : block array) =
  let trap_block, blocks =
    if
      AP.exists
        ~f:(function { terminator = Switch _; _ } -> true | _ -> false)
        blocks
    then
      let id = AP.length blocks in
      (Some (Block id), AP.append blocks trap_block_arr)
    else (None, blocks)
  in
  let num_blocks = AP.length blocks in
  let inverted_cfg = AP.create ~len:num_blocks [] in
  assert (
    AP.is_sorted_strictly
      ~compare:(fun l r -> compare_int l.offset r.offset)
      blocks);

  let c = { inverted_cfg; intrinsics; trap_block; raw_blocks = blocks } in
  AP.foldi ~f:(fun i () b -> add_block_branches c i b) ~init:() blocks;
  let block_data = AP.mapi ~f:(translate_block c) c.raw_blocks in
  AP.fold
    ~f:(fun () b ->
      if Poly.(b.state.input_condition_instr <> INVALID) then
        add_input_blocks c.inverted_cfg block_data b)
    ~init:() block_data;
  let used_locals = Hashtbl.create (module String) in
  let signature = Util.fast_call in
  List.fold
    ~f:(fun () a -> add_used_local used_locals a.name a.typ)
    ~init:() signature.args;
  add_used_local used_locals signature.return.name Util.fast_call.return.typ;
  let mir_blocks = AP.map ~f:(to_mir_block used_locals) block_data in
  Func.
    {
      name;
      signature;
      blocks = mir_blocks;
      locals = Map.of_hashtbl_exn (module String) used_locals;
    }
