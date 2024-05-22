open! Core
open Radatnet
open Types
open Mir
module AP = Array
module Vec = Mir.Vec

type terminator =
  | Goto of int
  | Branch of { succeed : int; fail : int }
  | Switch of { offset : int; cases : jump_table_case list }
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
  offset : int;
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
  |> Option.value_or_thunk ~default:(fun () ->
         raise_s [%message "offset not found" (offset : int)])

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
      (* Technically there is an edge from here to the trap_block,
         but the trap block shouldn't do anything anyway so it doesn't matter*)
      List.iter ~f:(fun { jump; _ } -> add_branch jump) cases
  | Return | Trap -> ()

let translate_block c id block =
  let state = Instr_translator.initial_state () in
  let builder = Builder.create Util.used_locals in
  if id = 0 then
    Builder.load32 builder ~varName:Util.ret_addr_local
      (Builder.newest_var builder @@ X86reg.to_ident `esp)
    |> ignore;

  AP.foldi
    ~f:(fun i () op ->
      if i <> AP.length block.ops - 1 then
        Instr_translator.translate c.intrinsics builder state op)
    ~init:() block.ops;

  let terminator =
    if Poly.(block.terminator = Trap) then (
      Builder.unreachable builder ();
      Block.Return)
    else
      let term_op = AP.last block.ops in
      let block_term = block.terminator in
      let term_found =
        Instr_translator.translate_terminator c.intrinsics builder state term_op
          ~tail_position:Poly.(block_term = Return)
      in
      match (term_found, block_term) with
      | Nothing, Goto next_addr ->
          let next_block = id + 1 in
          if
            not
              (next_block < AP.length c.raw_blocks
              && (AP.get c.raw_blocks next_block).offset = next_addr)
          then
            raise_s
              [%message
                "terminator falls through, but doesn't match block info"
                  (term_op : opcode)
                  (block_term : terminator)];
          Goto (Block next_block)
      | Unconditional { target }, Goto next_addr when next_addr = target ->
          let next_block = offset_to_block_id c target in
          Goto (Block next_block)
      | Conditional { target; condition }, Branch { succeed; fail }
        when target = succeed ->
          let succeed_block = offset_to_block_id c succeed in
          let fail_block = offset_to_block_id c fail in
          if fail_block <> id + 1 then
            raise_s
              [%message
                "terminator conditionally falls through, but doesn't match \
                 block info"
                  (term_op : opcode)
                  (block_term : terminator)];
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
                if i <> case.value then
                  raise_s
                    [%message
                      "jump cases not sequential from 0"
                        (i : int)
                        (term_op : opcode)
                        (block_term : terminator)];
                Block (offset_to_block_id c case.jump))
              cases
          in
          Switch { cases; default = default_case; switch_on }
      | Return, Return -> Return
      | Unconditional _, Return ->
          (* tail call *)
          Return
      | _ ->
          raise_s
            [%message
              "Invalid terminator"
                (term_op : opcode)
                (term_found : Instr_translator.term_trans_result)
                ~block_term:(block.terminator : terminator)]
  in
  if Instr_translator.backward_direction state then
    raise_s [%message "ended in backward direction" ~block:(block.offset : int)];
  {
    id;
    builder;
    state;
    terminator;
    offset = block.offset;
    output_condition_op = INVALID;
  }

let add_input_blocks inverted_cfg block_data b =
  match Instr_translator.input_condition_opcode b.state with
  | None -> ()
  | Some condition_op ->
      let parents = inverted_cfg.(b.id) in
      if List.is_empty parents then
        raise_s
          [%message
            "block has input condition but no parents" ~block:(b.offset : int)];
      let open Poly in
      List.iter
        ~f:(fun p ->
          let parent = block_data.(p) in
          if parent.output_condition_op <> condition_op.id then (
            if parent.output_condition_op <> INVALID then
              raise_s
                [%message
                  "parent block has different output condition"
                    ~block:(b.offset : int)
                    ~parent:(parent.offset : int)];
            if Instr_translator.compare_instr parent.state = INVALID then
              raise_s
                [%message
                  "parent block has no output condition"
                    ~block:(b.offset : int)
                    ~parent:(parent.offset : int)];
            Instr_translator.translate_output_condition parent.builder
              parent.state condition_op;
            parent.output_condition_op <- condition_op.id))
        parents

let add_used_local used_locals ident typ =
  let found_typ =
    (Hashtbl.find_or_add
       ~default:(fun () -> { name = ident; typ })
       used_locals ident)
      .typ
  in
  if Poly.(found_typ <> typ) then
    raise_s
      [%message
        "multiple types for local"
          (ident : ident)
          ~typ1:(found_typ : local_type)
          ~typ2:(typ : local_type)]

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
  if
    not
    @@ AP.is_sorted_strictly
         ~compare:(fun l r -> compare_int l.offset r.offset)
         blocks
  then raise_s [%message "blocks not sorted" (name : ident)];

  let c = { inverted_cfg; intrinsics; trap_block; raw_blocks = blocks } in
  AP.foldi ~f:(fun i () b -> add_block_branches c i b) ~init:() blocks;
  let block_data = AP.mapi ~f:(translate_block c) c.raw_blocks in
  AP.iter ~f:(add_input_blocks c.inverted_cfg block_data) block_data;
  let used_locals = Hashtbl.create (module String) in
  let signature = Util.fast_call in
  List.iter ~f:(fun a -> add_used_local used_locals a.name a.typ) signature.args;
  List.iter signature.returns ~f:(fun r ->
      add_used_local used_locals r.name r.typ);
  let mir_blocks = AP.map ~f:(to_mir_block used_locals) block_data in
  Func.
    {
      name;
      signature;
      blocks = mir_blocks;
      locals = Map.of_hashtbl_exn (module String) used_locals;
    }
