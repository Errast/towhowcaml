(* Why are you still working on this? *)
(* -- I'm giving up after the code generation, not doing the runtime. *)
(* I found a new module paper and I also told leo about my module ideas *)
(* leo said they sound like fun ideas *)
(* that doesn't sound like he likes them *)
(* I made him read the papers *)
open! Core
open Util
module Out = Out_channel
module OA = Option_array
module AP = Array.Permissioned

type 'a alloc_location =
  | Drop
  | Stack
  | Local of 'a
  | Tee of 'a
  | Variable
  | TeeVariable
[@@deriving sexp, equal]

module Bool_map : sig
  type t

  val make : int -> t
  val set : t -> int -> bool -> unit
  val get : t -> int -> bool
end = struct
  type t = bytes

  let make len = Bytes.make len '\000'
  let set t i b = Bytes.set t i (if b then '\001' else '\000')
  let get t i = Char.equal '\001' @@ Bytes.get t i
end

let count_uses : Block.t -> (_, immutable) AP.t =
 fun block ->
  let arr = Array.create ~len:(AP.length block.instrs) 0 in
  AP.iter block.instrs ~f:(Instr.iter (fun (Ref i) -> arr.(i) <- arr.(i) + 1));
  (match block.terminator with
  | Branch { condition = Ref r; _ }
  | BranchReturn { condition = Ref r; _ }
  | Switch { switch_on = Ref r; _ } ->
      arr.(r) <- arr.(r) + 1
  | Goto _ | Return -> ());
  AP.of_array arr

let stack_allocate_block :
    Block.t -> (Types.local_type alloc_location, immutable) AP.t =
 fun block ->
  let num_uses = count_uses block in
  let locs =
    AP.mapi block.instrs ~f:(fun i instr ->
        match instr with
        | OutsideContext _ -> Variable
        | _ when Set.mem block.roots (Ref i) -> Variable
        | _ when AP.get num_uses i = 0 -> Drop
        | _ -> Local ())
  in
  let open struct
    type stack_value = Potential of Instr.Ref.t | Actual of Instr.Ref.t | Null
    [@@deriving sexp]
  end in
  let stack = Vec.create () in
  let is_used = Bool_map.make @@ AP.length block.instrs in
  let rec consume_arg r =
    match Vec.pop_exn stack with
    | Potential _ | Null -> consume_arg r
    | Actual arg ->
        assert (Instr.Ref.equal arg r);
        Bool_map.set is_used (Instr.Ref.to_int r) true
  in
  let consume_args =
    Instr.iter_right (fun (Ref r as arg) ->
        match AP.get locs r with
        | Stack -> consume_arg arg
        | (Tee _ | TeeVariable) when not @@ Bool_map.get is_used r ->
            consume_arg arg
        | Local _ | Tee _ | Variable | TeeVariable -> ()
        | Drop -> failwith "cannot consume Drop")
  in
  let stackify (Instr.Ref r) =
    let new_loc =
      match AP.get locs r with
      | Variable -> TeeVariable
      | Local () when AP.get num_uses r = 1 -> Stack
      | Local () -> Tee ()
      | Drop | TeeVariable | Tee _ | Stack -> failwith "already on stack"
    in
    AP.set locs r new_loc
  in
  let find_on_stack r =
    let rec go i =
      if i > 0 then
        match Vec.get stack i with
        | Potential arg when Instr.Ref.equal arg r ->
            Vec.set stack i (Actual arg);
            stackify arg
        | Potential _ | Null -> go (i - 1)
        | Actual _ -> remove (i - 1)
    and remove i =
      if i > 0 then
        match Vec.get stack i with
        | Potential arg when Instr.Ref.equal arg r -> Vec.set stack i Null
        | _ -> remove (i - 1)
    in
    go (Vec.length stack - 1)
  in

  AP.iteri block.instrs ~f:(fun i instr ->
      match instr with
      | OutsideContext _ -> ()
      | _ ->
          Instr.iter find_on_stack instr;
          (* print_s [%message "" (i : int) (stack : stack_value Vec.t)]; *)
          consume_args instr;
          if Instr.is_assignment instr then Vec.add stack @@ Potential (Ref i));
  AP.mapi locs ~f:(fun i loc ->
      match loc with
      | Local () -> Local (Instr.value_type @@ AP.get block.instrs i)
      | Tee () -> Tee (Instr.value_type @@ AP.get block.instrs i)
      | Drop -> Drop
      | Stack -> Stack
      | Variable -> Variable
      | TeeVariable -> TeeVariable)

type number_of_locals = { int : int; long : int; float : int; vec : int }
[@@deriving sexp]

let option_array_to_some_array : 'a OA.t -> ('a, [< _ perms ]) AP.t =
 fun arr ->
  for i = 0 to Option_array.length arr - 1 do
    assert (Option_array.is_some arr i)
  done;
  (* why? *)
  Option_array.copy arr |> Obj.magic

let allocate_locations :
    (Types.local_type alloc_location, immutable) AP.t ->
    Block.t ->
    ((Types.local_type * int) alloc_location, immutable) AP.t * number_of_locals
    =
 fun locs block ->
  let fresh_int = ref 0 in
  let free_int = ref [] in
  let fresh_long = ref 0 in
  let free_long = ref [] in
  let fresh_float = ref 0 in
  let free_float = ref [] in
  let fresh_vec = ref 0 in
  let free_vec = ref [] in
  let alloced_locs = Option_array.create ~len:(AP.length locs) in
  let get_local typ =
    let fresh, free =
      match typ with
      | Types.Int -> (fresh_int, free_int)
      | Long -> (fresh_long, free_long)
      | Float -> (fresh_float, free_float)
      | Vec -> (fresh_vec, free_vec)
    in
    match !free with
    | local :: free_list ->
        free := free_list;
        local
    | [] ->
        let local = !fresh in
        fresh := local + 1;
        local
  in

  let add_use (Instr.Ref r) =
    if OA.is_none alloced_locs r then
      let alloced_loc =
        match AP.get locs r with
        | (Stack | Variable | TeeVariable | Drop) as loc -> loc
        | Local typ -> Local (typ, get_local typ)
        | Tee typ -> Tee (typ, get_local typ)
      in
      OA.set_some alloced_locs r alloced_loc
  in
  let free_local local_id typ =
    let free_list =
      match typ with
      | Types.Int -> free_int
      | Float -> free_float
      | Long -> free_long
      | Vec -> free_vec
    in
    free_list := local_id :: !free_list
  in

  (match block.terminator with
  | Goto _ | Return -> ()
  | Branch { condition = r; _ }
  | BranchReturn { condition = r; _ }
  | Switch { switch_on = r; _ } ->
      add_use r);

  for i = AP.length locs - 1 downto 0 do
    let alloced_loc =
      match AP.get locs i with
      | Drop ->
          assert (OA.is_none alloced_locs i);
          OA.set_some alloced_locs i Drop;
          Drop
      | Variable when OA.is_none alloced_locs i ->
          OA.set_some alloced_locs i @@ Variable;
          Variable
      | _ -> OA.get_some_exn alloced_locs i
    in

    (match alloced_loc with
    | Drop | Stack | Variable | TeeVariable -> ()
    | Local (typ, l) | Tee (typ, l) -> free_local l typ);

    Instr.iter_right add_use @@ AP.get block.instrs i
  done;

  ( option_array_to_some_array alloced_locs,
    {
      int = !fresh_int;
      long = !fresh_long;
      float = !fresh_float;
      vec = !fresh_vec;
    } )

let validate_stack :
    Block.t ->
    ((Types.local_type * int) alloc_location, immutable) AP.t ->
    number_of_locals ->
    unit =
 fun block locs nums ->
  let int_locals = Array.create ~len:nums.int Instr.Ref.invalid in
  let long_locals = Array.create ~len:nums.long Instr.Ref.invalid in
  let float_locals = Array.create ~len:nums.float Instr.Ref.invalid in
  let vec_locals = Array.create ~len:nums.vec Instr.Ref.invalid in
  let get_local = function
    | Types.Int -> int_locals
    | Long -> long_locals
    | Float -> float_locals
    | Vec -> vec_locals
  in
  let variables = Hashtbl.create (module String) in
  let is_used = Bool_map.make @@ AP.length block.instrs in
  let pop_stack stack ref =
    match stack with
    | s :: stack when Instr.Ref.equal ref s -> stack
    | _ -> raise_s @@ Instr.Ref.sexp_of_t ref
  in
  let stack =
    AP.foldi block.instrs ~init:[] ~f:(fun i stack instr ->
        match instr with
        | Instr.OutsideContext { var = { name; _ }; _ } ->
            assert (not @@ Hashtbl.mem variables name);
            Hashtbl.add_exn variables ~key:name ~data:(Instr.Ref i);
            stack
        | _ -> (
            let stack =
              Instr.fold_right
                (fun (Ref r) stack ->
                  let stack =
                    match AP.get locs r with
                    | Stack -> pop_stack stack (Ref r)
                    | (Tee _ | TeeVariable) when not @@ Bool_map.get is_used r
                      ->
                        pop_stack stack (Ref r)
                    | Tee (typ, loc) | Local (typ, loc) ->
                        assert (Instr.Ref.equal (get_local typ).(loc) (Ref r));
                        stack
                    | Variable | TeeVariable ->
                        assert (
                          Instr.Ref.equal (Ref r)
                          @@ Hashtbl.find_exn variables
                               (Instr.assignment_var @@ AP.get block.instrs r
                               |> Option.value_exn)
                                 .name);
                        stack
                    | Drop -> failwith "can't have Drop as an argument"
                  in
                  Bool_map.set is_used r true;
                  stack)
                stack instr
            in
            let loc = AP.get locs i in
            (match loc with
            | Tee (typ, loc) | Local (typ, loc) ->
                (get_local typ).(loc) <- Ref i
            | Variable | TeeVariable ->
                Hashtbl.set variables ~data:(Ref i)
                  ~key:
                    (AP.get block.instrs i |> Instr.assignment_var
                   |> Option.value_exn)
                      .name
            | _ -> ());
            match loc with
            | Stack | Tee _ | TeeVariable -> Ref i :: stack
            | _ -> stack))
  in
  assert (List.is_empty stack)

let test block =
  let types = stack_allocate_block block in
  let locs, nums = allocate_locations types block in
  print_s
  @@ [%sexp_of: ((Types.local_type * int) alloc_location, _) iparray] locs;
  print_s @@ [%sexp_of: number_of_locals] nums;
  validate_stack block locs nums

let nums_max n1 n2 =
  {
    int = Int.max n1.int n2.int;
    long = Int.max n1.long n2.long;
    float = Int.max n1.float n2.float;
    vec = Int.max n1.vec n2.vec;
  }

let run out func =
  let structured = Structure_cfg.structure_cfg func in
  ()
