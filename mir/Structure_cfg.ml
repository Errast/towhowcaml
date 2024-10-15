open! Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module AP = Array.Permissioned
module IntHashtbl = Hashtbl.Make (Int)

module IntHashSet : sig
  type elt = int
  type t [@@deriving sexp]

  val of_set : (elt, _) Set.t -> t
  val singleton : elt -> t
  val remove : t -> elt -> unit
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
  val copy : t -> t
  val of_set : (int, _) Set.t -> t
  val of_array : int array -> t
  val choose_exn : t -> elt
  val union_inplace : dest:t -> src:t -> unit
  val are_disjoint : t -> t -> bool

  include Container.S0 with type t := t and type elt := elt
end = struct
  type elt = int
  type t = unit IntHashtbl.t

  let of_set c =
    let hashset = IntHashtbl.create ~size:(Set.length c) () in
    Set.iter c ~f:(fun e -> Hashtbl.add hashset ~key:e ~data:() |> ignore);
    hashset

  let of_array c =
    let hashset = IntHashtbl.create ~size:(Array.length c) () in
    Array.iter c ~f:(fun e -> Hashtbl.add hashset ~key:e ~data:() |> ignore);
    hashset

  let copy = Hashtbl.copy
  let remove = Hashtbl.remove
  let mem = Hashtbl.mem
  let add t elt = Hashtbl.add t ~key:elt ~data:() |> ignore
  let choose_exn t = Hashtbl.choose_exn t |> fst
  let t_of_sexp sexp = array_of_sexp int_of_sexp sexp |> of_array
  let sexp_of_t t = Hashtbl.keys t |> List.sexp_of_t sexp_of_int

  let fold (t : t) ~init ~f =
    Hashtbl.fold t ~init ~f:(fun ~key ~data:_ acc -> f acc key)

  include Container.Make0 (struct
    type nonrec t = t

    module Elt = Int

    let fold = fold
    let iter = `Custom Hashtbl.iter_keys
    let length = `Custom Hashtbl.length
  end)

  let union_inplace ~dest ~src = iter src ~f:(add dest)

  let are_disjoint set1 set2 =
    let larger, smaller =
      if length set1 > length set2 then (set1, set2) else (set2, set1)
    in
    for_all smaller ~f:(fun e -> not @@ mem larger e)

  let singleton elt =
    let hashset = IntHashtbl.create ~size:1 () in
    add hashset elt;
    hashset
end

let sexp_of_iarray sexp_of_t a =
  Array.mapi a ~f:Tuple.T2.create |> [%sexp_of: (int * t) array]

let sexp_of_iparray sexp_of_t a =
  AP.mapi a ~f:Tuple.T2.create |> [%sexp_of: (int * t, _) AP.t]

module Block = struct
  include Block

  let sexp_of_t b =
    [%message "Block" ~_:(b.id : int) ~_:(b.terminator : terminator)]
end

type node = { edges : IntSet.t; block : int; terminator : Block.terminator }
[@@deriving sexp]

type graph = (node, immutable) AP.t
type 'perms pgraph = (node, 'perms) AP.t

let g_edges graph n = (AP.get graph n).edges
let g_block graph n = (AP.get graph n).block

type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of int * wasm_control * wasm_control
  | WasmCodeReturn of int
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : int; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of int
  | WasmFallthrough

let rec sexp_of_wasm_control =
  let open Sexplib.Sexp in
  function
  | WasmBlock b -> List [ Atom "WasmBlock"; sexp_of_wasm_control b ]
  | WasmLoop b -> List [ Atom "WasmLoop"; sexp_of_wasm_control b ]
  | WasmIf (c, t, f) ->
      [%message
        "WasmIf"
          ~_:(Block c : Types.branch_target)
          ~_:(t : wasm_control)
          ~_:(f : wasm_control)]
  | WasmCodeReturn b ->
      [%message "WasmCodeReturn" ~_:(Block b : Types.branch_target)]
  | WasmCode b -> [%message "WasmCode" ~_:(Block b : Types.branch_target)]
  | WasmReturn -> Atom "WasmReturn"
  | WasmFallthrough -> Atom "WasmFallthrough"
  | WasmBr i -> List [ Atom "WasmBr"; sexp_of_int i ]
  | WasmBrTable { bb_id; targets; default } ->
      [%message
        "WasmBrTable"
          ~_:(Block bb_id : Types.branch_target)
          (targets : int list)
          (default : int)]
  | WasmSeq (car, cdr) ->
      [%message "WasmSeq" ~_:(car : wasm_control) ~_:(cdr : wasm_control)]

let create_graph : (Block.t, immutable) AP.t -> _ pgraph =
 fun blocks ->
  let edges =
    AP.mapi blocks ~f:(fun i b ->
        assert (i = b.id);
        let edges =
          match b.terminator with
          | Goto (Block l) | BranchReturn { fail = Block l; _ } ->
              IntSet.singleton l
          | Branch { succeed = Block s; fail = Block f; _ } ->
              Set.add (IntSet.singleton s) f
          | Switch { cases; default = Block d; _ } ->
              List.fold
                ~f:(fun s (Block target) -> Set.add s target)
                ~init:(IntSet.singleton d) cases
          | Return -> IntSet.empty
        in
        edges)
  in
  AP.mapi edges ~f:(fun i edges ->
      { edges; block = i; terminator = (AP.get blocks i).terminator })

let graph_to_dot graph =
  let b = Buffer.create (AP.length graph * 30) in
  Buffer.add_string b "digraph G{";
  graph
  |> AP.iteri ~f:(fun s ds ->
         sprintf "%d [label = \"%d_%d\"]; %d -> {" s s ds.block s
         |> Buffer.add_string b;
         Set.iter ds.edges ~f:(fun e ->
             string_of_int e |> Buffer.add_string b;
             Buffer.add_char b ';');
         Buffer.add_char b '}');
  Buffer.add_char b '}';
  Buffer.contents b

let to_dot : Func.t -> string = fun f -> create_graph f.blocks |> graph_to_dot

let rev_postorder : graph -> (int, read) AP.t =
 fun graph ->
  let numbering = AP.create ~len:(AP.length graph) (-1) in
  (* who needs tail recursion *)
  let num = ref (AP.length graph) in
  let rec go i =
    if AP.get numbering i = -1 then (
      AP.set numbering i (-2);
      g_edges graph i |> Set.iter ~f:go;
      num := !num - 1;
      AP.set numbering i !num)
  in
  go 0;
  assert (AP.for_all ~f:(fun n -> n >= 0) numbering);
  numbering |> AP.to_array_id |> AP.of_array_id

(* SEMI-NCA algorithm *)
type preorder_num = { p_num : int } [@@unboxed]

let sexp_of_preorder_num n = sexp_of_int n.p_num

let find_idoms : graph -> int array * IntSet.t array =
 fun graph ->
  let inv_preorder = Array.create ~len:(AP.length graph) { p_num = -1 } in
  let parent = Array.create ~len:(AP.length graph) { p_num = -1 } in
  let preds = Array.create ~len:(AP.length graph) IntSet.empty in
  (* make preorder *)
  let preorder = Array.create ~len:(AP.length graph) (-1) in
  let get_preorder { p_num } = preorder.(p_num) in
  let n = ref 0 in
  let rec dfs i =
    let i_num = { p_num = !n } in
    Array.set inv_preorder i i_num;
    Array.set preorder !n i;
    n := !n + 1;
    g_edges graph i
    |> Set.iter ~f:(fun succ ->
           Array.set preds succ (Set.add preds.(succ) i);
           if inv_preorder.(succ).p_num = -1 then (
             parent.(succ) <- i_num;
             dfs succ))
  in
  dfs 0;

  (* semidominators *)
  (* values are preorder numbers, not nodes *)
  let semidom = Array.create ~len:(AP.length graph) { p_num = -1 } in
  let ancestor = Array.copy parent in
  (* indexes & values are preorder numbers, not nodes *)
  let label = Array.init ~f:(fun i -> { p_num = i }) @@ AP.length graph in
  let get_label { p_num } = label.(p_num) in
  let set_label { p_num } = Array.set label p_num in
  let rec path_compress v_n current_node =
    let v = get_preorder v_n in
    let a = ancestor.(v) in
    if a.p_num > current_node.p_num then (
      path_compress a current_node;
      Array.set ancestor v @@ ancestor.(get_preorder a);
      let ancestor_semidom = get_label a in
      if ancestor_semidom.p_num < (get_label v_n).p_num then
        set_label v_n ancestor_semidom)
  in
  for n = Array.length preorder - 1 downto 1 do
    let i = preorder.(n) in
    let semi =
      preds.(i)
      |> Set.fold ~init:n ~f:(fun min p ->
             let p_n = inv_preorder.(p) in
             path_compress p_n { p_num = n };
             (get_label p_n).p_num |> Int.min min)
    in
    let semi = { p_num = semi } in
    semidom.(i) <- semi;
    set_label { p_num = n } semi
  done;

  (* values here are preorder numbers, not nodes *)
  let idoms = parent in
  Array.iter preorder ~f:(fun i ->
      if i <> 0 then
        let sdom = semidom.(i) in
        let rec go j =
          if j.p_num <= sdom.p_num then j else go @@ idoms.(get_preorder j)
        in
        Array.set idoms i @@ go @@ parent.(i));
  idoms.(0) <- { p_num = 0 };

  let idoms = Array.map ~f:get_preorder idoms in
  idoms.(0) <- -1;

  (idoms, preds)

let is_reducible : _ pgraph -> int array -> (int, _) AP.t -> bool =
 fun graph idoms rpnum ->
  let rec dominates b1 b2 =
    if b1 = b2 then true else if b2 = -1 then false else dominates b1 idoms.(b2)
  in
  AP.for_alli graph ~f:(fun from node ->
      Set.for_all node.edges ~f:(fun target ->
          AP.get rpnum target > AP.get rpnum from || dominates target from))

type supernode = {
  id : int;
  preds : IntHashSet.t;
  succs : IntHashSet.t;
  nodes : int Vec.t;
}
[@@deriving sexp]

type supernode_graph = (int, supernode) Hashtbl.t

let make_reducible : _ pgraph -> IntSet.t array -> _ =
 fun graph preds ->
  let graph = Vec.of_array_perm graph in
  let sn_graph : supernode_graph =
    Hashtbl.create ~size:(Vec.length graph) (module Int)
  in
  Vec.iteri graph ~f:(fun i { edges; block; _ } ->
      Hashtbl.add_exn sn_graph ~key:i
        ~data:
          {
            id = i;
            nodes = Vec.singleton block;
            (* remove self-edges *)
            succs = Set.remove edges i |> IntHashSet.of_set;
            preds = Set.remove preds.(i) i |> IntHashSet.of_set;
          });
  let fresh_ref = ref (Vec.length graph) in
  let fresh () =
    let n = !fresh_ref in
    fresh_ref := n + 1;
    n
  in
  let remove_node i =
    let node = Hashtbl.find_exn sn_graph i in
    IntHashSet.iter node.preds ~f:(fun p ->
        IntHashSet.remove (Hashtbl.find_exn sn_graph p).succs i);
    IntHashSet.iter node.succs ~f:(fun s ->
        IntHashSet.remove (Hashtbl.find_exn sn_graph s).preds i);
    Hashtbl.remove sn_graph i
  in
  let add_node node =
    IntHashSet.iter node.preds ~f:(fun p ->
        IntHashSet.add (Hashtbl.find_exn sn_graph p).succs node.id);
    IntHashSet.iter node.succs ~f:(fun p ->
        IntHashSet.add (Hashtbl.find_exn sn_graph p).preds node.id);
    Hashtbl.add_exn sn_graph ~key:node.id ~data:node
  in
  let rec merge () =
    (* find all supernodes with exactly 1 predecessor *)
    let merge_nodes =
      Hashtbl.fold sn_graph ~init:[] ~f:(fun ~key:_ ~data acc ->
          if IntHashSet.length data.preds = 1 then data :: acc else acc)
    in
    (* stop at fixpoint *)
    if List.is_empty merge_nodes then ()
    else (
      List.iter merge_nodes ~f:(fun n ->
          assert (IntHashSet.length n.preds = 1);
          let pred =
            Hashtbl.find_exn sn_graph @@ IntHashSet.choose_exn n.preds
          in
          (* remove the node *)
          Hashtbl.remove sn_graph n.id;
          (* merge its nodes with the predecessor's *)
          Vec.append pred.nodes n.nodes;

          (* merge its successors with the predecessor's, remove any loops *)
          IntHashSet.union_inplace ~dest:pred.succs ~src:n.succs;
          IntHashSet.remove pred.succs pred.id;
          IntHashSet.remove pred.succs n.id;

          (* updates its successors' predecessors with the predecessor *)
          IntHashSet.iter n.succs ~f:(fun s ->
              let succ = Hashtbl.find_exn sn_graph s in
              IntHashSet.remove succ.preds n.id;
              if s <> pred.id then IntHashSet.add succ.preds pred.id));
      merge ())
  in
  let rec split () =
    merge ();
    (* stop at fixpoint *)
    if Hashtbl.length sn_graph > 1 then (
      let loop_supernode supernode =
        not @@ IntHashSet.are_disjoint supernode.preds supernode.succs
      in
      (* find the smallest supernode in a 2-node loop *)
      (* this is not guaranteed to exist ¯\_(ツ)_/¯ *)
      let[@warning "-8"] (Some dup_snode) =
        Hashtbl.fold sn_graph ~init:None ~f:(fun ~key:_ ~data -> function
          | Some n
          (* using # of blocks as a proxy for code size *)
            when Vec.length n.nodes > Vec.length data.nodes
                 && loop_supernode data ->
              Some data
          | None when loop_supernode data -> Some data
          | acc -> acc)
      in
      remove_node dup_snode.id;

      (* we have to not dup for one of these so we don't have unused nodes in graph *)
      let[@warning "-8"] (to_skip :: to_dup) =
        IntHashSet.to_list dup_snode.preds
        |> List.sort ~compare:(fun l r ->
               let len v = Vec.length (Hashtbl.find_exn sn_graph v).nodes in
               compare_int (len r) (len l))
      in
      add_node
        { dup_snode with id = fresh (); preds = IntHashSet.singleton to_skip };

      List.iter to_dup ~f:(fun pred ->
          (* the nodes in this supernode will be the next (length dup_snode.nodes) nodes in the graph *)
          let duped_node_vec =
            Vec.init (Vec.length dup_snode.nodes) (fun i ->
                Vec.length graph + i)
          in
          (* duplicate the appropriate nodes in graph *)
          let duped_nodes =
            Vec.fold dup_snode.nodes ~init:IntMap.empty ~f:(fun acc i ->
                let node = Vec.get graph i in
                Vec.add graph node;
                Map.add_exn acc ~key:i ~data:(Vec.length graph - 1))
          in
          let update_node i =
            let node = Vec.get graph i in
            let updated_succs =
              Map.fold duped_nodes ~init:node.edges ~f:(fun ~key ~data acc ->
                  if Set.mem acc key then Set.add (Set.remove acc key) data
                  else acc)
            in
            (* if any updates have been made, write them *)
            if not (phys_equal updated_succs node.edges) then
              Vec.set graph i
                {
                  node with
                  edges = updated_succs;
                  terminator =
                    Block.Terminator.map
                      (fun (Block n) ->
                        Block (Map.find duped_nodes n |> Option.value ~default:n))
                      node.terminator;
                }
          in

          (* update edges to old nodes *)
          Map.iter duped_nodes ~f:update_node;
          Vec.iter (Hashtbl.find_exn sn_graph pred).nodes ~f:update_node;

          add_node
            {
              id = fresh ();
              preds = IntHashSet.singleton pred;
              nodes = duped_node_vec;
              succs = IntHashSet.copy dup_snode.succs;
            });
      split ())
  in
  split ();
  Vec.to_perm_array graph

type containing_syntax =
  | IfThenElse of int option
  | LoopHeadedBy of int
  | BlockFollowedBy of int
[@@deriving sexp]

type context = { enclosing : containing_syntax list; fallthrough : int }
[@@deriving sexp]

let f : graph -> (int, _) AP.t -> int array -> IntSet.t array -> wasm_control =
 fun graph rpnum idoms preds ->
  let dom_tree =
    AP.init (AP.length graph) ~f:(fun i ->
        (* invert tree badly *)
        Array.foldi idoms ~init:[] ~f:(fun j ns idom ->
            if idom = i then j :: ns else ns)
        |> List.sort ~compare:(fun l r ->
               Int.compare (AP.get rpnum r) (AP.get rpnum l)))
  in
  let is_back_edge src target = AP.get rpnum target <= AP.get rpnum src in
  let merge_blocks =
    Array.foldi preds ~init:IntSet.empty ~f:(fun i acc ps ->
        if Set.count ps ~f:(fun v -> not @@ is_back_edge v i) >= 2 then
          Set.add acc i
        else acc)
  in
  let loop_headers =
    Array.foldi preds ~init:IntSet.empty ~f:(fun i acc ps ->
        if Set.exists ps ~f:(fun p -> is_back_edge p i) then Set.add acc i
        else acc)
  in
  let br_index : int -> context -> int =
   fun l c ->
    let depth =
      List.findi_exn c.enclosing ~f:(fun _ f ->
          match f with
          | (LoopHeadedBy ll | BlockFollowedBy ll | IfThenElse (Some ll))
            when l = ll ->
              true
          | _ -> false)
      |> fst
    in
    depth
  in
  let is_merge_node = Set.mem merge_blocks in
  let is_loop_header = Set.mem loop_headers in
  let generates_if b =
    match (AP.get graph b).terminator with
    | Branch _ | BranchReturn _ -> true
    | _ -> false
  in
  let inside c syntax = { c with enclosing = syntax :: c.enclosing } in
  let[@tail_mod_cons] rec node_within :
      int -> int list -> int option -> context -> _ =
   fun x ys prev_within c ->
    match (ys, prev_within) with
    | _ :: _, Some node ->
        WasmBlock (node_within x ys None @@ inside c (BlockFollowedBy node))
    | y :: ys, None ->
        WasmSeq
          ( (node_within [@tailcall]) x ys (Some y) { c with fallthrough = y },
            do_tree y c )
    | [], Some prev_within when not @@ generates_if x ->
        WasmBlock
          (node_within x [] None @@ inside c (BlockFollowedBy prev_within))
    | [], _ -> (
        match (AP.get graph x).terminator with
        | Goto (Block l) -> WasmSeq (WasmCode (g_block graph x), do_branch x l c)
        | Branch { succeed = Block t; fail = Block f; _ } ->
            let context = inside c @@ IfThenElse prev_within in
            WasmIf
              ( g_block graph x,
                do_branch x t context,
                (do_branch [@tailcall]) x f context )
        | Return -> WasmCodeReturn (g_block graph x)
        | BranchReturn { fail = Block fail; _ } ->
            WasmIf
              ( g_block graph x,
                WasmReturn,
                do_branch x fail @@ inside c @@ IfThenElse prev_within )
        | Switch { cases; default = Block default; _ } ->
            WasmBrTable
              {
                bb_id = g_block graph x;
                targets = List.map ~f:(fun (Block b) -> br_index b c) cases;
                default = br_index default c;
              })
  and[@tail_mod_cons] do_branch : int -> int -> context -> wasm_control =
   fun src target c ->
    if c.fallthrough = target then WasmFallthrough
    else if is_back_edge src target || is_merge_node target then
      try WasmBr (br_index target c)
      with _ ->
        raise_s
          [%message
            "no index"
              (src : int)
              (target : int)
              (c : context)
              (preds : IntSet.t iarray)]
    else do_tree target c
  and[@tail_mod_cons] do_tree : int -> context -> wasm_control =
   fun x c ->
    let children = AP.get dom_tree x in
    let children_within =
      match (AP.get graph x).terminator with
      | Switch _ -> children
      | _ -> List.filter ~f:is_merge_node children
    in
    if is_loop_header x then
      WasmLoop (node_within x children_within None @@ inside c (LoopHeadedBy x))
    else node_within x children_within None c
  in
  do_tree 0 { enclosing = []; fallthrough = -1 }

let structure_cfg : Func.t -> wasm_control =
 fun func ->
  let graph = create_graph func.blocks in
  let rpnum = rev_postorder graph in
  let idoms, preds = find_idoms graph in
  let graph, idoms, rpnum, preds =
    if is_reducible graph idoms rpnum then (graph, idoms, rpnum, preds)
    else
      let graph' = make_reducible graph preds in
      let rpnum' = rev_postorder graph' in
      let idoms', preds' = find_idoms graph' in
      assert (is_reducible graph' idoms' rpnum');
      (graph', idoms', rpnum', preds')
  in

  (* print_string @@ graph_to_dot graph; *)
  f graph rpnum idoms preds
