open! Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module AP = Array.Permissioned

let sexp_of_iarray sexp_of_t a =
  Array.mapi a ~f:Tuple.T2.create |> [%sexp_of: (int * t) array]

let sexp_of_iparray sexp_of_t a =
  AP.mapi a ~f:Tuple.T2.create |> [%sexp_of: (int * t, _) AP.t]

type graph = (IntSet.t, immutable) AP.t

type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of int * wasm_control * wasm_control
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : int; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of int
[@@deriving sexp]

let create_graph : (Block.t, immutable) AP.t -> graph =
 fun blocks ->
  AP.map blocks ~f:(fun b ->
      match b.terminator with
      | Goto (Block l) | BranchReturn { fail = Block l; _ } ->
          IntSet.singleton l
      | Branch { succeed = Block s; fail = Block f; _ } ->
          Set.add (IntSet.singleton s) f
      | Switch { cases; default = Block d; _ } ->
          List.fold
            ~f:(fun s (Block b) -> Set.add s b)
            ~init:(IntSet.singleton d) cases
      | Return -> IntSet.empty)

let rev_postorder : graph -> (int, read) AP.t =
 fun graph ->
  let numbering = AP.create ~len:(AP.length graph) (-1) in
  (* who needs tail recursion *)
  let num = ref (AP.length graph) in
  let rec go i =
    if AP.get numbering i = -1 then (
      AP.set numbering i (-2);
      AP.get graph i |> Set.iter ~f:go;
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
    AP.get graph i
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
    if v_n.p_num > current_node.p_num then (
      let v = get_preorder v_n in
      let a = ancestor.(v) in
      path_compress a current_node;
      let ancestor_semidom = get_label a in
      Array.set ancestor v @@ ancestor.(get_preorder a);
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
  print_s [%message "" (parent : preorder_num iarray)];
  print_s [%message "" (preorder : int iarray)];
  print_s [%message "" (inv_preorder : preorder_num iarray)];
  print_s [%message "" (semidom : preorder_num iarray)];
  print_s [%message "" (ancestor : preorder_num iarray)];
  print_s [%message "" (label : preorder_num iarray)];
  print_s [%message "" (graph : IntSet.t iparray)];
  print_s [%message "" (preds : IntSet.t iarray)];

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
  print_s [%message "" ~idoms1:(idoms : preorder_num iarray)];

  let idoms = Array.map ~f:get_preorder idoms in
  print_s [%message "" ~idoms2:(idoms : int iarray)];

  (idoms, preds)

let is_reducible : (IntSet.t, _) AP.t -> int array -> (int, _) AP.t -> bool =
 fun graph idoms rpnum ->
  let rec dominates b1 b2 =
    if b1 = b2 then true else if b2 = -1 then false else dominates b1 idoms.(b2)
  in
  AP.for_alli graph ~f:(fun from ->
      Set.for_all ~f:(fun target ->
          AP.get rpnum target > AP.get rpnum from || dominates target from))

type containing_syntax =
  | IfThenElse
  | LoopHeadedBy of int
  | BlockFollowedBy of int
[@@deriving sexp]

type context = containing_syntax list

let f : Func.t -> graph -> (int, _) AP.t -> IntSet.t array -> _ =
 fun func graph rpnum preds idoms ->
  let dom_tree =
    AP.init (AP.length graph) ~f:(fun i ->
        (* invert tree badly *)
        Array.foldi idoms ~init:[] ~f:(fun j ns idom ->
            if idom = i then j :: ns else ns)
        |> List.sort ~compare:(fun l r ->
               Int.compare (AP.get rpnum r) (AP.get rpnum l)))
  in
  print_s [%message "" (rpnum : int iparray) (dom_tree : int list iparray)];
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
    List.findi_exn c ~f:(fun _ f ->
        match f with
        | (LoopHeadedBy ll | BlockFollowedBy ll) when l = ll -> true
        | _ -> false)
    |> fst
  in
  let is_merge_node = Set.mem merge_blocks in
  let is_loop_header = Set.mem loop_headers in
  let[@tail_mod_cons] rec node_within : int -> int list -> context -> _ =
   fun x ys c ->
    print_s [%message "" (x : int) (ys : int list) (c : containing_syntax list)];
    match ys with
    | y :: ys ->
        WasmSeq
          ( WasmBlock (node_within x ys (BlockFollowedBy y :: c)),
            (do_tree [@tailcall]) y c )
    | [] -> (
        match (AP.get func.blocks x).terminator with
        | Goto (Block l) -> WasmSeq (WasmCode x, do_branch x l c)
        | Branch { succeed = Block t; fail = Block f; _ } ->
            WasmIf
              ( x,
                do_branch x t (IfThenElse :: c),
                (do_branch [@tailcall]) x f (IfThenElse :: c) )
        | Return -> WasmSeq (WasmCode x, WasmReturn)
        | BranchReturn { fail = Block fail; _ } ->
            WasmIf (x, WasmReturn, do_branch x fail (IfThenElse :: c))
        | Switch { cases; default = Block default; _ } ->
            WasmBrTable
              {
                bb_id = x;
                targets = List.map ~f:(fun (Block b) -> br_index b c) cases;
                default = br_index default c;
              })
  and[@tail_mod_cons] do_branch : int -> int -> context -> wasm_control =
   fun src target c ->
    if is_back_edge src target || is_merge_node target then
      WasmBr
        (print_s
           [%message "" (src : int) (target : int) (c : containing_syntax list)];
         br_index target c)
    else do_tree target c
  and[@tail_mod_cons] do_tree : int -> context -> wasm_control =
   fun x c ->
    let children = AP.get dom_tree x in
    let children_within =
      match (AP.get func.blocks x).terminator with
      | Switch _ -> children
      | _ -> List.filter ~f:is_merge_node children
    in
    print_s
      [%message "" (x : int) (children : int list) (children_within : int list)];
    if is_loop_header x then
      WasmLoop (node_within x children_within @@ (LoopHeadedBy x :: c))
    else node_within x children_within c
  in
  print_s [%message "" (preds : IntSet.t iarray)];
  print_s [%message "" (merge_blocks : IntSet.t)];
  do_tree 0 []

let structure_cfg : Func.t -> wasm_control =
 fun func ->
  let graph = create_graph func.blocks in
  let rpnum = rev_postorder graph in
  let idoms, preds = find_idoms graph in
  assert (is_reducible graph idoms rpnum);
  f func graph rpnum preds idoms
