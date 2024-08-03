open! Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module AP = Array.Permissioned

type graph = (IntSet.t, immutable) AP.t

type wasm_control =
  | WasmBlock of wasm_control
  | WasmLoop of wasm_control
  | WasmIf of int * wasm_control * wasm_control
  | WasmReturn
  | WasmBr of int
  | WasmBrTable of { bb_id : int; targets : int list; default : int }
  | WasmSeq of wasm_control * wasm_control
  | WasmCode of { bb_id : int }
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
let find_idoms : graph -> int array * IntSet.t array =
 fun graph ->
  let semidom =  Array.create ~len:(AP.length graph) (-1)in
  let ancestor = Array.create ~len:(AP.length graph) 0 in
  let idoms = Array.copy ancestor in
  let label = Array.init ~f:Fn.id @@ AP.length graph in
  let preds = Array.create ~len:(AP.length graph) IntSet.empty in
  (* make preorder *)
  let preorder = Array.create ~len:(AP.length graph) (-1) in
  let n = ref 0 in
  let rec dfs i =
    Array.set semidom i !n;
    Array.set preorder !n i;
    n := !n + 1;
    AP.get graph i
    |> Set.iter ~f:(fun succ ->
           Array.set preds succ (Set.add (Array.get preds succ) i);
           if Array.get semidom succ = -1 then dfs succ)
  in
  dfs 0;

  (* semidominators *)
  let rec path_compress v =
    let a = Array.get ancestor v in
    if Array.get ancestor a <> 0 then (
      path_compress a;
      let prev_semidom = Array.get label a in
      if prev_semidom < Array.get label v then Array.set label v prev_semidom;
      Array.set ancestor v @@ Array.get ancestor a)
  in
  for n = AP.length graph - 1 downto 1 do
    let i = Array.get preorder n in
    let semi =
      Array.get preds i
      |> Set.fold ~init:i ~f:(fun min p ->
             path_compress p;
             Array.get label p |> Int.min min)
    in
    Array.set semidom i semi;
    Array.set label i semi
  done;

  Array.iteri preorder ~f:(fun n i ->
      if n <> 0 then
        let rec go () =
          let id = Array.get idoms i in
          if id > Array.get semidom i then (
            Array.set idoms i @@ Array.get idoms id;
            go ())
        in
        go ());
  Array.set idoms 0 (-1);

  (idoms, preds)

type containing_syntax =
  | IfThenElse
  | LoopHeadedBy of int
  | BlockFollowedBy of int

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
  let merge_blocks =
    Array.foldi preds ~init:IntSet.empty ~f:(fun i acc ps ->
        if Set.count ps ~f:(fun v -> v > i) >= 2 then Set.add acc i else acc)
  in
  let is_back_edge src target = AP.get rpnum target <= AP.get rpnum src in
  let loop_headers =
    Array.foldi preds ~init:IntSet.empty ~f:(fun i acc ps ->
        if Set.exists ps ~f:(fun p -> is_back_edge p i) then Set.add acc i
        else acc)
  in
  let ( ++ ) l r = WasmSeq (l, r) in
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
  let rec node_within : int -> int list -> context -> _ =
   fun x ys c ->
    match ys with
    | y :: ys -> node_within x ys (BlockFollowedBy y :: c) ++ do_tree y c
    | [] -> (
        match (AP.get func.blocks x).terminator with
        | Goto (Block l) -> do_branch x l c
        | Branch { succeed = Block t; fail = Block f; _ } ->
            WasmIf
              ( x,
                do_branch x t (IfThenElse :: c),
                do_branch x f (IfThenElse :: c) )
        | Return -> WasmReturn
        | BranchReturn { fail = Block fail; _ } ->
            WasmIf (x, WasmReturn, do_branch x fail (IfThenElse :: c))
        | Switch { cases; default = Block default; _ } ->
            WasmBrTable
              {
                bb_id = x;
                targets = List.map ~f:(fun (Block b) -> br_index b c) cases;
                default = br_index default c;
              })
  and do_branch : int -> int -> context -> wasm_control =
   fun src target c ->
    if is_back_edge src target || is_merge_node target then
      WasmBr (br_index target c)
    else do_tree target c
  and do_tree : int -> context -> wasm_control =
   fun x c ->
    let children = AP.get dom_tree x in
    let children_within =
      match (AP.get func.blocks x).terminator with
      | Switch _ -> children
      | _ -> List.filter ~f:(fun b -> Set.mem merge_blocks b) children
    in
    let code_for_x = node_within x children_within in
    if is_loop_header x then WasmLoop (code_for_x (LoopHeadedBy x :: c))
    else code_for_x c
  in
  do_tree 0 []

let structure_cfg : Func.t -> wasm_control =
 fun func ->
  let graph = create_graph func.blocks in
  let rpnum = rev_postorder graph in
  let idoms, preds = find_idoms graph in
  f func graph rpnum preds idoms
