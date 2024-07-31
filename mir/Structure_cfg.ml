open! Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module AP = Array.Permissioned

type graph = (IntSet.t, immutable) AP.t
type 'a tree = Node of 'a * 'a tree list

let create_graph : (Mir.Block.t, _) AP.t -> graph =
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
  let rec num = ref (AP.length graph) in
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

let preorder : graph -> int array =
 fun graph ->
  let numbering = Array.create ~len:(AP.length graph) (-1) in
  (* who needs tail recursion *)
  let rec num = ref 0 in
  let rec go i =
    if Array.get numbering i = -1 then (
      Array.set numbering i !num;
      num := !num + 1;
      AP.get graph i |> Set.iter ~f:go)
  in
  go 0;
  assert (Array.for_all ~f:(fun n -> n >= 0) numbering);
  numbering

(* SEMI-NCA algorithm *)
let dom_tree : graph -> int tree =
 fun graph ->
  let semidom = Array.create ~len:(AP.length graph) 0 in
  let preorder = Array.copy semidom in
  let ancestor = Array.copy semidom in
  let idoms = Array.copy semidom in
  let label = Array.init ~f:Fn.id @@ AP.length graph in
  let preds = Array.create ~len:(AP.length graph) IntSet.empty in
  (* make preorder *)
  let n = ref 0 in
  let rec dfs i =
    Array.set semidom i !n;
    Array.set preorder !n i;
    n := !n + 1;
    AP.get graph i
    |> Set.iter ~f:(fun succ ->
           Array.set preds succ (Set.add (Array.get preds succ) i);
           if Array.get semidom succ = 0 then dfs succ)
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

  (* invert tree badly *)
  let rec subtree i =
    let children =
      Array.foldi idoms ~init:[] ~f:(fun j ns idom ->
          if idom = i then subtree j :: ns else ns)
    in
    Node (i, children)
  in
  subtree 0
