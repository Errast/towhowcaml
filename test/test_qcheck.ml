open! Core

let packed_bitarray_tests =
  let open QCheck2 in
  let gen_width = Gen.int_range 1 (32 - 7) in
  let gen_arr =
    Gen.(
      let* width = gen_width in
      let+ a = array_size (nat >|= ( + ) 1) (int_bound (Int.pow 2 width - 1)) in
      (width, a))
  in
  let index_gen =
    Gen.(
      let* width, arr = gen_arr in
      let+ i = int_bound (Array.length arr - 1) in
      (width, arr, i))
  in
  let set_gen =
    Gen.(
      let* width, arr, i = index_gen in
      let+ x = int_bound @@ (Int.pow 2 width - 1) in
      (width, arr, i, x))
  in
  let open Towhowcaml__Static_dict.Packed_bitarray in
  [
    Test.make ~name:"id = to_array . of_array" gen_arr
      ~print:Print.(tup2 int (array int))
      (fun (width, a) ->
        Array.equal Int.equal a @@ to_array (of_array ~width a));
    Test.make ~name:"of_array . to_array = id" gen_arr
      ~print:Print.(tup2 int (array int))
      (fun (width, a) ->
        Array.equal Int.equal a @@ to_array (of_array ~width a));
    Test.make ~name:"Array.get = get . of_array " index_gen
      ~print:Print.(tup3 int (array int) int)
      (fun (width, a, i) -> Array.get a i = get (of_array ~width a) i);
    Test.make ~name:"Array.set = to_array . set . of_array" set_gen
      ~print:Print.(tup4 int (array int) int int)
      (fun (width, a, i, x) ->
        let packed = of_array ~width a in
        Array.set a i x;
        set packed i x;
        Array.equal Int.equal a (to_array packed));
  ]

let perfect_hash_tests =
  let open QCheck2 in
  let gen_set =
    Gen.(
      list_size (nat >|= ( + ) 1) (int_range 0 ((1 lsl 32) - 1))
      >|= Hash_set.of_list (module Int)
      >|= Hash_set.to_array)
  in
  let gen_elems =
    Gen.(
      let* elems = gen_set in
      match
        Towhowcaml__Perfect_hash.make_perfect_hash
          (Array.Permissioned.of_array_id elems)
      with
      | Some phf ->
          let+ e1, e2 = pair (oneofa elems) (oneofa elems) in
          (elems, Some phf, e1, e2)
      | None -> pure (elems, None, 0, 0))
  in
  let open Towhowcaml__Perfect_hash in
  [
    Test.make ~name:"injective" gen_elems
      ~print:
        Print.(
          tup4 (array int)
            (function Some phf -> int phf.seed | None -> "<none>")
            int int)
      (function
        | _, None, _, _ -> true
        | _, Some phf, e1, e2 -> Bool.equal (hash phf e1 = hash phf e2) (e1 = e2));
  ]

(* let _ = *)
  (* QCheck_base_runner.run_tests @@ packed_bitarray_tests @ perfect_hash_tests *)
