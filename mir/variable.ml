open Core

type t = { name : Types.ident } [@@unboxed]

let compare {name=lhs} {name=rhs} = compare_string lhs rhs
let hash {name} = hash_string name
let hash_fold_t a {name}= hash_fold_string a name


include
  Sexpable.Of_sexpable
    (String)
    (struct
      type nonrec t = t

      let to_sexpable t = t.name
      let of_sexpable s = { name = s }
    end)
