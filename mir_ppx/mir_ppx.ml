open Ppxlib

let expander ~ctxt : longident -> expression -> cases -> expression =
 fun arr discr cases -> 
   let rec go cases = List.map Ast_pattern.(parse @@ ppat_construct __ (__ )) cases  in discr

let extension =
  Extension.V3.declare "mir" Extension.Context.expression
    Ast_pattern.(
      single_expr_payload
      @@ pexp_match (pexp_tuple @@ pexp_ident __ ^:: __ ^:: nil) __)
    expander

let rule = Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "mir_ppx"
