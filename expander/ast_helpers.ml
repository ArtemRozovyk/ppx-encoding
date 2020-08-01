open Ppxlib
module T = Ppxlib.Ast_builder.Default

(* Single field record injection function *)
let fun_record_from_name_inj ~loc name =
  let ppat_record =
    T.ppat_record ~loc
      [ ({ txt = Lident name; loc }, T.ppat_var ~loc { txt = name; loc }) ]
      Closed
  in
  let construct = T.pexp_ident ~loc { txt = Lident name; loc } in
  T.pexp_fun ~loc Nolabel None ppat_record construct

(* Single field record projection function *)
let fun_record_from_name_proj ~loc name =
  let pat_var = T.ppat_var ~loc { txt = name; loc } in
  let construct =
    T.pexp_record ~loc
      [
        ( { txt = Lident name; loc },
          T.pexp_ident ~loc { txt = Lident name; loc } );
      ]
      None
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let apply_mu_op ~loc name inter_expr =
  let pat_var = T.ppat_var ~loc { txt = name ^ "_encoding"; loc } in
  [%expr
    mu
      [%e T.pexp_constant ~loc (Pconst_string (name, None))]
      [%e T.pexp_fun ~loc Nolabel None pat_var inter_expr]]
