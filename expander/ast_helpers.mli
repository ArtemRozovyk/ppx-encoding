module T = Ppxlib.Ast_builder.Default
val name_of_type_name : Base.String.t -> Base.String.t
val generate_encoding : Ppxlib.core_type -> Ppxlib__.Import.expression
val take_a :
  Base__Int.t -> 'a list -> 'a Base__List.t -> 'a Base__List.t * 'a list
val take_split : Base__Int.t -> 'a list -> 'a Base__List.t * 'a list
val make_obj_arg : Ppxlib.core_type -> string * Ppxlib.core_type
val objN_enc_from_ldl :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration Base.List.t -> Ppxlib__.Import.expression
val make_obj_n :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration Base.List.t -> Ppxlib__.Import.expression
val fun_record_from_name_inj :
  loc:Ppxlib__.Import.location -> string -> Ppxlib__.Import.expression
val fun_record_from_name_proj :
  loc:Ppxlib__.Import.location -> string -> Ppxlib__.Import.expression
val apply_mu_op :
  loc:Ppxlib__.Import.location ->
  Base.String.t -> Ppxlib__.Import.expression -> Ppxlib.expression
val make_nested_ppat_tuple :
  loc:Ppxlib__.Import.location ->
  string Base.List.t -> Ppxlib__.Import.pattern
val make_nested_pexp_tuple :
  loc:Ppxlib__.Import.location ->
  string Base.List.t -> Ppxlib__.Import.expression
val fun_ll_inj :
  loc:Ppxlib__.Import.location ->
  string Base.List.t -> Ppxlib__.Import.expression
val fun_ll_proj :
  loc:Ppxlib__.Import.location ->
  string Base.List.t -> Ppxlib__.Import.expression
val single_field_record :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration -> Ppxlib.expression
val mult_field_record :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration Base.List.t -> Ppxlib.expression