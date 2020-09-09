module T = Ppxlib.Ast_builder.Default

val name_of_type_name : Base.String.t -> Base.String.t

val generate_encoding : Ppxlib.core_type -> string option -> Ppxlib.expression


val apply_mu_op :
  loc:Ppxlib__.Import.location ->
  Base.String.t ->
  Ppxlib__.Import.expression ->
  Ppxlib.expression


val single_field_record :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration ->
  string option ->
  Ppxlib.expression

val mult_field_record :
  loc:Ppxlib__.Import.location ->
  Ppxlib.label_declaration Base.List.t ->
  string option ->
  Ppxlib.expression

val single_case_variant :
  loc:Ppxlib__.Import.location ->
  Ppxlib.constructor_declaration ->
  string option ->
  Ppxlib.expression

val generate_cases :
  loc:Ppxlib__.Import.location ->
  Ppxlib.constructor_declaration list ->
  string option ->
  Ppxlib.expression
