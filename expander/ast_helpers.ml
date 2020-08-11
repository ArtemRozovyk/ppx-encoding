open Ppxlib
open Base
module T = Ppxlib.Ast_builder.Default

let name_of_type_name = function
  | "t" -> "encoding"
  | type_name -> "encoding_of_" ^ type_name

let rec take_a n list tl =
  if n > 0 then
    match list with
    | [] -> failwith "Not enough elements in list"
    | x :: xs -> take_a (n - 1) xs (tl @ [ x ])
  else (tl, list)

(* split list into two lists (l1,l2) such that l1 contains 
n elements of the initial list and l2 containst the rets *)
let take_split n list = take_a n list []

let rec make_nested_ppat_tuple ~loc ctl =
  (* to call with numbered label list*)
  let l1, l2 = take_split (List.length ctl / 2) ctl in
  let make_arg g =
    if List.length g < 11 then
      T.ppat_tuple ~loc
        (List.map ~f:(fun l -> T.ppat_var ~loc { txt = l; loc }) g)
    else make_nested_ppat_tuple ~loc g
  in
  let arg1 = make_arg l1 in
  let arg2 = make_arg l2 in
  T.ppat_tuple ~loc [ arg1; arg2 ]

(* make tuple expression from label list*)
let rec make_nested_pexp_tuple ~loc ctl =
  (* to call with numbered label list*)
  let l1, l2 = take_split (List.length ctl / 2) ctl in
  let make_arg g =
    if List.length g < 11 then
      T.pexp_tuple ~loc
        (List.map ~f:(fun l -> T.pexp_ident ~loc { txt = Lident l; loc }) g)
    else make_nested_pexp_tuple ~loc g
  in
  let arg1 = make_arg l1 in
  let arg2 = make_arg l2 in
  T.pexp_tuple ~loc [ arg1; arg2 ]

let fun_tuple_inj ~loc ctl =
  let pat_var =
    if List.length ctl < 11 then
      T.pexp_tuple ~loc
        (List.mapi
           ~f:(fun i _ ->
             T.pexp_ident ~loc { txt = Lident ("v" ^ Int.to_string i); loc })
           ctl)
    else
      make_nested_pexp_tuple ~loc
        (List.mapi ~f:(fun i _ -> "v" ^ Int.to_string i) ctl)
  in
  let construct =
    T.ppat_tuple ~loc
      (List.mapi
         ~f:(fun i _ -> T.ppat_var ~loc { txt = "v" ^ Int.to_string i; loc })
         ctl)
  in
  T.pexp_fun ~loc Nolabel None construct pat_var

let fun_tuple_proj ~loc ctl =
  let pat_var =
    if List.length ctl < 11 then
      T.ppat_tuple ~loc
        (List.mapi
           ~f:(fun i _ -> T.ppat_var ~loc { txt = "v" ^ Int.to_string i; loc })
           ctl)
    else
      make_nested_ppat_tuple ~loc
        (List.mapi ~f:(fun i _ -> "v" ^ Int.to_string i) ctl)
  in
  let construct =
    T.pexp_tuple ~loc
      (List.mapi
         ~f:(fun i _ ->
           T.pexp_ident ~loc { txt = Lident ("v" ^ Int.to_string i); loc })
         ctl)
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

(* Construct an expression that represents the encoding of
 a core type (ast leafs) *)
let rec generate_encoding core_t r_name =
  let loc = { core_t.ptyp_loc with loc_ghost = true } in
  match core_t.ptyp_desc with
  | Ptyp_tuple ctl -> conv_tuples ~loc ctl r_name
  | Ptyp_constr ({ txt = Ldot (modules, typ); _ }, _) ->
      let ldot_type_enc_name = name_of_type_name typ in
      [%expr
        [%e T.pexp_ident ~loc { txt = Ldot (modules, ldot_type_enc_name); loc }]]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ tp ]) ->
      [%expr list [%e generate_encoding tp r_name]]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> [%expr string]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr int31]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr float]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr bool]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ ct ]) ->
      [%expr option [%e generate_encoding ct r_name]]
  | Ptyp_constr ({ txt = Lident id; _ }, _) ->
      let type_enc_name =
        match r_name with
        | Some rn when String.equal rn id -> id ^ "_encoding"
        | _ -> name_of_type_name id
      in
      [%expr [%e T.pexp_ident ~loc { txt = Lident type_enc_name; loc }]]
  | Ptyp_var name ->
      [%expr
        [%e T.pexp_ident ~loc { txt = Lident ("_" ^ name ^ "_encoding"); loc }]]
  | _ ->
      Location.raise_errorf ~loc
        " [Ppx_encoding] : generate_encoding -> Unsupported type"

and make_tup_n ~loc ctl r_name =
  let sz = List.length ctl in
  if sz <= 10 then
    T.pexp_apply ~loc
      (T.pexp_ident ~loc { txt = Lident ("tup" ^ Int.to_string sz); loc })
      (List.map ~f:(fun ct -> (Nolabel, generate_encoding ct r_name)) ctl)
  else
    let l1, l2 = take_split (sz / 2) (* or 10 *) ctl in
    T.pexp_apply ~loc
      (T.pexp_ident ~loc { txt = Lident "merge_tups"; loc })
      [
        (Nolabel, [%expr [%e make_tup_n ~loc l1 r_name]]);
        (Nolabel, [%expr [%e make_tup_n ~loc l2 r_name]]);
      ]

and conv_tuples ~loc ctl r_name =
  if List.length ctl < 11 then make_tup_n ~loc ctl r_name
  else
    let f1 = fun_tuple_inj ~loc ctl in
    let f2 = fun_tuple_proj ~loc ctl in
    let enc = make_tup_n ~loc ctl r_name in
    [%expr conv [%e f1] [%e f2] [%e enc]]

(*Detect if the field is optional then use "opt" instead of "req" *)
let make_obj_arg ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ oct ]) -> ("opt", oct)
  | _ -> ("req", ct)

let objN_enc_from_ldl ~loc ldl r_name=
  let label_list = List.map ~f:(fun x -> x.pld_name.txt) ldl in
  let core_type_list = List.map ~f:(fun x -> x.pld_type) ldl in
  let label_type_list = List.zip_exn label_list core_type_list in
  let objN =
    T.pexp_ident ~loc (* *)
      { txt = Lident ("obj" ^ Int.to_string (List.length ldl)); loc }
  in
  let to_req (lb, ct) =
    let optionality, ctd = make_obj_arg ct in
    ( Nolabel,
      T.pexp_apply ~loc
        (T.pexp_ident ~loc { txt = Lident optionality; loc })
        [
          (Nolabel, T.pexp_constant ~loc (Pconst_string (lb, None)));
          (Nolabel, generate_encoding ctd r_name);
        ] )
  in
  let reqs = List.map ~f:to_req label_type_list in

  T.pexp_apply ~loc objN reqs

let rec make_obj_n ~loc ldl r_name=
  let sz = List.length ldl in
  if sz <= 10 then objN_enc_from_ldl ~loc ldl r_name
  else
    let l1, l2 = take_split (sz / 2) (* or 10 *) ldl in
    T.pexp_apply ~loc
      (T.pexp_ident ~loc { txt = Lident "merge_objs"; loc })
      [
        (Nolabel, [%expr [%e make_obj_n ~loc l1 r_name]]);
        (Nolabel, [%expr [%e make_obj_n ~loc l2 r_name]]);
      ]

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

(* Applying the mu operation for a recurive type *)
let apply_mu_op ~loc name inter_expr =
  let pat_var = T.ppat_var ~loc { txt = name ^ "_encoding"; loc } in
  [%expr
    mu
      [%e T.pexp_constant ~loc (Pconst_string (name, None))]
      [%e T.pexp_fun ~loc Nolabel None pat_var inter_expr]]

(* make tuple pattern from label list*)

(* Multiple field record injection function *)
let fun_ll_inj ~loc lbl =
  let ppat_record =
    T.ppat_record ~loc
      (List.map
         ~f:(fun name ->
           ({ txt = Lident name; loc }, T.ppat_var ~loc { txt = name; loc }))
         lbl)
      Closed
  in
  let construct =
    if List.length lbl < 11 then
      T.pexp_tuple ~loc
        (List.map
           ~f:(fun name -> T.pexp_ident ~loc { txt = Lident name; loc })
           lbl)
    else make_nested_pexp_tuple ~loc lbl
  in
  T.pexp_fun ~loc Nolabel None ppat_record construct

(* Multiple field record projection function *)

let fun_ll_proj ~loc lbl =
  let pat_var =
    if List.length lbl < 11 then
      T.ppat_tuple ~loc
        (List.map ~f:(fun name -> T.ppat_var ~loc { txt = name; loc }) lbl)
    else make_nested_ppat_tuple ~loc lbl
  in
  let construct =
    T.pexp_record ~loc
      (List.map
         ~f:(fun name ->
           ( { txt = Lident name; loc },
             T.pexp_ident ~loc { txt = Lident name; loc } ))
         lbl)
      None
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let single_field_record ~loc ld r_name=
  let name = ld.pld_name.txt in
  let type_enc = generate_encoding ld.pld_type r_name in
  let f1 = fun_record_from_name_inj ~loc name in
  let f2 = fun_record_from_name_proj ~loc name in
  [%expr conv [%e f1] [%e f2] [%e type_enc]]

let mult_field_record ~loc ldl r_name =
  let label_list = List.map ~f:(fun x -> x.pld_name.txt) ldl in
  let f1 = fun_ll_inj ~loc label_list in
  let f2 = fun_ll_proj ~loc label_list in
  let enc =
    if List.length ldl < 11 then objN_enc_from_ldl ~loc ldl r_name
    else make_obj_n ~loc ldl r_name
  in
  [%expr conv [%e f1] [%e f2] [%e enc]]

let construct_fun_var_inj ~loc cd =
  let name = String.lowercase cd.pcd_name.txt in
  let pat_var = T.pexp_ident ~loc { txt = Lident name; loc } in
  let construct =
    T.ppat_construct ~loc
      { txt = Lident cd.pcd_name.txt; loc }
      (Some (T.ppat_var ~loc { txt = name; loc }))
  in
  T.pexp_fun ~loc Nolabel None construct pat_var

let construct_fun_unit_inj ~loc cd =
  let name = "()" in
  let pat_var =
    T.ppat_construct ~loc { txt = Lident cd.pcd_name.txt; loc } None
  in
  let construct = T.pexp_construct ~loc { txt = Lident name; loc } None in
  T.pexp_fun ~loc Nolabel None pat_var construct

let construct_fun_tuple_inj ~loc cd ctl =
  let name = String.lowercase cd.pcd_name.txt in
  let pat_var =
    if List.length ctl < 11 then
      T.pexp_tuple ~loc
        (List.mapi
           ~f:(fun i _ ->
             T.pexp_ident ~loc
               { txt = Lident (String.lowercase name ^ Int.to_string i); loc })
           ctl)
    else
      make_nested_pexp_tuple ~loc
        (List.mapi ~f:(fun i _ -> String.lowercase name ^ Int.to_string i) ctl)
  in
  let construct =
    T.ppat_construct ~loc
      { txt = Lident cd.pcd_name.txt; loc }
      (Some
         (T.ppat_tuple ~loc
            (List.mapi
               ~f:(fun i _ ->
                 T.ppat_var ~loc
                   { txt = String.lowercase name ^ Int.to_string i; loc })
               ctl)))
  in
  T.pexp_fun ~loc Nolabel None construct pat_var

let fun_from_constructor_inj ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> construct_fun_unit_inj ~loc cd
  | Pcstr_tuple [ _ ] -> construct_fun_var_inj ~loc cd
  | Pcstr_tuple ctl -> construct_fun_tuple_inj ~loc cd ctl
  | _ ->
      Location.raise_errorf ~loc
        "[Ppx_encoding] : enc_from_carg -> Records should not appear here %s"
        cd.pcd_name.txt

let construct_fun_tuple_proj ~loc cd ctl =
  let name = String.lowercase cd.pcd_name.txt in
  let pat_var =
    if List.length ctl < 11 then
      T.ppat_tuple ~loc
        (List.mapi
           ~f:(fun i _ ->
             T.ppat_var ~loc
               { txt = String.lowercase name ^ Int.to_string i; loc })
           ctl)
    else
      make_nested_ppat_tuple ~loc
        (List.mapi ~f:(fun i _ -> String.lowercase name ^ Int.to_string i) ctl)
  in
  let construct =
    T.pexp_construct ~loc
      { txt = Lident cd.pcd_name.txt; loc }
      (Some
         (T.pexp_tuple ~loc
            (List.mapi
               ~f:(fun i _ ->
                 T.pexp_ident ~loc
                   {
                     txt = Lident (String.lowercase name ^ Int.to_string i);
                     loc;
                   })
               ctl)))
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let construct_fun_var_proj ~loc cd =
  let name = String.lowercase cd.pcd_name.txt in
  let pat_var = T.ppat_var ~loc { txt = name; loc } in
  let construct =
    T.pexp_construct ~loc
      { txt = Lident cd.pcd_name.txt; loc }
      (Some (T.pexp_ident ~loc { txt = Lident name; loc }))
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let construct_fun_unit_proj ~loc cd =
  let name = "()" in
  let pat_var = T.ppat_construct ~loc { txt = Lident name; loc } None in
  let construct =
    T.pexp_construct ~loc { txt = Lident cd.pcd_name.txt; loc } None
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let fun_from_constructor_proj ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> construct_fun_unit_proj ~loc cd
  | Pcstr_tuple [ _ ] -> construct_fun_var_proj ~loc cd
  | Pcstr_tuple ctl -> construct_fun_tuple_proj ~loc cd ctl
  | _ ->
      Location.raise_errorf ~loc
        "[Ppx_encoding] : enc_from_carg -> Records should not appear here %s"
        cd.pcd_name.txt

let object1 ~loc enc cname =
  [%expr
    obj1 (req [%e T.pexp_constant ~loc (Pconst_string (cname, None))] [%e enc])]

let enc_from_carg ~loc carg cname r_name=
  match carg with
  | Pcstr_tuple [ ct ] -> object1 ~loc (generate_encoding ct r_name) cname
  | Pcstr_tuple [] -> object1 ~loc [%expr unit] cname
  | Pcstr_tuple ctl -> object1 ~loc [%expr [%e make_tup_n ~loc ctl r_name]] cname
  | Pcstr_record _ ->
      Location.raise_errorf
        "[Ppx_encoding] : enc_from_carg -> Records should not appear here %s, "
        cname

let encode_variant_tuple_conv ~loc cd r_name=
  let f1 = fun_from_constructor_inj ~loc cd in
  let f2 = fun_from_constructor_proj ~loc cd in
  [%expr
    conv [%e f1] [%e f2] [%e enc_from_carg ~loc cd.pcd_args cd.pcd_name.txt r_name]]

let variant_inline_record_conv ~loc _ldl cd r_name=
  let label_list = List.map ~f:(fun x -> x.pld_name.txt) _ldl in
  let name = cd.pcd_name.txt in
  let constr =
    T.ppat_construct ~loc { txt = Lident name; loc }
      (Some
         (T.ppat_record ~loc
            (List.map
               ~f:(fun x ->
                 ({ txt = Lident x; loc }, T.ppat_var ~loc { txt = x; loc }))
               label_list)
            Closed))
  in
  let f1 =
    T.pexp_fun ~loc Nolabel None constr
      (*if multiple ld then tuple *)
      ( if List.length label_list = 1 then
        T.pexp_ident ~loc { txt = Lident (List.hd_exn label_list); loc }
      else if List.length label_list < 11 then
        T.pexp_tuple ~loc
          (List.map
             ~f:(fun name -> T.pexp_ident ~loc { txt = Lident name; loc })
             label_list)
      else make_nested_pexp_tuple ~loc label_list )
  in
  let constr2 =
    (*if multiple ld then tuple *)
    if List.length label_list = 1 then
      T.ppat_var ~loc { txt = List.hd_exn label_list; loc }
    else if List.length _ldl < 11 then
      T.ppat_tuple ~loc
        (List.map
           ~f:(fun name -> T.ppat_var ~loc { txt = name; loc })
           label_list)
    else make_nested_ppat_tuple ~loc label_list
  in
  let f2 =
    T.pexp_fun ~loc Nolabel None constr2
      (T.pexp_construct ~loc { txt = Lident name; loc }
         (Some
            (T.pexp_record ~loc
               (List.map
                  ~f:(fun x ->
                    ( { txt = Lident x; loc },
                      T.pexp_ident ~loc { txt = Lident x; loc } ))
                  label_list)
               None)))
  in
  [%expr
    conv [%e f1] [%e f2]
      [%e
        if List.length _ldl < 11 then objN_enc_from_ldl ~loc _ldl r_name
        else make_obj_n ~loc _ldl r_name]]

let case_from_constructor_decl1_tuple ~loc cd ctl =
  let name = cd.pcd_name.txt in
  let lhs =
    T.ppat_construct ~loc { txt = Lident name; loc }
      (Some
         (T.ppat_tuple ~loc
            (List.mapi
               ~f:(fun i _ ->
                 T.ppat_var ~loc
                   { txt = String.lowercase name ^ Int.to_string i; loc })
               ctl)))
  in
  let name = String.lowercase name in
  let rhs =
    T.pexp_construct ~loc
      { txt = Lident "Some"; loc }
      (Some
         ( if List.length ctl < 11 then
           T.pexp_tuple ~loc
             (List.mapi
                ~f:(fun i _ ->
                  T.pexp_ident ~loc
                    {
                      txt = Lident (String.lowercase name ^ Int.to_string i);
                      loc;
                    })
                ctl)
         else
           make_nested_pexp_tuple ~loc
             (List.mapi
                ~f:(fun i _ -> String.lowercase name ^ Int.to_string i)
                ctl) ))
  in
  T.case ~lhs ~guard:None ~rhs

let single_case_variant ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple _ -> encode_variant_tuple_conv ~loc cd
  | Pcstr_record ldl -> variant_inline_record_conv ~loc ldl cd

let case_from_constructor_decl2 ~loc =
  let lhs = T.ppat_any ~loc in
  let rhs = T.pexp_construct ~loc { txt = Lident "None"; loc } None in
  T.case ~lhs ~guard:None ~rhs

let case_from_constructor_decl1_unit ~loc name =
  let lhs = T.ppat_construct ~loc { txt = Lident name; loc } None in

  let name = "()" in
  let rhs =
    T.pexp_construct ~loc
      { txt = Lident "Some"; loc }
      (Some (T.pexp_construct ~loc { txt = Lident name; loc } None))
  in
  T.case ~lhs ~guard:None ~rhs

let make_cases_unit ~loc cd =
  let cname = cd.pcd_name.txt in
  let case = [ case_from_constructor_decl1_unit ~loc cname ] in
  let case = case @ [ case_from_constructor_decl2 ~loc ] in
  T.pexp_function ~loc case

let case_from_constructor_decl1_var ~loc cd =
  let name = cd.pcd_name.txt in
  let lhs =
    T.ppat_construct ~loc { txt = Lident name; loc }
      (Some (T.ppat_var ~loc { txt = String.lowercase name; loc }))
  in
  let name = String.lowercase name in
  let rhs =
    T.pexp_construct ~loc
      { txt = Lident "Some"; loc }
      (Some (T.pexp_ident ~loc { txt = Lident name; loc }))
  in
  T.case ~lhs ~guard:None ~rhs

let case_from_cdl ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [ _ ] -> case_from_constructor_decl1_var ~loc cd
  | Pcstr_tuple ctl -> case_from_constructor_decl1_tuple ~loc cd ctl
  | _ ->
      Location.raise_errorf ~loc
        "[Ppx_encoding] : case_from_cdl ->Error generating argument type of %s"
        cd.pcd_name.txt

let make_cases ~loc cd =
  let case = [ case_from_cdl ~loc cd ] in
  let case = case @ [ case_from_constructor_decl2 ~loc ] in
  T.pexp_function ~loc case

let function_from_constructor ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> make_cases_unit ~loc cd
  | Pcstr_tuple _ -> make_cases ~loc cd
  | _ -> make_cases ~loc cd

let generate_tuple_case ~loc h n r_name=
  let f1 = function_from_constructor ~loc h in
  let f2 = fun_from_constructor_proj ~loc h in
  let tag_id = T.pexp_constant ~loc (Pconst_integer (Int.to_string n, None)) in
  let inner_encoding = enc_from_carg ~loc h.pcd_args h.pcd_name.txt r_name in
  [%expr
    case
      ~title:[%e T.pexp_constant ~loc (Pconst_string (h.pcd_name.txt, None))]
      (Tag [%e tag_id])
      [%e inner_encoding] [%e f1] [%e f2]]

let fun_ll_record_dec ~loc lbl cstr_name =
  let pat_var =
    if List.length lbl < 11 then
      T.ppat_tuple ~loc
        (List.map ~f:(fun name -> T.ppat_var ~loc { txt = name; loc }) lbl)
    else make_nested_ppat_tuple ~loc lbl
  in
  let construct =
    T.pexp_construct ~loc
      { txt = Lident cstr_name; loc }
      (Some
         (T.pexp_record ~loc
            (List.map
               ~f:(fun name ->
                 ( { txt = Lident name; loc },
                   T.pexp_ident ~loc { txt = Lident name; loc } ))
               lbl)
            None))
  in
  T.pexp_fun ~loc Nolabel None pat_var construct

let record_case_from_constructor_decl1 ~loc lbl name =
  let lhs =
    T.ppat_construct ~loc { txt = Lident name; loc }
      (Some
         (T.ppat_record ~loc
            (List.map
               ~f:(fun x ->
                 ({ txt = Lident x; loc }, T.ppat_var ~loc { txt = x; loc }))
               lbl)
            Closed))
  in
  let rhs =
    T.pexp_construct ~loc
      { txt = Lident "Some"; loc }
      (Some
         ( if List.length lbl < 11 then
           T.pexp_tuple ~loc
             (List.map
                ~f:(fun x -> T.pexp_ident ~loc { txt = Lident x; loc })
                lbl)
         else make_nested_pexp_tuple ~loc lbl ))
  in
  T.case ~lhs ~guard:None ~rhs

let function_record_enc ~loc lbl cname =
  let case = [ record_case_from_constructor_decl1 ~loc lbl cname ] in
  let case = case @ [ case_from_constructor_decl2 ~loc ] in
  T.pexp_function ~loc case

let generate_record_case ~loc ldl n cname r_name =
  let label_list = List.map ~f:(fun x -> x.pld_name.txt) ldl in
  let f1 = function_record_enc ~loc label_list cname in
  let f2 = fun_ll_record_dec ~loc label_list cname in
  let tag_id = T.pexp_constant ~loc (Pconst_integer (Int.to_string n, None)) in
  [%expr
    case
      ~title:[%e T.pexp_constant ~loc (Pconst_string (cname, None))]
      (Tag [%e tag_id])
      [%e
        object1 ~loc
          ( if List.length ldl < 11 then objN_enc_from_ldl ~loc ldl r_name
          else make_obj_n ~loc ldl r_name)
          cname]
      [%e f1] [%e f2]]

let rec generate_cases ~loc cdl n r_name =
  match cdl with
  | [] -> []
  | h :: t -> (
      match h.pcd_args with
      | Pcstr_tuple _ ->
          generate_tuple_case ~loc h n r_name :: generate_cases ~loc t (n + 1)  r_name
      | Pcstr_record lbl ->
          generate_record_case ~loc lbl n h.pcd_name.txt r_name
          :: generate_cases ~loc t (n + 1) r_name) 

let generate_cases ~loc cdl  r_name =
  [%expr union ~tag_size:`Uint8 [%e T.elist ~loc (generate_cases ~loc cdl 0 r_name)]]
