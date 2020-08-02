open Ppxlib
open Base

module T = Ppxlib.Ast_builder.Default


let name_of_type_name = function
  | "t" -> "encoding"
  | type_name -> "encoding_of_" ^ type_name


(* Construct an expression that represents the encoding of
 a core type (ast leafs) *)
let rec generate_encoding core_t =
  let loc = { core_t.ptyp_loc with loc_ghost = true } in
  match core_t.ptyp_desc with
  | Ptyp_tuple _ -> failwith "Not yet implemented."
  | Ptyp_constr ({ txt = Ldot (modules, typ); _ }, _) ->
      let ldot_type_enc_name = name_of_type_name typ in
      [%expr
        [%e T.pexp_ident ~loc { txt = Ldot (modules, ldot_type_enc_name); loc }]]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ tp ]) ->
      [%expr list [%e generate_encoding tp]]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> [%expr string]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr int31]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr float]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr bool]
  | Ptyp_constr ({ txt = Lident id; _ }, _) ->
      let type_enc_name = name_of_type_name id in
      [%expr [%e T.pexp_ident ~loc { txt = Lident type_enc_name; loc }]]
  | _ ->
      Location.raise_errorf ~loc
        " [Ppx_encoding] : generate_encoding -> Unsupported type"


let rec take_a n list tl =
  if n > 0 then
    match list with
    | [] -> failwith "Not enough elements in list"
    | x :: xs -> take_a (n - 1) xs (tl @ [ x ])
  else (tl, list)

(* split list into two lists (l1,l2) such that l1 contains 
n elements of the initial list and l2 containst the rets *)
let take_split n list = take_a n list []

let make_obj_arg ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ oct ]) -> ("opt", oct)
  | _ -> ("req", ct)

let objN_enc_from_ldl ~loc ldl =
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
          (Nolabel, generate_encoding ctd);
        ] )
  in
  let reqs = List.map ~f:to_req label_type_list in

  T.pexp_apply ~loc objN reqs

let rec make_obj_n ~loc ldl =
  let sz = List.length ldl in
  if sz <= 10 then objN_enc_from_ldl ~loc ldl
  else
    let l1, l2 = take_split (sz / 2) (* or 10 *) ldl in
    T.pexp_apply ~loc
      (T.pexp_ident ~loc { txt = Lident "merge_objs"; loc })
      [
        (Nolabel, [%expr [%e make_obj_n ~loc l1]]);
        (Nolabel, [%expr [%e make_obj_n ~loc l2]]);
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

  let single_field_record ~loc ld =
  let name = ld.pld_name.txt in
  let type_enc = generate_encoding ld.pld_type in
  let f1 = fun_record_from_name_inj ~loc name in
  let f2 = fun_record_from_name_proj ~loc name in
  [%expr conv [%e f1] [%e f2] [%e type_enc]]


  let mult_field_record ~loc ldl =
  let label_list = List.map ~f:(fun x -> x.pld_name.txt) ldl in
  let f1 = fun_ll_inj ~loc label_list in
  let f2 = fun_ll_proj ~loc label_list in
  let enc =
    if List.length ldl < 11 then objN_enc_from_ldl ~loc ldl
    else make_obj_n ~loc ldl
  in
  [%expr conv [%e f1] [%e f2] [%e enc]]