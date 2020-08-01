(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Base
open Ppxlib
open Ast_builder.Default
module T = Ppxlib.Ast_builder.Default
module A = Ast_helpers

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

let single_field_record ~loc ld =
  let name = ld.pld_name.txt in
  let type_enc = generate_encoding ld.pld_type in
  let f1 = A.fun_record_from_name_inj ~loc name in
  let f2 = A.fun_record_from_name_proj ~loc name in
  [%expr conv [%e f1] [%e f2] [%e type_enc]]

let generate_record_enc ~loc ldl =
  match ldl with
  | [] -> [%expr []]
  | [ ld ] -> single_field_record ~loc ld
  | _ -> failwith "Not yet implemented."

(* Construct an object item that represents the encoding of
 a type based on its kind  *)
let encode_td td =
  let td = name_type_params_in_td td in
  let loc = td.ptype_loc in
  let encode =
    match td.ptype_kind with
    | Ptype_abstract -> (
        match td.ptype_manifest with
        | None ->
            Location.raise_errorf ~loc
              "[Ppx_encoding] : encode_td -> Encoding cannot be derived for \
               fully abstract types"
        | Some tp -> generate_encoding tp )
    | Ptype_variant [] ->
        Location.raise_errorf ~loc
          "[Ppx_encoding] : encode_td -> Encoding cannot be derived for an \
           empty variant"
    | Ptype_variant [ _ ] -> failwith "Not yet implemented."
    | Ptype_variant _ -> failwith "Not yet implemented."
    | Ptype_record ldl -> generate_record_enc ~loc ldl
    | Ptype_open -> failwith "Not yet implemented."
  in
  let name = name_of_type_name td.ptype_name.txt in

  let body = eabstract ~loc [] encode in
  [%str
    let [%p pvar ~loc name] =
      let open! Data_encoding in
      [%e body]]

(* Entry point of the deriver*)
let str_type_decl ~loc ~path:_ (_rf, tds) =
  match tds with
  | [ td ] -> encode_td td
  | _ ->
      Location.raise_errorf ~loc
        "[Ppx_encoding] : str_type_decl -> Deriving only one type at a time is \
         supported by ppx_encoding"
