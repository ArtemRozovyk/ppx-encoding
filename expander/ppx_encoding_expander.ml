(*****************************************************************************)
(*                                                                           *)
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


let get_params_from_td td =
  let var_core_type_name_exn ct =
    match ct with
    | Ptyp_var label -> label
    | _ ->
        Location.raise_errorf
          "[Ppx_encoding] : get_params_from_td -> Illegal parameter"
  in
  match td.ptype_params with
  | [] -> []
  | l -> List.map ~f:(fun (ct, _) -> var_core_type_name_exn ct.ptyp_desc) l



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
        | Some tp -> A.generate_encoding tp )
    | Ptype_variant [] ->
        Location.raise_errorf ~loc
          "[Ppx_encoding] : encode_td -> Encoding cannot be derived for an \
           empty variant"
    | Ptype_variant [ cd ] -> A.single_case_variant ~loc cd
    | Ptype_variant cdl -> A.generate_cases ~loc cdl 
    | Ptype_record [] -> [%expr []]
    | Ptype_record [ ld ] -> A.single_field_record ~loc ld
    | Ptype_record ldl -> A.mult_field_record ~loc ldl
    | Ptype_open ->
        Location.raise_errorf ~loc
          "[Ppx_encoding] : encode_td -> Encoding cannot be derived for open \
           types"
  in
  let name = A.name_of_type_name td.ptype_name.txt in
  let body = eabstract ~loc (List.map
         ~f:(fun lab -> T.ppat_var ~loc { txt = "_" ^ lab ^ "_encoding"; loc })
         (get_params_from_td td)) encode in
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
