(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = int [@@deriving_inline encoding]

let _ = fun (_ : t) -> ()

let encoding =
  let open! Data_encoding in
  int31

let _ = encoding

[@@@end]

type t2 = string [@@deriving_inline encoding]

let _ = fun (_ : t2) -> ()

let encoding_of_t2 =
  let open! Data_encoding in
  string

let _ = encoding_of_t2

[@@@end]

type t3 = t [@@deriving_inline encoding]

let _ = fun (_ : t3) -> ()

let encoding_of_t3 =
  let open! Data_encoding in
  encoding

let _ = encoding_of_t3

[@@@end]

type t4 = t2 [@@deriving_inline encoding]

let _ = fun (_ : t4) -> ()
let encoding_of_t4 = let open! Data_encoding in encoding_of_t2
let _ = encoding_of_t4

[@@@end]

type t5 = int list [@@deriving_inline encoding]

let _ = fun (_ : t5) -> ()
let encoding_of_t5 = let open! Data_encoding in list int31
let _ = encoding_of_t5
[@@@end]


type t6 = t list [@@deriving_inline encoding]

let _ = fun (_ : t6) -> ()
let encoding_of_t6 = let open! Data_encoding in list encoding
let _ = encoding_of_t6

[@@@end]
