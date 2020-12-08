(* Inline records *)
type vars_rec_single = Ze of { e2 : int } [@@deriving_inline encoding]

let _ = fun (_ : vars_rec_single) -> ()

let encoding_of_vars_rec_single =
  let open! Data_encoding in
  conv (fun (Ze { e2 }) -> e2) (fun e2 -> Ze { e2 }) (obj1 (req "e2" int31))

let _ = encoding_of_vars_rec_single

[@@@end]

type vars_rec_mult_field = Ze of { e2 : int; ra : string }
[@@deriving_inline encoding]

let _ = fun (_ : vars_rec_mult_field) -> ()

let encoding_of_vars_rec_mult_field =
  let open! Data_encoding in
  conv
    (fun (Ze { e2; ra }) -> (e2, ra))
    (fun (e2, ra) -> Ze { e2; ra })
    (obj2 (req "e2" int31) (req "ra" string))

let _ = encoding_of_vars_rec_mult_field

[@@@end]

type inline_rec_type =
  | Rqe of {
      a0 : int;
      a1 : int;
      a2 : int;
      a3 : int;
      a4 : int;
      a5 : int;
      a6 : int;
      a7 : int;
      a8 : int;
      a9 : int;
      a10 : int;
      a11 : int;
      a12 : int;
      a13 : int;
      a14 : int;
      a15 : int;
      a16 : int;
      a17 : int;
      a18 : int;
      a19 : int;
      a20 : int;
      a21 : int;
      a22 : int;
    }
[@@deriving_inline encoding]

let _ = fun (_ : inline_rec_type) -> ()

let encoding_of_inline_rec_type =
  let open! Data_encoding in
  conv
    (fun (Rqe
           {
             a0;
             a1;
             a2;
             a3;
             a4;
             a5;
             a6;
             a7;
             a8;
             a9;
             a10;
             a11;
             a12;
             a13;
             a14;
             a15;
             a16;
             a17;
             a18;
             a19;
             a20;
             a21;
             a22;
           }) ->
      ( ((a0, a1, a2, a3, a4), (a5, a6, a7, a8, a9, a10)),
        ((a11, a12, a13, a14, a15, a16), (a17, a18, a19, a20, a21, a22)) ))
    (fun ( ((a0, a1, a2, a3, a4), (a5, a6, a7, a8, a9, a10)),
           ((a11, a12, a13, a14, a15, a16), (a17, a18, a19, a20, a21, a22)) ) ->
      Rqe
        {
          a0;
          a1;
          a2;
          a3;
          a4;
          a5;
          a6;
          a7;
          a8;
          a9;
          a10;
          a11;
          a12;
          a13;
          a14;
          a15;
          a16;
          a17;
          a18;
          a19;
          a20;
          a21;
          a22;
        })
    (merge_objs
       (merge_objs
          (obj5 (req "a0" int31) (req "a1" int31) (req "a2" int31)
             (req "a3" int31) (req "a4" int31))
          (obj6 (req "a5" int31) (req "a6" int31) (req "a7" int31)
             (req "a8" int31) (req "a9" int31) (req "a10" int31)))
       (merge_objs
          (obj6 (req "a11" int31) (req "a12" int31) (req "a13" int31)
             (req "a14" int31) (req "a15" int31) (req "a16" int31))
          (obj6 (req "a17" int31) (req "a18" int31) (req "a19" int31)
             (req "a20" int31) (req "a21" int31) (req "a22" int31))))

let _ = encoding_of_inline_rec_type

[@@@end]
