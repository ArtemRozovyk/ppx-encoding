type k = string list [@@deriving encoding]

type todo1 = F of float | T of k [@@deriving_inline encoding]

let _ = fun (_ : todo1) -> ()
let encoding_of_todo1 =
  let open! Data_encoding in
    union ~tag_size:`Uint8
      [case ~title:"F" (Tag 0) (obj1 (req "F" float))
         (function | F f -> Some f | _ -> None) (fun f -> F f);
      case ~title:"T" (Tag 1) (obj1 (req "T" encoding_of_k))
        (function | T t -> Some t | _ -> None) (fun t -> T t)]
let _ = encoding_of_todo1
[@@@end]

type var_type = A of int | B of string | C of int [@@deriving_inline encoding]

let _ = fun (_ : var_type) -> ()

let encoding_of_var_type =
  let open! Data_encoding in
    union ~tag_size:`Uint8
      [case ~title:"A" (Tag 0) (obj1 (req "A" int31))
         (function | A a -> Some a | _ -> None) (fun a -> A a);
      case ~title:"B" (Tag 1) (obj1 (req "B" string))
        (function | B b -> Some b | _ -> None) (fun b -> B b);
      case ~title:"C" (Tag 2) (obj1 (req "C" int31))
        (function | C c -> Some c | _ -> None) (fun c -> C c)]
let _ = encoding_of_var_type

[@@@end]

type t = string list [@@deriving encoding]


type t2 = string list [@@deriving encoding]

type var_type2 = D of t2 | E of int list | F [@@deriving_inline encoding]

let _ = fun (_ : var_type2) -> ()

let encoding_of_var_type2 =
  let open! Data_encoding in
    union ~tag_size:`Uint8
      [case ~title:"D" (Tag 0) (obj1 (req "D" encoding_of_t2))
         (function | D d -> Some d | _ -> None) (fun d -> D d);
      case ~title:"E" (Tag 1) (obj1 (req "E" (list int31)))
        (function | E e -> Some e | _ -> None) (fun e -> E e);
      case ~title:"F" (Tag 2) (obj1 (req "F" unit))
        (function | F -> Some () | _ -> None) (fun () -> F)]
let _ = encoding_of_var_type2

[@@@end]

type vars_mult_rec3 = Rz of { rlol : string } | Ka of int
[@@deriving_inline encoding]

let _ = fun (_ : vars_mult_rec3) -> ()

let encoding_of_vars_mult_rec3 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Rz" (Tag 0)
        (obj1 (req "Rz" (obj1 (req "rlol" string))))
        (function Rz { rlol } -> Some rlol | _ -> None)
        (fun rlol -> Rz { rlol });
      case ~title:"Ka" (Tag 1)
        (obj1 (req "Ka" int31))
        (function Ka ka -> Some ka | _ -> None)
        (fun ka -> Ka ka);
    ]

let _ = encoding_of_vars_mult_rec3

[@@@end]

type vars_mult_rec4 = Ze of { e : int; z : string } | Bloblo
[@@deriving_inline encoding]

let _ = fun (_ : vars_mult_rec4) -> ()

let encoding_of_vars_mult_rec4 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Ze" (Tag 0)
        (obj1 (req "Ze" (obj2 (req "e" int31) (req "z" string))))
        (function Ze { e; z } -> Some (e, z) | _ -> None)
        (fun (e, z) -> Ze { e; z });
      case ~title:"Bloblo" (Tag 1)
        (obj1 (req "Bloblo" unit))
        (function Bloblo -> Some () | _ -> None)
        (fun () -> Bloblo);
    ]

let _ = encoding_of_vars_mult_rec4

[@@@end]

type ke = string list [@@deriving encoding]

type todo12 =
  | F of float
  | Sq of { lol : ke }
  | Di of { carpio : ke; la : int }
[@@deriving_inline encoding]

let _ = fun (_ : todo12) -> ()

let encoding_of_todo12 =
  let open! Data_encoding in
    union ~tag_size:`Uint8
      [case ~title:"F" (Tag 0) (obj1 (req "F" float))
         (function | F f -> Some f | _ -> None) (fun f -> F f);
      case ~title:"Sq" (Tag 1)
        (obj1 (req "Sq" (obj1 (req "lol" encoding_of_ke))))
        (function | Sq { lol } -> Some lol | _ -> None)
        (fun lol -> Sq { lol });
      case ~title:"Di" (Tag 2)
        (obj1
           (req "Di" (obj2 (req "carpio" encoding_of_ke) (req "la" int31))))
        (function | Di { carpio; la } -> Some (carpio, la) | _ -> None)
        (fun (carpio, la) -> Di { carpio; la })]

let _ = encoding_of_todo12

[@@@end]

type inline_rec_type0 =
  | Rqx of {
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
  | Nvq
[@@deriving_inline encoding]

let _ = fun (_ : inline_rec_type0) -> ()

let encoding_of_inline_rec_type0 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Rqx" (Tag 0)
        (obj1
           (req "Rqx"
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
                       (req "a20" int31) (req "a21" int31) (req "a22" int31))))))
        (function
          | Rqx
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
              } ->
              Some
                ( ((a0, a1, a2, a3, a4), (a5, a6, a7, a8, a9, a10)),
                  ( (a11, a12, a13, a14, a15, a16),
                    (a17, a18, a19, a20, a21, a22) ) )
          | _ -> None)
        (fun ( ((a0, a1, a2, a3, a4), (a5, a6, a7, a8, a9, a10)),
               ((a11, a12, a13, a14, a15, a16), (a17, a18, a19, a20, a21, a22))
             ) ->
          Rqx
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
            });
      case ~title:"Nvq" (Tag 1)
        (obj1 (req "Nvq" unit))
        (function Nvq -> Some () | _ -> None)
        (fun () -> Nvq);
    ]

let _ = encoding_of_inline_rec_type0

[@@@end]
