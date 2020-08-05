type typ_type0 = int * int * int * int * int * int [@@deriving_inline encoding]

let _ = fun (_ : typ_type0) -> ()

let encoding_of_typ_type0 =
  let open! Data_encoding in
  tup6 int31 int31 int31 int31 int31 int31

let _ = encoding_of_typ_type0

[@@@end]

type tup_type1 = int * int * int * int * int * int * int * int * int * int
[@@deriving_inline encoding]

let _ = fun (_ : tup_type1) -> ()

let encoding_of_tup_type1 =
  let open! Data_encoding in
  tup10 int31 int31 int31 int31 int31 int31 int31 int31 int31 int31

let _ = encoding_of_tup_type1

[@@@end]

type tup_type2 =
  int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
  * int
[@@deriving_inline encoding]

let _ = fun (_ : tup_type2) -> ()

let encoding_of_tup_type2 =
  let open! Data_encoding in
  conv
    (fun ( v0,
           v1,
           v2,
           v3,
           v4,
           v5,
           v6,
           v7,
           v8,
           v9,
           v10,
           v11,
           v12,
           v13,
           v14,
           v15,
           v16,
           v17,
           v18,
           v19,
           v20,
           v21,
           v22 ) ->
      ( ((v0, v1, v2, v3, v4), (v5, v6, v7, v8, v9, v10)),
        ((v11, v12, v13, v14, v15, v16), (v17, v18, v19, v20, v21, v22)) ))
    (fun ( ((v0, v1, v2, v3, v4), (v5, v6, v7, v8, v9, v10)),
           ((v11, v12, v13, v14, v15, v16), (v17, v18, v19, v20, v21, v22)) ) ->
      ( v0,
        v1,
        v2,
        v3,
        v4,
        v5,
        v6,
        v7,
        v8,
        v9,
        v10,
        v11,
        v12,
        v13,
        v14,
        v15,
        v16,
        v17,
        v18,
        v19,
        v20,
        v21,
        v22 ))
    (merge_tups
       (merge_tups
          (tup5 int31 int31 int31 int31 int31)
          (tup6 int31 int31 int31 int31 int31 int31))
       (merge_tups
          (tup6 int31 int31 int31 int31 int31 int31)
          (tup6 int31 int31 int31 int31 int31 int31)))

let _ = encoding_of_tup_type2

[@@@end]

type ke = (string * int) list [@@deriving encoding]

type todo1 =
  | R of int * bool
  | L of (int * int * int)
  | Di of { carpio : ke; la : int * int }
[@@deriving_inline encoding]

let _ = fun (_ : todo1) -> ()

let encoding_of_todo1 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"R" (Tag 0)
        (obj1 (req "R" (tup2 int31 bool)))
        (function R (r0, r1) -> Some (r0, r1) | _ -> None)
        (fun (r0, r1) -> R (r0, r1));
      case ~title:"L" (Tag 1)
        (obj1 (req "L" (tup3 int31 int31 int31)))
        (function L l -> Some l | _ -> None)
        (fun l -> L l);
      case ~title:"Di" (Tag 2)
        (obj1
           (req "Di"
              (obj2 (req "carpio" encoding_of_ke) (req "la" (tup2 int31 int31)))))
        (function Di { carpio; la } -> Some (carpio, la) | _ -> None)
        (fun (carpio, la) -> Di { carpio; la });
    ]

let _ = encoding_of_todo1

[@@@end]

type mult_var_with_mult_tups =
  | Koko of
      int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
  | Nk
[@@deriving_inline encoding]

let _ = fun (_ : mult_var_with_mult_tups) -> ()

let encoding_of_mult_var_with_mult_tups =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Koko" (Tag 0)
        (obj1
           (req "Koko"
              (merge_tups
                 (tup6 int31 int31 int31 int31 int31 int31)
                 (tup7 int31 int31 int31 int31 int31 int31 int31))))
        (function
          | Koko
              ( koko0,
                koko1,
                koko2,
                koko3,
                koko4,
                koko5,
                koko6,
                koko7,
                koko8,
                koko9,
                koko10,
                koko11,
                koko12 ) ->
              Some
                ( (koko0, koko1, koko2, koko3, koko4, koko5),
                  (koko6, koko7, koko8, koko9, koko10, koko11, koko12) )
          | _ -> None)
        (fun ( (koko0, koko1, koko2, koko3, koko4, koko5),
               (koko6, koko7, koko8, koko9, koko10, koko11, koko12) ) ->
          Koko
            ( koko0,
              koko1,
              koko2,
              koko3,
              koko4,
              koko5,
              koko6,
              koko7,
              koko8,
              koko9,
              koko10,
              koko11,
              koko12 ));
      case ~title:"Nk" (Tag 1)
        (obj1 (req "Nk" unit))
        (function Nk -> Some () | _ -> None)
        (fun () -> Nk);
    ]

let _ = encoding_of_mult_var_with_mult_tups

[@@@end]
