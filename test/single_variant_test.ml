type t = To[@@deriving_inline encoding]

let _ = fun (_ : t) -> ()
let encoding =
  let open! Data_encoding in
    conv (fun (To) -> ()) (fun () -> To) (obj1 (req "To" unit))
let _ = encoding
[@@@end]


type t1=  Ti of int [@@deriving_inline encoding]

let _ = fun (_ : t1) -> ()
let encoding_of_t1 =
  let open! Data_encoding in
    conv (fun (Ti ti) -> ti) (fun ti -> Ti ti) (obj1 (req "Ti" int31))
let _ = encoding_of_t1
[@@@end]


type t2 = Btt of int * int [@@deriving_inline encoding]
let _ = fun (_ : t2) -> ()
let encoding_of_t2 =
  let open! Data_encoding in
    conv (fun (Btt (btt0, btt1)) -> (btt0, btt1))
      (fun (btt0, btt1) -> Btt (btt0, btt1))
      (obj1 (req "Btt" (tup2 int31 int31)))
let _ = encoding_of_t2
[@@@end]

type tup_type_variant4 =
  | Btt of int * int * int * int * int * int * int * int * int * int * int
   * int * int * int * int * int * int [@@deriving_inline encoding] 

let _ = fun (_ : tup_type_variant4) -> ()
let encoding_of_tup_type_variant4 =
  let open! Data_encoding in
    conv
      (fun (Btt
         (btt0, btt1, btt2, btt3, btt4, btt5, btt6, btt7, btt8, btt9, btt10,
          btt11, btt12, btt13, btt14, btt15, btt16))
         ->
         ((btt0, btt1, btt2, btt3, btt4, btt5, btt6, btt7),
           (btt8, btt9, btt10, btt11, btt12, btt13, btt14, btt15, btt16)))
      (fun
         ((btt0, btt1, btt2, btt3, btt4, btt5, btt6, btt7),
          (btt8, btt9, btt10, btt11, btt12, btt13, btt14, btt15, btt16))
         ->
         Btt
           (btt0, btt1, btt2, btt3, btt4, btt5, btt6, btt7, btt8, btt9,
             btt10, btt11, btt12, btt13, btt14, btt15, btt16))
      (obj1
         (req "Btt"
            (merge_tups
               (tup8 int31 int31 int31 int31 int31 int31 int31 int31)
               (tup9 int31 int31 int31 int31 int31 int31 int31 int31 int31))))
let _ = encoding_of_tup_type_variant4
[@@@end]

