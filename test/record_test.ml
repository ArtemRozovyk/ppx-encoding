type single_record = { er : int } [@@deriving_inline encoding]

let _ = fun (_ : single_record) -> ()

let encoding_of_single_record =
  let open! Data_encoding in
  conv (fun { er } -> er) (fun er -> { er }) int31

let _ = encoding_of_single_record

[@@@end]

type variable_record2 = { p : int; q : string; b : bool; f : float }
[@@deriving_inline encoding]

let _ = fun (_ : variable_record2) -> ()

let encoding_of_variable_record2 =
  let open! Data_encoding in
  conv
    (fun { p; q; b; f } -> (p, q, b, f))
    (fun (p, q, b, f) -> { p; q; b; f })
    (obj4 (req "p" int31) (req "q" string) (req "b" bool) (req "f" float))

let _ = encoding_of_variable_record2

[@@@end]
