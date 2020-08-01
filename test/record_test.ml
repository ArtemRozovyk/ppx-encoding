type single_record = { er : int } [@@deriving_inline encoding]


let _ = fun (_ : single_record) -> ()
let encoding_of_single_record =
  let open! Data_encoding in conv (fun { er } -> er) (fun er -> { er }) int31
let _ = encoding_of_single_record
[@@@end]

