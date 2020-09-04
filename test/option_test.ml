type option0 = int option [@@deriving_inline encoding]

let _ = fun (_ : option0) -> ()

let encoding_of_option0 =
  let open! Data_encoding in
  option int31

let _ = encoding_of_option0

[@@@end]

type option1 =
  | Opt of {
      a : int option;
      lek : string;
      kn : string option list option option;
    }
  | BliBli
[@@deriving_inline encoding]

let _ = fun (_ : option1) -> ()

let encoding_of_option1 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Opt" (Tag 0)
        (obj1
           (req "Opt"
              (obj3 (opt "a" int31) (req "lek" string)
                 (opt "kn" (option (list (option string)))))))
        (function Opt { a; lek; kn } -> Some (a, lek, kn) | _ -> None)
        (fun (a, lek, kn) -> Opt { a; lek; kn });
      case ~title:"BliBli" (Tag 1)
        (obj1 (req "BliBli" unit))
        (function BliBli -> Some () | _ -> None)
        (fun () -> BliBli);
    ]

let _ = encoding_of_option1

[@@@end]

type option2 = Opt2 of int option | BlaBle [@@deriving_inline encoding]

let _ = fun (_ : option2) -> ()

let encoding_of_option2 =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Opt2" (Tag 0)
        (obj1 (req "Opt2" (option int31)))
        (function Opt2 opt2 -> Some opt2 | _ -> None)
        (fun opt2 -> Opt2 opt2);
      case ~title:"BlaBle" (Tag 1)
        (obj1 (req "BlaBle" unit))
        (function BlaBle -> Some () | _ -> None)
        (fun () -> BlaBle);
    ]

let _ = encoding_of_option2

[@@@end]
