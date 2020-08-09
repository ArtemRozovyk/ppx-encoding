type 'a t = M | T of string | X of 'a | P of 'a * 'a 
[@@deriving_inline encoding]

let _ = fun (_ : 'a t) -> ()
let encoding =
  let open! Data_encoding in
    fun _a_encoding ->
      union ~tag_size:`Uint8
        [case ~title:"M" (Tag 0) (obj1 (req "M" unit))
           (function | M -> Some () | _ -> None) (fun () -> M);
        case ~title:"T" (Tag 1) (obj1 (req "T" string))
          (function | T t -> Some t | _ -> None) (fun t -> T t);
        case ~title:"X" (Tag 2) (obj1 (req "X" _a_encoding))
          (function | X x -> Some x | _ -> None) (fun x -> X x);
        case ~title:"P" (Tag 3)
          (obj1 (req "P" (tup2 _a_encoding _a_encoding)))
          (function | P (p0, p1) -> Some (p0, p1) | _ -> None)
          (fun (p0, p1) -> P (p0, p1))]
let _ = encoding
[@@@end]


type ('a, 'b) par_type2 = Ne of 'b | Le of 'a * 'b | C of 'a * 'b * 'a
[@@deriving_inline encoding]

let _ = fun (_ : ('a, 'b) par_type2) -> ()

let encoding_of_par_type2 =
  let open! Data_encoding in
    fun _a_encoding ->
      fun _b_encoding ->
        union ~tag_size:`Uint8
          [case ~title:"Ne" (Tag 0) (obj1 (req "Ne" _b_encoding))
             (function | Ne ne -> Some ne | _ -> None) (fun ne -> Ne ne);
          case ~title:"Le" (Tag 1)
            (obj1 (req "Le" (tup2 _a_encoding _b_encoding)))
            (function | Le (le0, le1) -> Some (le0, le1) | _ -> None)
            (fun (le0, le1) -> Le (le0, le1));
          case ~title:"C" (Tag 2)
            (obj1 (req "C" (tup3 _a_encoding _b_encoding _a_encoding)))
            (function | C (c0, c1, c2) -> Some (c0, c1, c2) | _ -> None)
            (fun (c0, c1, c2) -> C (c0, c1, c2))]
let _ = encoding_of_par_type2

[@@@end]

type ('a, 'b, 'c) par_type2 = K of 'a * 'b * 'c 
[@@deriving_inline encoding]

let _ = fun (_ : ('a, 'b, 'c) par_type2) -> ()
let encoding_of_par_type2 =
  let open! Data_encoding in
    fun _a_encoding ->
      fun _b_encoding ->
        fun _c_encoding ->
          conv (fun (K (k0, k1, k2)) -> (k0, k1, k2))
            (fun (k0, k1, k2) -> K (k0, k1, k2))
            (obj1 (req "K" (tup3 _a_encoding _b_encoding _c_encoding)))
let _ = encoding_of_par_type2

[@@@end]
