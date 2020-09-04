type t = A of t list [@@deriving_inline encoding]

let _ = fun (_ : t) -> ()

let encoding =
  let open! Data_encoding in
  mu "t" (fun t_encoding ->
      conv (fun (A a) -> a) (fun a -> A a) (obj1 (req "A" (list t_encoding))))

let _ = encoding

[@@@end]

type 'a rec_t = M | T of string | X of 'a | P of 'a * 'a | F of 'a * 'a rec_t
[@@deriving_inline encoding]

let _ = fun (_ : 'a rec_t) -> ()

let encoding_of_rec_t =
  let open! Data_encoding in
  fun _a_encoding ->
    mu "rec_t" (fun rec_t_encoding ->
        union ~tag_size:`Uint8
          [
            case ~title:"M" (Tag 0)
              (obj1 (req "M" unit))
              (function M -> Some () | _ -> None)
              (fun () -> M);
            case ~title:"T" (Tag 1)
              (obj1 (req "T" string))
              (function T t -> Some t | _ -> None)
              (fun t -> T t);
            case ~title:"X" (Tag 2)
              (obj1 (req "X" _a_encoding))
              (function X x -> Some x | _ -> None)
              (fun x -> X x);
            case ~title:"P" (Tag 3)
              (obj1 (req "P" (tup2 _a_encoding _a_encoding)))
              (function P (p0, p1) -> Some (p0, p1) | _ -> None)
              (fun (p0, p1) -> P (p0, p1));
            case ~title:"F" (Tag 4)
              (obj1 (req "F" (tup2 _a_encoding rec_t_encoding)))
              (function F (f0, f1) -> Some (f0, f1) | _ -> None)
              (fun (f0, f1) -> F (f0, f1));
          ])

let _ = encoding_of_rec_t

[@@@end]

type ('a, 'b) par_rec_type1 =
  | Ie of 'a
  | Ne of ('a, 'b) par_rec_type1
  | Ker of 'a * ('a, 'b) par_rec_type1
[@@deriving_inline encoding]

let _ = fun (_ : ('a, 'b) par_rec_type1) -> ()

let encoding_of_par_rec_type1 =
  let open! Data_encoding in
  fun _a_encoding _b_encoding ->
    mu "par_rec_type1" (fun par_rec_type1_encoding ->
        union ~tag_size:`Uint8
          [
            case ~title:"Ie" (Tag 0)
              (obj1 (req "Ie" _a_encoding))
              (function Ie ie -> Some ie | _ -> None)
              (fun ie -> Ie ie);
            case ~title:"Ne" (Tag 1)
              (obj1 (req "Ne" par_rec_type1_encoding))
              (function Ne ne -> Some ne | _ -> None)
              (fun ne -> Ne ne);
            case ~title:"Ker" (Tag 2)
              (obj1 (req "Ker" (tup2 _a_encoding par_rec_type1_encoding)))
              (function Ker (ker0, ker1) -> Some (ker0, ker1) | _ -> None)
              (fun (ker0, ker1) -> Ker (ker0, ker1));
          ])

let _ = encoding_of_par_rec_type1

[@@@end]

type 'a rec_t2 =
  | O of { bla : 'a }
  | S of { bla : 'a }
  | S1 of { bla : 'a * 'a }
  | I of { bli : 'a rec_t2 }
  | U of { titi : 'a rec_t2; tata : int }
  | V of { nunu : 'a rec_t2; nini : 'a }
[@@deriving_inline encoding]

let _ = fun (_ : 'a rec_t2) -> ()

let encoding_of_rec_t2 =
  let open! Data_encoding in
  fun _a_encoding ->
    mu "rec_t2" (fun rec_t2_encoding ->
        union ~tag_size:`Uint8
          [
            case ~title:"O" (Tag 0)
              (obj1 (req "O" (obj1 (req "bla" _a_encoding))))
              (function O { bla } -> Some bla | _ -> None)
              (fun bla -> O { bla });
            case ~title:"S" (Tag 1)
              (obj1 (req "S" (obj1 (req "bla" _a_encoding))))
              (function S { bla } -> Some bla | _ -> None)
              (fun bla -> S { bla });
            case ~title:"S1" (Tag 2)
              (obj1
                 (req "S1" (obj1 (req "bla" (tup2 _a_encoding _a_encoding)))))
              (function S1 { bla } -> Some bla | _ -> None)
              (fun bla -> S1 { bla });
            case ~title:"I" (Tag 3)
              (obj1 (req "I" (obj1 (req "bli" rec_t2_encoding))))
              (function I { bli } -> Some bli | _ -> None)
              (fun bli -> I { bli });
            case ~title:"U" (Tag 4)
              (obj1
                 (req "U"
                    (obj2 (req "titi" rec_t2_encoding) (req "tata" int31))))
              (function U { titi; tata } -> Some (titi, tata) | _ -> None)
              (fun (titi, tata) -> U { titi; tata });
            case ~title:"V" (Tag 5)
              (obj1
                 (req "V"
                    (obj2 (req "nunu" rec_t2_encoding) (req "nini" _a_encoding))))
              (function V { nunu; nini } -> Some (nunu, nini) | _ -> None)
              (fun (nunu, nini) -> V { nunu; nini });
          ])

let _ = encoding_of_rec_t2

[@@@end]
