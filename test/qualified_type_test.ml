(*Following the convention, if the type is named "t" we suppose 
that there exists an encoding object in the same module, named "encoding", 
which corresponds to the encoding of that type. Otherwise, an object having the 
same name as the type, prefixed with "encoding_of_" will be expected to be defined. *)

type t1 = Test_types.t [@@deriving_inline encoding]

let _ = fun (_ : t1) -> ()

let encoding_of_t1 =
  let open! Data_encoding in
  Test_types.encoding

let _ = encoding_of_t1

[@@@end]

type t2 = Test_types.SubTest.t [@@deriving_inline encoding]

let _ = fun (_ : t2) -> ()

let encoding_of_t2 =
  let open! Data_encoding in
  Test_types.SubTest.encoding

let _ = encoding_of_t2

[@@@end]

type t3 = Test_types.some_t [@@deriving_inline encoding]

let _ = fun (_ : t3) -> ()

let encoding_of_t3 =
  let open! Data_encoding in
  Test_types.encoding_of_some_t

let _ = encoding_of_t3

[@@@end]
