open Data_encoding

type t = int

let encoding = int31

type some_t = int

let encoding_of_some_t = int31

module SubTest = struct
  type t = int

  let encoding = int31
end
