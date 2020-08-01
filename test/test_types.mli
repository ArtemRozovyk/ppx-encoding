open Data_encoding

type t = int

val encoding : int encoding

type some_t = int

val encoding_of_some_t : int encoding

module SubTest : sig
  type t = int

  val encoding : int encoding
end
