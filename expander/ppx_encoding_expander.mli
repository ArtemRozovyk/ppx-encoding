open Ppxlib

val str_type_decl :
  loc:Location.t -> path:string -> rec_flag * type_declaration list -> structure
