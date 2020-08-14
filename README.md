Ppx_encoding
============= 


Generate the encoding description of a type.

`ppx_encoding` is a ppx rewriter which generates an object corresponding to the encoding description using the [data-encoding](https://gitlab.com/nomadic-labs/data-encoding) library. 

In order to use the preprocessor, add `(preprocess (pps ppx_encoding))` to your dune file. 

Basic Usage
-----------

The basic usage is simply to add `[@@deriving encoding]` after the type
definition.  For example:

```
type t =
  | Foo
  | Bar of bool
  | Baz of (int*float) list
  [@@deriving encoding]
```

This will create an object named `encoding` since by convention, `t` is the main type of the module. For other names, the name will be `encoding_of_your_type_name`. 

To look at what is produced you can generate the code at compile time by replacing `[@@deriving encoding]` by `[@@deriving_inline encoding][@@@end]` 
and executing : 
```
$ dune build --auto-promote
```

To execute the examples in test directory run 
```
dune build @runtest @lint --auto-promote
```

For instance, the aforementioned type `t` will generate the following code : 

```
let encoding =
  let open! Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Foo" (Tag 0)
        (obj1 (req "Foo" unit))
        (function Foo -> Some () | _ -> None)
        (fun () -> Foo);
      case ~title:"Bar" (Tag 1)
        (obj1 (req "Bar" bool))
        (function Bar bar -> Some bar | _ -> None)
        (fun bar -> Bar bar);
      case ~title:"Baz" (Tag 2)
        (obj1 (req "Baz" (list (tup2 int31 float))))
        (function Baz baz -> Some baz | _ -> None)
        (fun baz -> Baz baz);
    ]
```

