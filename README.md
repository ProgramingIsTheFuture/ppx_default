# ppx_default

Generate a default value based on the type definition.

```ocaml
type 'a t = {
  poly_field: 'a;
}[@@deriving default]

type ind =
  | Abc of int
  | Efg of string
  [@@deriving default]

let int_t = default 5 () (* { poly_field = 5 } *)
let ind_value = default_ind () (* (Abc 0) *)
```

```ocaml
type abc = {
  test_me : int;
  name : string;
  tup : int * string;
  calculate : string -> int -> float -> int;
  arr : string array;
  l : int list;
}
[@@deriving show, default]

let _ = default_abc () (* { Sample.test_me = 0; name = ""; tup = (0, ""); calculate = <fun>; arr = [||]; l = [] } *)
```

# Features missing

- use of polymorphic inside a record/inductive type

Eg:
```ocaml
type 'a d =
  D of 'a
  [@@deriving show, default]

type 'a f = {
  my_field : 'a d
}[@@deriving show, default]
```
