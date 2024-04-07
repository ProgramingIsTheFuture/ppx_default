# v0.1

New features:
- hability to default any type
ex: 
```ocaml
type test = int [@@deriving default]
(* val test_default : () -> test *)
```
