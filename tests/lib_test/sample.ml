type abc = {
  test_me : int;
  name : string;
  tup : int * string;
  calculate : string -> int -> float -> int;
  arr : string array;
  l : int list;
}
[@@deriving show, default]

let _ =
  let abc = abc_default () in
  abc |> show_abc |> print_string |> print_newline |> flush_all
