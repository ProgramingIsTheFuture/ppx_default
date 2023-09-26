type abc = { test_me : int; name : string } [@@deriving show, default]

let _ = abc_default () |> show_abc |> print_string |> print_newline |> flush_all
