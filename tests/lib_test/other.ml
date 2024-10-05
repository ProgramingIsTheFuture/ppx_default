type binding = { error_here : int } [@@deriving show, default]

type 'a poly_record = {
  poly_field: 'a;
}[@@deriving show, default]

module A = struct
  type 'a r = {
    example: 'a;
  }[@@deriving show, default]

  type e =
    E of int
  [@@deriving show, default]

  type t =
    [
    | `Abc of e
    | `Some of string
    ][@@deriving show, default]
end

let () =
  let t = A.default () in
  let a = default_poly_record 10 () in
  Format.printf "%s@.\n" @@ A.show t;
  Format.printf "%s@." @@ show_poly_record (fun f a -> Format.fprintf f "%d" a) a
