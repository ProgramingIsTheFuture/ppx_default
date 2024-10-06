type binding [@@deriving show, default]

module A : sig
  type t[@@deriving show, default]
  type 'a r[@@deriving show, default]
end
