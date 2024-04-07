open Ppxlib
module List = ListLabels
open Ast_builder.Default

let default_impl ~(fields : label_declaration list) ~ptype_name ~ptype_loc =
  let loc = ptype_loc in

  let field_initializers =
    List.map fields ~f:(fun field ->
        let label = field.pld_name in
        let default_value =
          (* TODO: Add support for all known types
              Recursivly default types
              Handle errors: "_" case is supposed to be an error/not supported yet. *)
          match field.pld_type.ptyp_desc with
          | Ptyp_constr ({ txt = Lident s; _ }, _) -> (
              match s with
              | "int" ->
                  Ast_builder.Default.pexp_constant ~loc
                    (Pconst_integer ("0", None))
              | "string" ->
                  Ast_builder.Default.pexp_constant ~loc
                    (Pconst_string ("", loc, None))
              | _ ->
                  Ast_builder.Default.pexp_constant ~loc
                    (Pconst_integer ("0", None)))
          | _ ->
              Ast_builder.Default.pexp_constant ~loc
                (Pconst_integer ("0", None))
        in
        (label, default_value))
  in
  let record_expr =
    let fields =
      List.map field_initializers ~f:(fun ({ txt; _ }, value_expr) ->
          let lid = { txt = lident txt; loc } in
          (lid, value_expr))
    in
    Ast_builder.Default.pexp_record ~loc fields None
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat =
          ppat_var ~loc { ptype_name with txt = ptype_name.txt ^ "_default" };
        pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_construct ~loc { txt = lident "()"; loc } None)
            record_expr;
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]

let ( ^ ) (l1 : label) (l2 : label) = String.concat l1 [ l2 ]

let default_intf ~ptype_name ~loc =
  psig_value ~loc
    {
      pval_name = { ptype_name with txt = ptype_name.txt ^ "_default" };
      pval_type =
        ptyp_arrow ~loc Nolabel
          (ptyp_constr ~loc { loc; txt = lident "unit" } [])
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } []);
      pval_attributes = [];
      pval_loc = loc;
      pval_prim = [];
    }

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
       ptype_loc;
       _;
      } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Not yet implemented to default this types"
          in
          Ast_builder.Default.pstr_extension ~loc ext []
      | { ptype_kind = Ptype_record fields; ptype_name; ptype_loc; _ } ->
          default_impl ~fields ~ptype_name ~ptype_loc)

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
       ptype_loc;
       _;
      } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Not yet implemented to default this types"
          in
          Ast_builder.Default.psig_extension ~loc ext []
      | { ptype_kind = Ptype_record _; ptype_name; ptype_loc; _ } ->
          default_intf ~ptype_name ~loc:ptype_loc)

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let ppx_default =
  Deriving.add "default" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
