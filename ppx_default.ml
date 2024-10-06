open Ppxlib
module List = ListLabels
open Ast_builder.Default

let url = "github.com/ProgramingIsTheFuture/ppx_default"

let not_supported_error e =
  failwith (Format.sprintf "%s. Create an issue at %s" e url)

let fun_names txt =
  if txt = "t" then "default"
  else "default_" ^ txt

let rec default_value_by_type ~loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr (({ txt = Ldot (_, _); loc } as l), _) ->
      let l =
        match l.txt with
        | Ldot (a, l) -> { txt = Ldot (a, fun_names l); loc }
        | _ -> l
      in
      let f = Ast_helper.Exp.ident l in
      Ast_builder.Default.pexp_apply ~loc f
        [ (Nolabel, pexp_construct ~loc { txt = lident "()"; loc } None) ]
  | Ptyp_constr ({ txt = Lident s; loc }, _) -> begin
      (* Handling constants *)
        match s with
        | "int" ->
            Ast_builder.Default.pexp_constant ~loc (Pconst_integer ("0", None))
        | "int64" ->
            Ast_builder.Default.pexp_constant ~loc
              (Ast_helper.Const.int64 Int64.zero)
        | "string" ->
            Ast_builder.Default.pexp_constant ~loc (Pconst_string ("", loc, None))
        | "float" ->
            Ast_builder.Default.pexp_constant ~loc (Pconst_float ("0.0", None))
        | "char" -> Ast_builder.Default.pexp_constant ~loc (Pconst_char ' ')
        | "array" -> Ast_builder.Default.pexp_array ~loc []
        | "list" ->
            Ast_builder.Default.pexp_construct ~loc
              { txt = lident "[]"; loc }
              None
        | _ ->
            let expr =
              Ast_builder.Default.pexp_ident ~loc { txt = lident (fun_names s); loc }
            in
            Ast_builder.Default.pexp_apply ~loc expr
              [ (Nolabel, pexp_construct ~loc { txt = lident "()"; loc } None) ]
    end
  | Ptyp_arrow (l, _, t2) ->
      (* Handling arrow types
         Gen a function that ignores all params and return the right expr *)
      Ast_builder.Default.pexp_fun ~loc l None
        (ppat_var ~loc { txt = "_"; loc })
        (default_value_by_type ~loc t2)
  | Ptyp_tuple cl ->
      (* Handling tuples *)
      Ast_builder.Default.pexp_tuple ~loc
        (List.map cl ~f:(default_value_by_type ~loc))
  | Ptyp_alias (core_type, _) ->
      default_value_by_type ~loc core_type
  | Ptyp_variant ({ prf_desc; prf_loc; _ } :: _, _, _) -> begin
      match prf_desc with
      | Rtag ({ txt; loc }
, true, []) ->
        Ast_builder.Default.pexp_variant ~loc txt None
      | Rtag ({ txt; loc }, _, l) ->
        Ast_builder.Default.pexp_variant
          ~loc
          txt
          (Option.some @@ Ast_builder.Default.pexp_tuple ~loc (List.map ~f:(default_value_by_type ~loc) l))
      | Rinherit core_type ->
        Ast_builder.Default.pexp_variant ~loc "" (Option.some @@ default_value_by_type  ~loc:prf_loc core_type)
  end
  | Ptyp_var l ->
      Ast_builder.Default.pexp_ident ~loc { txt = lident l; loc }
  | Ptyp_package _ | Ptyp_extension _
  | Ptyp_class _ | Ptyp_object _ | Ptyp_any | _ ->
      not_supported_error "Type is not supported"

let default_field ~loc field =
  let label = field.pld_name in
  let default_value = default_value_by_type ~loc field.pld_type in
  (label, default_value)

let default_fun ~loc ~ptype_name ~ptype_params expr =
  let name =
    let i = ref 0 in
    fun () ->
      let c = Char.chr (97 + !i) in
      incr i;
      Char.escaped c
  in
  let expr =
    List.fold_left ~f:(fun f ({ ptyp_loc=loc; _ }, _) ->
      pexp_fun ~loc Nolabel None
        (ppat_var ~loc { txt = (name ()); loc })
        f
    )
    ~init:(pexp_fun ~loc Nolabel None
      (ppat_construct ~loc { txt = lident "()"; loc } None)
      expr)
      ptype_params
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat =
          ppat_var ~loc { ptype_name with txt = fun_names ptype_name.txt };
        pvb_expr = expr;
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]

let default_impl ~(fields : label_declaration list) ~ptype_loc =
  let loc = ptype_loc in
  let field_initializers = List.map fields ~f:(default_field ~loc) in
  let record_expr =
    let fields =
      List.map field_initializers ~f:(fun ({ txt; _ }, value_expr) ->
          let lid = { txt = lident txt; loc } in
          (lid, value_expr))
    in
    Ast_builder.Default.pexp_record ~loc fields None
  in
  record_expr

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract;
       ptype_loc;
       ptype_name;
       ptype_manifest = Some core_t;
       ptype_params;
       _;
      } ->
          let expr = default_value_by_type ~loc:ptype_loc core_t in
          default_fun ~loc:ptype_loc ~ptype_name ~ptype_params expr
      | { ptype_kind = Ptype_variant constl; ptype_loc; ptype_name; ptype_params; _ } -> (
          let l =
            List.find_opt
              ~f:(fun a ->
                match a.pcd_args with
                | Pcstr_tuple [] | Pcstr_record [] -> true
                | _ -> false)
              constl
          in
          match l with
          | Some v ->
              let s = { txt = lident v.pcd_name.txt; loc = ptype_loc } in
              let expr =
                Ast_builder.Default.pexp_construct ~loc:ptype_loc s None
              in
              default_fun ~loc:ptype_loc ~ptype_name ~ptype_params expr
          | None -> (
              let l = List.hd constl in
              match l.pcd_args with
              | Pcstr_tuple types ->
                  let expr =
                    default_value_by_type ~loc:l.pcd_loc
                      {
                        ptyp_loc = ptype_loc;
                        ptyp_desc = Ptyp_tuple types;
                        ptyp_loc_stack = [];
                        ptyp_attributes = [];
                      }
                  in
                  let s = { txt = lident l.pcd_name.txt; loc = ptype_loc } in
                  let expr =
                    Ast_builder.Default.pexp_construct ~loc:ptype_loc s
                      (Some expr)
                  in
                  default_fun ~loc:ptype_loc ~ptype_name ~ptype_params expr
              | Pcstr_record fields ->
                  let s = { txt = lident l.pcd_name.txt; loc = ptype_loc } in
                  let expr = default_impl ~fields ~ptype_loc:l.pcd_loc in
                  let expr =
                    Ast_builder.Default.pexp_construct ~loc:ptype_loc s
                      (Some expr)
                  in
                  default_fun ~loc ~ptype_name expr ~ptype_params))
      | { ptype_kind = Ptype_record fields; ptype_name; ptype_loc; ptype_params; _ } ->
          default_impl ~fields ~ptype_loc |> default_fun ~loc ~ptype_name ~ptype_params
      | { ptype_loc; ptype_name; _ } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Not yet implemented to default this types: %s" ptype_name.txt
          in
          Ast_builder.Default.pstr_extension ~loc ext [])

let default_intf ~ptype_name ~loc ~ptype_params () =
  psig_value ~loc
    {
      pval_name = { ptype_name with txt = fun_names ptype_name.txt };
      pval_type =
        List.fold_left ~f:(fun f (core_typ, _)  ->
          ptyp_arrow ~loc Nolabel
            core_typ
            f
        ) ~init:(ptyp_arrow ~loc Nolabel
          (ptyp_constr ~loc { loc; txt = lident "unit" } [])
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } (List.map ~f:fst ptype_params))) ptype_params;
      pval_attributes = [];
      pval_loc = loc;
      pval_prim = [];
    }

let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_name; ptype_loc; ptype_params; _ } -> default_intf ~ptype_name ~loc:ptype_loc ~ptype_params ())

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let ppx_default =
  Deriving.add "default" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
