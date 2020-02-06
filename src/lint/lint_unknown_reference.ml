open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let linter =
  let tag = Error.Tag.make Error.Warning "doc:unresolved-reference" in

  let check ~notes ~span = function
    | Reference.Unknown x -> notes := note ~span ~tag "Unknown reference %S." x :: !notes
    | Internal _ | External _ -> ()
  in
  let description ~notes ~span (Description d) =
    let open Omd in
    let rec visit = function
      | Html ("illuaminate:ref", [ ("link", Some x) ], _) ->
          notes := note ~span ~tag "Unknown reference %S." x :: !notes
      | H1 x | H2 x | H3 x | H4 x | H5 x | H6 x | Paragraph x | Emph x | Bold x -> List.iter visit x
      | Url (_, x, _) | Blockquote x | Html (_, _, x) | Html_block (_, _, x) -> List.iter visit x
      | Ul xs | Ol xs | Ulp xs | Olp xs -> List.iter (List.iter visit) xs
      | Ref (_, _, _, f) | Img_ref (_, _, _, f) -> List.iter visit f#to_t
      | Text _ | Code _ | Code_block _ | Br | Hr | NL | Html_comment _ | Raw _ | Raw_block _ | Img _
        ->
          ()
      | X f -> Option.iter (List.iter visit) (f#to_t [])
    in
    List.iter visit d
  in

  let rec type_ ~notes ~span = function
    | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
    | Named (r, _) -> check ~notes ~span r
    | Function { args; return = rs, r } ->
        List.iter (fun { Type.ty; _ } -> type_ ~notes ~span ty) args;
        List.iter (type_ ~notes ~span) rs;
        Option.iter (type_ ~notes ~span) r
    | Table xs -> List.iter (table_entry ~notes ~span) xs
    | Union xs -> List.iter (type_ ~notes ~span) xs
  and table_entry ~notes ~span = function
    | Type.Field { value = x; optional = _; key = _ } | Item x | Many x -> type_ ~notes ~span x
    | Hash { key; optional = _; value } -> type_ ~notes ~span key; type_ ~notes ~span value
  in

  let arg ~notes ~span { arg_description; arg_type; _ } =
    Option.iter (type_ ~notes ~span) arg_type;
    Option.iter (description ~notes ~span) arg_description
  in
  let return ~notes ~span { ret_description; ret_type; _ } =
    Option.iter (type_ ~notes ~span) ret_type;
    Option.iter (description ~notes ~span) ret_description
  in
  let see ~notes ~span { see_reference; see_description; _ } =
    check ~notes ~span see_reference;
    Option.iter (description ~notes ~span) see_description
  in
  let example ~notes ~span = function
    | RawExample _ -> ()
    | RichExample d -> description ~notes ~span d
  in

  let rec value ~notes ~span = function
    | Function { args; rets; throws; has_self = _ } ->
        List.iter (List.iter (arg ~notes ~span)) args;
        List.iter (List.iter (return ~notes ~span)) rets;
        List.iter (description ~notes ~span) throws
    | Table xs -> List.iter (fun (_, x) -> documented value ~notes x) xs
    | Type x -> ty ~notes ~span x
    | Expr { ty; _ } -> type_ ~notes ~span ty
    | Unknown | Undefined -> ()
  and documented :
      type a.
      (notes:Syntax.program note list ref -> span:Span.t -> a -> unit) ->
      notes:Syntax.program note list ref ->
      a documented ->
      unit =
   fun child ~notes d ->
    let span = d.definition in
    Option.iter (description ~notes ~span) d.description;
    List.iter (see ~notes ~span) d.see;
    List.iter (example ~notes ~span) d.examples;
    child ~notes ~span d.descriptor
  and ty ~notes ~span:_ { type_members; _ } =
    type_members |> List.iter @@ fun { member_value; _ } -> documented value ~notes member_value
  in
  let modu ~notes ~span { mod_contents; mod_types; _ } =
    List.iter (documented ty ~notes) mod_types;
    value ~notes ~span mod_contents
  in

  make_no_opt ~tags:[ tag ]
    ~program:(fun () context prog ->
      match Data.get prog E.key context.data |> E.get_module with
      | None -> []
      | Some m ->
          let notes = ref [] in
          documented modu ~notes m; !notes)
    ()
