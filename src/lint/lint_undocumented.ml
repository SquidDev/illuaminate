open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let linter =
  let tag = Error.Tag.make Error.Warning "doc:undocumented" in
  let arg_tag = Error.Tag.make Error.Warning "doc:undocumented-arg" in
  let return_tag = Error.Tag.make Error.Warning "doc:undocumented-return" in

  let check ~notes ~tag ~span description msg =
    match description with
    | Some _ -> ()
    | None -> notes := note ~span ~tag "%s" msg :: !notes
  in

  let arg ~notes ~span { arg_description; arg_name; _ } =
    Printf.sprintf "Argument `%s` is missing a description" arg_name
    |> check ~notes ~span ~tag:arg_tag arg_description
  in

  let return ~notes ~span { ret_description; _ } =
    check ~notes ~span ~tag:return_tag ret_description "@return tag is missing a description"
  in

  let rec value ~notes ~span = function
    | Function { args; rets; _ } ->
        List.iter (List.iter (arg ~notes ~span)) args;
        List.iter (List.iter (return ~notes ~span)) rets
    | Table xs ->
        xs
        |> List.iter @@ fun (k, x) ->
           Printf.sprintf "`%s` is exported, but has no documentation." k
           |> documented value ~notes x
    | Type x -> ty ~notes ~span x
    | Expr _ | Unknown | Undefined -> ()
  and documented :
      type a.
      (notes:Syntax.program note list ref -> span:Span.t -> a -> unit) ->
      notes:Syntax.program note list ref ->
      a documented ->
      string ->
      unit =
   fun child ~notes { description; descriptor; definition; _ } message ->
    check ~notes ~span:definition ~tag description message;
    child ~notes ~span:definition descriptor
  and ty ~notes ~span:_ { type_name; type_members; _ } =
    type_members
    |> List.iter @@ fun { member_name; member_value; _ } ->
       Printf.sprintf "Type member `%s:%s` is exported, but has no documentation." type_name
         member_name
       |> documented value ~notes member_value
  in
  let modu ~notes ~span { mod_contents; mod_types; _ } =
    mod_types
    |> List.iter (fun ({ descriptor = { type_name; _ }; _ } as t) ->
           Printf.sprintf "Type '%s' is exported, but has no documentation." type_name
           |> documented ty ~notes t);
    value ~notes ~span mod_contents
  in

  make_no_opt ~tags:[ tag; arg_tag; return_tag ]
    ~program:(fun () context prog ->
      match Data.get prog E.key context.data |> E.get_module with
      | None -> []
      | Some m ->
          let notes = ref [] in
          documented modu ~notes m "Module is missing documentation";
          !notes)
    ()
