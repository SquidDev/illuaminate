open IlluaminateSemantics
open Doc.Syntax
module SMap = Map.Make (String)

type t =
  { name : string;
    full_name : string;
    summary : string option;
    source : string option;
    in_module : IlluaminateSemantics.Namespace.Ref.t;
    name_of : IlluaminateSemantics.Reference.name_of
  }

let assoc x : Yojson.Safe.t = `Assoc x

let ( @?: ) (name, value) rest : (string * Yojson.Safe.t) list =
  match value with
  | None -> rest
  | Some v -> (name, `String v) :: rest

let ( @: ) (name, value) rest : (string * Yojson.Safe.t) list = (name, `String value) :: rest

let to_json names : Yojson.Safe.t =
  let build_term { name = _; full_name; summary; source; in_module; name_of } =
    let { namespace = Namespace module_kind; id; _ } = in_module in
    let url = Helpers.reference_link in_module name_of in
    `Assoc
      (("name", full_name) @: ("source", source) @?: ("summary", summary) @?: ("module", id)
     @: ("module-kind", module_kind) @: ("url", url) @: [])
  in
  SMap.to_seq names
  |> Seq.map (fun (ident, term) -> (ident, build_term term))
  |> List.of_seq |> assoc

let of_documented ~in_module ~name ~section ?(suffix = Fun.const "") ~source_link ~body names =
  function
  | { local = true; _ } -> names
  | { description; definition; descriptor; _ } as term -> (
      let pretty_name =
        Reference.Internal { in_module; name = section; definition }
        |> Format.asprintf "%a" Reference.pp_resolved
      in
      let self =
        { name;
          full_name = pretty_name ^ suffix descriptor;
          source = Helpers.link ~source_link term;
          summary =
            Option.map
              (fun (d : description) ->
                Helpers.get_summary d.description |> Omd.to_plain_text |> String.trim)
              description;
          in_module;
          name_of = section
        }
      in
      let names = body names descriptor in
      match SMap.find_opt name names with
      | None -> SMap.add name self names
      | Some other ->
          (* If we've got a conflict, prefer builtin namespaces. We could add other heuristics here
             (prefer code to markdown), but this'll do for now *)
          if
            List.mem in_module.namespace Namespace.builtins
            && not (List.mem other.in_module.namespace Namespace.builtins)
          then
            SMap.add (Format.asprintf "%a" Namespace.Ref.pp other.in_module) other names
            |> SMap.add name self
          else SMap.add (Format.asprintf "%a" Namespace.Ref.pp in_module) self names)

let dot_name modu name =
  match modu with
  | "_G" -> name
  | _ -> Printf.sprintf "%s.%s" modu name

let dot_section (section : Reference.name_of) n : Reference.name_of =
  match section with
  | Module -> Value n
  | Value v -> Value (dot_name v n)
  | Member (t, v) -> Member (t, dot_name v n)
  | Type _ -> failwith "Cannot dot type."

let of_term ~source_link ~in_module =
  let rec go ~name ~section names = function
    | Table xs ->
        CCList.fold_left
          (fun names (field, x) ->
            go' ~name:(dot_name name field) ~section:(dot_section section field) names x)
          names xs
    | _ -> names
  and go' ~name ~section names =
    of_documented ~in_module ~name ~section ~suffix:get_suffix ~source_link
      ~body:(go ~name ~section) names
  in
  go'

let of_type ~source_link ~in_module =
  let mod_name = in_module.Namespace.Ref.id in
  let member ~type_name names { member_name; member_value; _ } =
    of_term ~in_module ~source_link
      ~name:(Printf.sprintf "%s.%s.%s" mod_name type_name member_name)
      ~section:(Member (type_name, member_name))
      names member_value
  in
  let ty names { type_name; type_members } =
    CCList.fold_left (member ~type_name) names type_members
  in
  fun names x ->
    let name = x.descriptor.type_name in
    of_documented ~in_module ~source_link ~name:(dot_name mod_name name) ~section:(Type name)
      ~body:ty names x

let everything ~source_link =
  CCList.fold_left
    (fun names ({ descriptor = { page_ref = in_module; page_contents; _ }; _ } as d) ->
      match page_contents with
      | Module { mod_contents; mod_types; _ } ->
          let names = CCList.fold_left (of_type ~source_link ~in_module) names mod_types in
          of_term ~in_module ~source_link ~name:in_module.id ~section:Module names
            { d with descriptor = mod_contents }
      | Markdown ->
          of_term ~in_module ~source_link ~name:in_module.id ~section:Module names
            { d with descriptor = Unknown })
    SMap.empty
