open IlluaminateSemantics
open Doc.Syntax
module SSet = Set.Make (String)

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

let to_json xs : Yojson.Safe.t =
  let _, items =
    List.fold_left
      (fun (seen, xs) { name; full_name; summary; source; in_module; name_of } ->
        let Namespace module_kind, module_name = in_module in
        let url = Helpers.reference_link in_module name_of in
        let section = Reference.section_of_name name_of in
        (* TODO: Drop section in a few months once we've updated the various bots that scrape
           this. *)
        let term =
          `Assoc
            (("name", full_name) @: ("source", source) @?: ("summary", summary)
           @?: ("module", module_name) @: ("module-kind", module_kind) @: ("section", section)
           @?: ("url", url) @: [])
        in
        if SSet.mem name seen then (seen, xs) else (SSet.add name seen, (name, term) :: xs))
      (SSet.empty, []) xs
  in
  assoc items

let of_documented ~in_module ~name ~section ?(suffix = Fun.const "") ~source_link ~body = function
  | { local = true; _ } -> []
  | { description; definition; descriptor; _ } as term ->
      let pretty_name =
        Reference.Internal { in_module; name = section; definition }
        |> Format.asprintf "%a" Reference.pp_resolved
      in
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
      :: body descriptor

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
  let rec go ~name ~section = function
    | Table xs ->
        CCList.flat_map
          (fun (field, x) -> go' ~name:(dot_name name field) ~section:(dot_section section field) x)
          xs
    | _ -> []
  and go' ~name ~section =
    of_documented ~in_module ~name ~section ~suffix:get_suffix ~source_link
      ~body:(go ~name ~section)
  in
  go'

let of_type ~source_link ~in_module =
  let _, mod_name = in_module in
  let member ~type_name { member_name; member_value; _ } =
    of_term ~in_module ~source_link
      ~name:(Printf.sprintf "%s.%s.%s" mod_name type_name member_name)
      ~section:(Member (type_name, member_name))
      member_value
  in
  let ty { type_name; type_members } = CCList.flat_map (member ~type_name) type_members in
  fun x ->
    let name = x.descriptor.type_name in
    of_documented ~in_module ~source_link ~name:(dot_name mod_name name) ~section:(Type name)
      ~body:ty x

let everything ~source_link =
  CCList.flat_map @@ fun ({ descriptor = { page_namespace; page_id; page_contents; _ }; _ } as d) ->
  let in_module = (page_namespace, page_id) in
  match page_contents with
  | Module { mod_contents; mod_types; _ } ->
      of_term ~in_module ~source_link ~name:page_id ~section:Module
        { d with descriptor = mod_contents }
      @ CCList.flat_map (of_type ~source_link ~in_module) mod_types
  | Markdown ->
      of_term ~in_module ~source_link ~name:page_id ~section:Module { d with descriptor = Unknown }
