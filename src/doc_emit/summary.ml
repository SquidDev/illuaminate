open IlluaminateSemantics
open Doc.Syntax

type t =
  { name : string;
    full_name : string;
    summary : string option;
    source : string option;
    in_module : string;
    section : string option
  }

let assoc x : Yojson.Safe.t = `Assoc x

let ( @?: ) (name, value) rest : (string * Yojson.Safe.t) list =
  match value with
  | None -> rest
  | Some v -> (name, `String v) :: rest

let ( @: ) x rest : (string * Yojson.Safe.t) list = x :: rest

let to_json xs : Yojson.Safe.t =
  List.rev_map
    (fun { name; full_name; summary; source; in_module; section } ->
      ( name,
        `Assoc
          ( ("name", `String full_name)
          @: ("source", source) @?: ("summary", summary)
          @?: ("module", `String in_module)
          @: ("section", section) @?: [] ) ))
    xs
  |> assoc

let of_documented ~in_module ~name ~section ?(suffix = Fun.const "") ~source_link ~body = function
  | { local = true; _ } -> []
  | { description; definition; descriptor; _ } ->
      let pretty_name =
        Reference.Internal { in_module; name = section; definition }
        |> Format.asprintf "%a" Reference.pp_resolved
      in
      { name;
        full_name = pretty_name ^ suffix descriptor;
        source = source_link definition;
        summary =
          Option.map
            (fun (d : description) ->
              Helpers.get_summary d.description |> Omd.to_text |> String.trim)
            description;
        in_module;
        section = Reference.section_of_name section
      }
      :: body descriptor

let dot_name = Printf.sprintf "%s.%s"

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
  let member ~type_name { member_name; member_value; _ } =
    of_term ~in_module ~source_link
      ~name:(Printf.sprintf "%s.%s.%s" in_module type_name member_name)
      ~section:(Member (type_name, member_name))
      member_value
  in
  let ty { type_name; type_members } = CCList.flat_map (member ~type_name) type_members in
  fun x ->
    let name = x.descriptor.type_name in
    of_documented ~in_module ~source_link ~name:(dot_name in_module name) ~section:(Type name)
      ~body:ty x

let everything ~source_link =
  CCList.flat_map @@ fun ({ descriptor = { mod_name; mod_types; mod_contents; _ }; _ } as d) ->
  of_term ~in_module:mod_name ~source_link ~name:mod_name ~section:Module
    { d with descriptor = mod_contents }
  @ CCList.flat_map (of_type ~source_link ~in_module:mod_name) mod_types
