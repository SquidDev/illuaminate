open IlluaminateSemantics.Doc.Syntax

type t =
  { name : string;
    full_name : string;
    summary : string option;
    source : string option
  }

let assoc x : Yojson.Safe.t = `Assoc x

let ( @?: ) (name, value) rest : (string * Yojson.Safe.t) list =
  match value with
  | None -> rest
  | Some v -> (name, `String v) :: rest

let ( @: ) x rest : (string * Yojson.Safe.t) list = x :: rest

let to_json xs : Yojson.Safe.t =
  List.rev_map
    (fun { name; full_name; summary; source } ->
      ( name,
        `Assoc (("name", `String full_name) @: ("source", source) @?: ("summary", summary) @?: [])
      ))
    xs
  |> assoc

let of_documented ~name ?(suffix = Fun.const "") ~source_link ~body = function
  | { local = true; _ } -> []
  | { description; definition; descriptor; _ } ->
      { name;
        full_name = name ^ suffix descriptor;
        source = source_link definition;
        summary =
          Option.map (fun (Description m) -> Helpers.get_summary m |> Omd.to_text) description
      }
      :: body ~name descriptor

let dot_name = Printf.sprintf "%s.%s"

let of_term ~source_link =
  let rec go ~name = function
    | Table xs -> CCList.flat_map (fun (field, x) -> go' ~name:(dot_name name field) x) xs
    | _ -> []
  and go' ~name body = of_documented ~name ~suffix:get_suffix ~source_link ~body:go body in
  go'

let of_type ~source_link =
  let member ~name { member_name; member_value; _ } =
    of_term ~source_link ~name:(dot_name name member_name) member_value
  in
  let ty ~name { type_name; type_members } =
    let name = dot_name name type_name in
    CCList.flat_map (member ~name) type_members
  in
  fun ~name x -> of_documented ~source_link ~body:ty ~name x

let everything ~source_link =
  CCList.flat_map @@ fun ({ descriptor = { mod_name; mod_types; mod_contents; _ }; _ } as d) ->
  of_term ~source_link ~name:mod_name { d with descriptor = mod_contents }
  @ CCList.flat_map (of_type ~source_link ~name:mod_name) mod_types
