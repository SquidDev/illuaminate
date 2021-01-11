open! Doc_syntax
open! Reference
module StringMap = Map.Make (String)
module MKMap = Map.Make (Module.Kind)
module Lift = Doc_abstract_syntax.Lift (Doc_syntax) (Doc_syntax)

module DocTbl = Hashtbl.Make (struct
  type t = Doc_syntax.value documented

  let hash = Hashtbl.hash

  let equal = ( == )
end)

(** Strategies for locating names. The general intuition is:

    - Lookup in the current module.
    - Find a module with the current name.
    - Look up in a module (for instance [foo.bar.baz] searches for [foo -> bar.baz] and
      [foo.bar -> baz])
    - Link to a Lua global. *)
module Resolvers = struct
  type state =
    { modules : module_info documented Lazy.t StringMap.t MKMap.t;
      mutable unique_modules : module_info documented option StringMap.t;
      current_module : module_info documented option
    }

  (** Find a unique module by a name.*)
  let lookup_unique_module state name =
    match StringMap.find_opt name state.unique_modules with
    | Some x -> x
    | None ->
        let matching =
          MKMap.to_seq state.modules |> Seq.map snd
          |> Seq.filter_map (StringMap.find_opt name)
          |> List.of_seq
        in
        let unique =
          match matching with
          | [ (lazy x) ] -> Some x
          | _ -> None
        in
        state.unique_modules <- StringMap.add name unique state.unique_modules;
        unique

  (** Find a module by a [type!name specifier] or by a name *)
  let lookup_module state name =
    match String.index_opt name '!' with
    | None -> lookup_unique_module state name
    | Some i ->
        let kind = CCString.take i name and name = CCString.drop (i + 1) name in
        MKMap.find_opt (ModuleKind kind) state.modules
        |> CCOpt.flat_map (StringMap.find_opt name)
        |> Option.map Lazy.force

  (* Look up names within a module *)
  let in_module_value { mod_kind; mod_name; mod_contents; _ } name is_type =
    if is_type then None
    else
      match mod_contents with
      | Table fields ->
          List.find_opt (fun (k, _) -> k = name) fields
          |> Option.map @@ fun (_, { definition; _ }) ->
             Internal { in_module = (mod_kind, mod_name); name = Value name; definition }
      | _ -> None

  (** Look up types within a module *)
  let in_module_type { mod_kind; mod_name; mod_types; _ } name _ =
    List.find_opt (fun ty -> ty.descriptor.type_name = name) mod_types
    |> Option.map @@ fun { definition; _ } ->
       Internal { in_module = (mod_kind, mod_name); name = Type name; definition }

  (** Look up methods within a module *)
  let in_module_method { mod_kind; mod_name; mod_types; _ } name is_type =
    if is_type then None
    else
      String.index_opt name ':'
      |> CCOpt.or_lazy ~else_:(fun () -> String.rindex_opt name '.')
      |> CCOpt.flat_map @@ fun i ->
         let type_name = CCString.take i name and item_name = CCString.drop (i + 1) name in
         mod_types
         |> List.find_opt (fun ty -> ty.descriptor.type_name = type_name)
         |> CCOpt.flat_map (fun ty ->
                List.find_opt
                  (fun { member_name; _ } -> member_name = item_name)
                  ty.descriptor.type_members)
         |> Option.map (fun { member_name; member_value; _ } ->
                Internal
                  { in_module = (mod_kind, mod_name);
                    name = Member (type_name, member_name);
                    definition = member_value.definition
                  })

  let in_module_finders = [ in_module_value; in_module_type; in_module_method ]

  (** Find a term in this module. *)
  let find_in_current_module { current_module; _ } name is_type =
    Option.bind current_module @@ fun current_module ->
    CCList.find_map (fun f -> f current_module.descriptor name is_type) in_module_finders

  (** Find a module with this name *)
  let find_module s name is_type =
    if is_type then None
    else
      lookup_module s name
      |> Option.map @@ fun { definition; descriptor = { mod_kind; mod_name; _ }; _ } ->
         Internal { in_module = (mod_kind, mod_name); name = Module; definition }

  (** Finds elements within modules. This tries foo.[bar.baz], then foo.bar.[baz], etc... *)
  let find_member s name is_type =
    let rec go i =
      match String.index_from_opt name i '.' with
      | None -> None
      | Some i ->
          let mod_name = CCString.take i name and item_name = CCString.drop (i + 1) name in
          lookup_module s mod_name
          |> CCOpt.flat_map (fun { descriptor = modu; _ } ->
                 CCList.find_map (fun f -> f modu item_name is_type) in_module_finders)
          |> CCOpt.or_lazy ~else_:(fun () -> go (i + 1))
    in
    go 0

  (** Looks up a Lua name *)
  let find_lua_builtin _ name is_type =
    match if is_type then Lua_reference.lookup_type name else Lua_reference.lookup_name name with
    | InManual section ->
        Some (External { name; url = Some (Lua_reference.manual_section section) })
    | Undocumented -> Some (External { name; url = None })
    | Unknown -> None

  let finders = [ find_in_current_module; find_module; find_member; find_lua_builtin ]

  (* Validate the reference is well-formed, resolve it, and return null if we can't *)
  let name context ~types_only name =
    let is_ident c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    if
      String.length name > 0
      && is_ident name.[String.length name - 1]
      && CCString.for_all (fun x -> is_ident x || x == '.' || x == ':') name
    then
      CCList.find_map (fun f -> f context name types_only) finders
      |> Option.value ~default:(Reference.Unknown name)
    else Unknown name

  let ref context ~types_only : reference -> reference = function
    | Unknown n -> name context ~types_only n
    | (External _ | Internal _) as r -> r
end

type cache = Doc_syntax.value Doc_syntax.documented DocTbl.t

type context = Lift.t

let go_desc context { description; description_pos } =
  let open Omd in
  let visit = function
    | Html ("illuaminate:ref", tags, label) ->
        let { Doc_comment.link_reference = Reference link;
              link_label = { description; description_pos };
              link_style
            } =
          Doc_comment.Link.of_tag tags label
        in
        let r = Resolvers.name context ~types_only:false link in
        Some
          [ Link.to_tag
              { link_reference = r; link_label = { description; description_pos }; link_style }
          ]
    | _ -> None
  in
  { description = Omd_representation.visit visit description; description_pos }

let context modules current_module =
  let context = { Resolvers.modules; current_module; unique_modules = StringMap.empty } in
  let lift : Lift.t =
    { any_ref = Resolvers.ref context ~types_only:false;
      type_ref = Resolvers.ref context ~types_only:true;
      description = go_desc context
    }
  in
  (DocTbl.create 16, lift)

let go_documented lift go_child
    { description;
      descriptor : 'a;
      definition;
      examples;
      see;
      local;
      export;
      deprecated;
      custom_source
    } =
  { description = Option.map (Lift.description lift) description;
    descriptor = go_child lift descriptor;
    definition;
    examples = List.map (Lift.example lift) examples;
    see = List.map (Lift.see lift) see;
    local;
    export;
    deprecated = Option.map (Lift.deprecation lift) deprecated;
    custom_source
  }

let rec go_value ~cache lift = function
  | Function { args; rets; throws; has_self } ->
      Function
        { args = List.map (List.map (Lift.arg lift)) args;
          rets = List.map (List.map (Lift.return lift)) rets;
          throws = List.map (Lift.description lift) throws;
          has_self
        }
  | Table fields ->
      Table (List.map (fun (name, field) -> (name, go_value_doc ~cache lift field)) fields)
  | (Expr _ | Unknown | Undefined) as e -> e
  | Type t -> Type (go_type ~cache lift t)

and go_value_doc ~cache lift value =
  match DocTbl.find_opt cache value with
  | Some x -> x
  | None ->
      let x = go_documented lift (go_value ~cache) value in
      DocTbl.add cache value x; x

and go_type ~cache lift { type_name; type_members } =
  let go_member { member_name; member_is_method; member_value } =
    { member_name; member_is_method; member_value = go_value_doc ~cache lift member_value }
  in
  { type_name; type_members = List.map go_member type_members }

let go_module ~cache lift to_resolve =
  go_documented lift
    (fun lift { mod_name; mod_kind; mod_contents; mod_types } ->
      { mod_name;
        mod_kind;
        mod_contents = go_value ~cache lift mod_contents;
        mod_types = List.map (go_documented lift (go_type ~cache)) mod_types
      })
    to_resolve
