open! Doc_syntax
open! Reference
module StringMap = Map.Make (String)
module NMap = Map.Make (Namespace)
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
    { pages : page documented Lazy.t StringMap.t NMap.t;
      mutable unique_pages : page documented option StringMap.t;
      current_page : page documented option
    }

  let is_module = function
    | (lazy { descriptor = { page_contents = Module _; _ }; _ }) -> true
    | (lazy { descriptor = { page_contents = Markdown; _ }; _ }) -> false

  (** Find a unique module by a name.*)
  let lookup_unique_module state name =
    match StringMap.find_opt name state.unique_pages with
    | Some x -> x
    | None ->
        let matching =
          NMap.to_seq state.pages |> Seq.map snd
          |> Seq.filter_map (StringMap.find_opt name)
          |> List.of_seq
        in
        let unique =
          match matching with
          | [ (lazy x) ] -> Some x
          | _ -> (
            (* TODO: Really should warn on ambiguous ones here. But propagating this is rather
               hard. *)
            match List.filter is_module matching with
            | [ (lazy x) ] -> Some x
            | _ -> None)
        in
        state.unique_pages <- StringMap.add name unique state.unique_pages;
        unique

  (** Find a module by a [type!name specifier] or by a name *)
  let lookup_module state name =
    match String.index_opt name '!' with
    | None -> lookup_unique_module state name
    | Some i ->
        let kind = CCString.take i name and name = CCString.drop (i + 1) name in
        NMap.find_opt (Namespace kind) state.pages
        |> CCOption.flat_map (StringMap.find_opt name)
        |> Option.map Lazy.force

  (* Look up names within a module *)
  let in_module_value { page_contents; page_ref } name is_type =
    match page_contents with
    | Module { mod_contents = Table fields; _ } when not is_type ->
        List.find_opt (fun (k, _) -> k = name) fields
        |> Option.map @@ fun (_, { definition; _ }) ->
           Internal { in_module = page_ref; name = Value name; definition }
    | _ -> None

  (** Look up types within a module *)
  let in_module_type { page_contents; page_ref } name _ =
    match page_contents with
    | Markdown -> None
    | Module { mod_types; _ } ->
        List.find_opt (fun ty -> ty.descriptor.type_name = name) mod_types
        |> Option.map @@ fun { definition; _ } ->
           Internal { in_module = page_ref; name = Type name; definition }

  (** Look up methods within a module *)
  let in_module_method { page_contents; page_ref } name is_type =
    match page_contents with
    | Module { mod_types; _ } when not is_type ->
        String.index_opt name ':'
        |> CCOption.or_lazy ~else_:(fun () -> String.rindex_opt name '.')
        |> CCOption.flat_map @@ fun i ->
           let type_name = CCString.take i name and item_name = CCString.drop (i + 1) name in
           mod_types
           |> List.find_opt (fun ty -> ty.descriptor.type_name = type_name)
           |> CCOption.flat_map (fun ty ->
                  List.find_opt
                    (fun { member_name; _ } -> member_name = item_name)
                    ty.descriptor.type_members)
           |> Option.map (fun { member_name; member_value; _ } ->
                  Internal
                    { in_module = page_ref;
                      name = Member (type_name, member_name);
                      definition = member_value.definition
                    })
    | _ -> None

  let in_module_finders = [ in_module_value; in_module_type; in_module_method ]

  (** Find a term in this module. *)
  let find_in_current_page { current_page; _ } name is_type =
    Option.bind current_page @@ fun current_page ->
    CCList.find_map (fun f -> f current_page.descriptor name is_type) in_module_finders

  (** Find a module with this name *)
  let find_module s name is_type =
    if is_type then None
    else
      lookup_module s name
      |> Option.map @@ fun { definition; descriptor; _ } ->
         Internal { in_module = descriptor.page_ref; name = Module; definition }

  (** Finds elements within modules. This tries foo.[bar.baz], then foo.bar.[baz], etc... *)
  let find_member s name is_type =
    let rec go i =
      match String.index_from_opt name i '.' with
      | None -> None
      | Some i ->
          let page_id = CCString.take i name and item_name = CCString.drop (i + 1) name in
          lookup_module s page_id
          |> CCOption.flat_map (fun { descriptor = modu; _ } ->
                 CCList.find_map (fun f -> f modu item_name is_type) in_module_finders)
          |> CCOption.or_lazy ~else_:(fun () -> go (i + 1))
    in
    go 0

  (** Looks up a Lua name *)
  let find_lua_builtin _ name is_type =
    match if is_type then Lua_reference.lookup_type name else Lua_reference.lookup_name name with
    | InManual section ->
        Some (External { name; url = Some (Lua_reference.manual_section section) })
    | Undocumented -> Some (External { name; url = None })
    | Unknown -> None

  let finders = [ find_in_current_page; find_module; find_member; find_lua_builtin ]

  (* Validate the reference is well-formed, resolve it, and return null if we can't *)
  let name context ~types_only name =
    if String.length name > 0 then
      CCList.find_map (fun f -> f context name types_only) finders
      |> Option.value ~default:(Reference.Unknown name)
    else Unknown name

  let ref context ~types_only : reference -> reference = function
    | Unknown n -> name context ~types_only n
    | (External _ | Internal _) as r -> r
end

type cache = Doc_syntax.value Doc_syntax.documented DocTbl.t
type context = Lift.t

let context pages current_page =
  let context = { Resolvers.pages; current_page; unique_pages = StringMap.empty } in
  let with_renaming f x =
    match f x with
    | Internal { name = Module; in_module = { title = Some title; _ }; _ } as x -> (Some title, x)
    | x -> (None, x)
  in
  let lift : Lift.t =
    { any_ref = with_renaming (Resolvers.ref context ~types_only:false);
      type_ref = Resolvers.ref context ~types_only:true
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
      custom_source;
      changes
    } =
  { description = Option.map (Lift.description lift) description;
    descriptor = go_child lift descriptor;
    definition;
    examples = List.map (Lift.example lift) examples;
    see = List.map (Lift.see lift) see;
    local;
    export;
    deprecated = Option.map (Lift.deprecation lift) deprecated;
    custom_source;
    changes = List.map (Lift.change lift) changes
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

let go_page_contents ~cache lift = function
  | Markdown -> Markdown
  | Module { mod_kind; mod_contents; mod_types } ->
      Module
        { mod_kind;
          mod_contents = go_value ~cache lift mod_contents;
          mod_types = List.map (go_documented lift (go_type ~cache)) mod_types
        }

let go_page ~cache lift to_resolve =
  go_documented lift
    (fun lift { page_ref; page_contents } ->
      { page_ref; page_contents = go_page_contents ~cache lift page_contents })
    to_resolve
