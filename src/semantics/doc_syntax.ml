open IlluaminateCore

type reference = Reference.resolved

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Resolved
end)

type 'a documented =
  { description : description option;
    descriptor : 'a;
    definition : Span.t;
    examples : example list;
    see : see list;
    local : bool;
    export : bool;
    deprecated : deprecation option;
    custom_source : position option
  }

type value =
  | Function of
      { args : arg list list;
        rets : return list list;
        throws : description list;
        has_self : bool
      }
  | Table of (string * value documented) list
  | Expr of
      { ty : Type.t;
        value : string option
      }
  | Type of type_info
  | Unknown
  | Undefined

and member =
  { member_name : string;
    member_is_method : bool;
    member_value : value documented
  }

and type_info =
  { type_name : string;
    type_members : member list
  }

and module_info =
  { mod_name : string;
    mod_types : type_info documented list;
    mod_kind : module_kind;
    mod_contents : value
  }

let get_suffix = function
  | Function { args = []; _ } -> "()"
  | Function { args = [ args ]; _ } ->
      let buf = Buffer.create 10 in
      Buffer.add_char buf '(';
      let _, opt =
        List.fold_left
          (fun (i, o) { arg_name; arg_opt; _ } ->
            if arg_opt <> Required then (
              if i > 0 then Buffer.add_char buf ' ';
              Buffer.add_char buf '[' );
            if i > 0 then Buffer.add_string buf ", ";
            Buffer.add_string buf arg_name;
            ( match arg_opt with
            | Default x -> Buffer.add_char buf '='; Buffer.add_string buf x
            | Required | Optional -> () );
            (i + 1, o + if arg_opt <> Required then 1 else 0))
          (0, 0) args
      in
      for _ = 1 to opt do
        Buffer.add_char buf ']'
      done;
      Buffer.add_char buf ')';
      Buffer.contents buf
  | Function _ -> "(...)"
  | Expr { value = Some value; _ } -> " = " ^ value
  | _ -> ""

let is_documented = function
  | { description = None; examples = []; see = []; _ } -> false
  | _ -> true

let base_iter = new abstract_iter

class iter =
  object (self)
    method abstract_syntax : span:Span.t -> abstract_iter = fun ~span:_ -> base_iter

    method value ~span =
      function
      | Unknown | Undefined -> ()
      | Function { args; rets; throws; has_self = _ } ->
          let abs = self#abstract_syntax ~span in
          List.iter (List.iter abs#arg) args;
          List.iter (List.iter abs#return) rets;
          List.iter abs#description throws
      | Table xs -> List.iter (fun (_, v) -> self#documented self#value v) xs
      | Type t -> self#type_info ~span t
      | Expr { ty; value = _ } -> (self#abstract_syntax ~span)#type_ ty

    method documented : 'a. (span:Span.t -> 'a -> unit) -> 'a documented -> unit =
      fun child
          { description;
            descriptor;
            definition;
            examples;
            see;
            deprecated;
            local = _;
            export = _;
            custom_source = _
          } ->
        let abs = self#abstract_syntax ~span:definition in
        Option.iter abs#description description;
        child ~span:definition descriptor;
        List.iter abs#example examples;
        List.iter abs#see see;
        Option.iter abs#deprecation deprecated;
        ()

    method member { member_name = _; member_is_method = _; member_value; _ } =
      self#documented self#value member_value

    method type_info : span:Span.t -> type_info -> unit =
      fun ~span:_ { type_members; type_name = _ } -> List.iter self#member type_members

    method module_info ~span { mod_name = _; mod_types; mod_kind = _; mod_contents } =
      List.iter (self#documented self#type_info) mod_types;
      self#value ~span mod_contents
  end

let iter_of f =
  object
    inherit iter

    method! abstract_syntax = f
  end

module Link = struct
  let definition =
    let open Span in
    let filename = Filename.mk @@ "=[" ^ __MODULE__ ^ ".Link]" in
    let buf = Lexing.from_string "" in
    Span.Lines.using filename buf @@ fun l -> Span.of_pos2 l buf.lex_curr_p buf.lex_curr_p

  let malformed attrs =
    invalid_arg
    @@ Format.asprintf "Malformed attributes: <i:ref %a>_</>"
         Format.(
           pp_print_list (fun out (k, v) ->
               fprintf out "%s=%a" k (pp_print_option pp_print_string) v))
         attrs

  let of_tag attrs description =
    let make r = function
      | [ ("style", Some style) ] ->
          let style =
            match style with
            | "text" -> `Text
            | "code" -> `Code
            | _ -> invalid_arg "Invalid style"
          in
          { link_reference = r;
            link_style = style;
            link_label = { description; description_pos = None }
          }
      | _ -> malformed attrs
    in
    match attrs with
    | ("module", Some in_module) :: ("sec", Some name) :: attrs ->
        let name : Reference.name_of =
          match CCString.chop_prefix ~pre:"v:" name with
          | Some v -> Value v
          | None -> (
            match CCString.chop_prefix ~pre:"ty:" name with
            | None -> invalid_arg ("Unknown name " ^ name)
            | Some t -> Type t )
        in
        make (Internal { in_module; name; definition }) attrs
    | ("module", Some in_module) :: attrs ->
        make (Internal { in_module; name = Module; definition }) attrs
    | ("href", Some url) :: attrs -> make (External { url = Some url; name = "" }) attrs
    | ("link", Some link) :: attrs -> make (Unknown link) attrs
    | attrs -> make (External { url = None; name = "" }) attrs

  let to_tag { link_reference; link_label = { description; _ }; link_style } : Omd.element =
    let style =
      match link_style with
      | `Code -> "code"
      | `Text -> "text"
    in
    let attrs = [ ("style", Some style) ] in
    let make a : Omd.element = Html ("illuaminate:ref", a, description) in
    match link_reference with
    | Internal { in_module; name; _ } -> (
      match Reference.section_of_name name with
      | None -> make (("module", Some in_module) :: attrs)
      | Some s -> make (("module", Some in_module) :: ("sec", Some s) :: attrs) )
    | External { url = Some url; _ } -> make (("href", Some url) :: attrs)
    | External { url = None; _ } -> make attrs
    | Unknown link -> make (("link", Some link) :: attrs)
end
