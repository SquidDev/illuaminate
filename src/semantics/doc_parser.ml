open IlluaminateCore
open Doc_comment
module IntMap = Map.Make (Int)

open struct
  module IArray = Illuaminate.IArray
  module Position_map = Illuaminate.Position_map
end

(* Drop the and [foo!] from references. *)
let trim_reference x =
  match String.index_opt x '!' with
  | None -> x
  | Some i -> CCString.drop (i + 1) x

module Markdown_parser = struct
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  (** Version of trim which removes leading spaces until the first line, and then all remaining line
      breaks. All trailing spaces are removed. This ensures we're compatible with markdown. *)
  let md_trim s =
    let len = String.length s in

    let i =
      let rec ldrop first_line i =
        if i >= len then i
        else
          match String.unsafe_get s i with
          | '\n' -> ldrop false (i + 1)
          | ' ' when first_line -> ldrop true (i + 1)
          | _ -> i
      in
      ldrop true 0
    in
    let j =
      let rec rdrop j = if j > i && is_space (String.unsafe_get s j) then rdrop (j - 1) else j in
      rdrop (len - 1)
    in
    if i = 0 && j = len - 1 then `No_change else if i >= j then `Empty else `Trim (i, j - i + 1)

  let add_label_meta key value label =
    Cmarkit.Label.with_meta (Cmarkit.Label.meta label |> Cmarkit.Meta.add key value) label

  let add_reference refr label =
    add_label_meta Doc_comment.Markdown.reference (Reference refr) label

  let add_admonition = add_label_meta Doc_abstract_syntax.Cmarkit_meta.admonition_level

  let resolver : Cmarkit.Label.resolver = function
    | `Ref (`Link, label, None) ->
        (* We use [text_to_string] here, as we need to be case sensitive. *)
        let key = Cmarkit.Label.text_to_string label in
        let len = String.length key in
        if len > 2 && key.[0] = '`' && key.[len - 1] = '`' then
          Some (add_reference (String.sub key 1 (len - 2)) label)
        else if len >= 1 && key.[0] = '!' then
          match key with
          | "!NOTE" -> Some (add_admonition Note label)
          | "!INFO" -> Some (add_admonition Info label)
          | "!TIP" -> Some (add_admonition Tip label)
          | "!WARNING" -> Some (add_admonition Warning label)
          | "!DANGER" -> Some (add_admonition Danger label)
          | _ -> None
        else None
    | x -> Cmarkit.Label.default_resolver x

  (** Parse a markdown string into a description. *)
  let parse x = Cmarkit.Doc.of_string ~resolver ~strict:false ~locs:true x

  let id_pat =
    let open Re in
    seq [ str "{#"; group (rep1 (alt [ wordc; char '-'; char '.'; char ':' ])); str "}"; eos ]
    |> compile

  (** Default empty code blocks to Lua. *)
  let postprocess ~default_lua doc =
    (* Parse out a trailing Markdown extra ID ({#foo}), returning the ID and the trimmed inline.
       Note we don't support the wider range of attribute syntax (e.g. {.foo}, {x=y}) *)
    let rec get_id = function
      | Cmarkit.Inline.Text (t, m) as n
        when let length = String.length t in
             length > 3 && t.[length - 1] == '}' -> (
        match Re.exec_opt id_pat t with
        | None -> (None, n)
        | Some group ->
            let start = Re.Group.start group 0 in
            let id = Re.Group.get group 1 in
            (* Trim extra trailing whitespace. *)
            let rec rtrim i = if i >= 0 && t.[i] = ' ' then rtrim (i - 1) else i + 1 in
            let t = String.sub t 0 (rtrim (start - 1)) in
            (Some id, Cmarkit.Inline.Text (t, m)))
      | Cmarkit.Inline.Inlines (xs, n) ->
          let id, xs = get_id_list xs in
          (id, Cmarkit.Inline.Inlines (xs, n))
      | n -> (None, n)
    and get_id_list = function
      | [] -> (None, [])
      | [ x ] ->
          let id, x = get_id x in
          (id, [ x ])
      | x :: xs ->
          let id, xs = get_id_list xs in
          (id, x :: xs)
    in

    let inline _ = function
      (* Rewrite [`foo!bar`] nodes to be [`bar`] *)
      | Cmarkit.Inline.Link (l, lm) -> (
          let module L = Cmarkit.Inline.Link in
          let module C = Cmarkit.Inline.Code_span in
          match L.reference l with
          | `Ref ((`Collapsed | `Shortcut), _, target) as reference
            when Cmarkit.Label.meta target |> Cmarkit.Meta.mem Doc_comment.Markdown.reference
                 && String.contains (Cmarkit.Label.key target) '!' ->
              let text =
                match L.text l with
                | Cmarkit.Inline.Code_span (code, cm) ->
                    Cmarkit.Inline.Code_span (C.code code |> trim_reference |> C.of_string, cm)
                | _ -> failwith "Shortcut reference must have a code span"
              in
              Cmarkit.Inline.Link (L.make ~legacy:(L.is_legacy_ref l) text reference, lm)
              |> Cmarkit.Mapper.ret
          | _ -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default
    in

    let block _ = function
      (* Add IDs to our headers. *)
      | Cmarkit.Block.Heading (h, meta) when Option.is_none (Cmarkit.Block.Heading.id h) -> (
          let module H = Cmarkit.Block.Heading in
          let id, inline = Cmarkit.Block.Heading.inline h |> get_id in
          match id with
          | None -> Cmarkit.Mapper.default
          | Some id ->
              let h = H.make ~id:(`Id id) ~layout:(H.layout h) ~level:(H.level h) inline in
              Cmarkit.Mapper.ret (Cmarkit.Block.Heading (h, meta)))
      (* Default indented code blocks to Lua if no language is given. *)
      | Cmarkit.Block.Code_block (c, meta) when default_lua -> (
          let module Cb = Cmarkit.Block.Code_block in
          match Cb.layout c with
          | `Indented ->
              let c =
                Cb.make ~layout:`Indented ~info_string:("lua", Cmarkit.Meta.none) (Cb.code c)
              in
              Cmarkit.Mapper.ret (Cmarkit.Block.Code_block (c, meta))
          | `Fenced _ -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default
    in
    Cmarkit.Mapper.map_doc (Cmarkit.Mapper.make ~inline ~block ()) doc
end

let parse_description ?(default_lua = false) x =
  let doc = Markdown_parser.parse x |> Markdown_parser.postprocess ~default_lua in
  Markdown.Markdown doc

module Tag = struct
  (* TODO: Replace these with separate error types. *)
  let malformed_tag = "doc:malformed-tag"
  let malformed_type = "doc:malformed-type"
  let duplicate_definitions = "doc:duplicate-definitions"
  let bad_index = "doc:bad-index"
  let wrong_tag = "doc:wrong-tag"
end

module Lex = struct
  include Doc_lexer

  type 'a ranged =
    { contents : 'a;
      offset : int;
      mapping : Span.t IntMap.t
    }

  (** Construct a lexer from a list of spanned strings. *)
  let lex_of_lines xs =
    let len = List.fold_left (fun a l -> a + String.length l.Span.value) 0 xs in
    (* TODO: Ideally we could get away without a string buffer here. Instead we could build an array
       of strings, and swap out the buffer between them. *)
    let b = Buffer.create len in
    let _, mapping =
      List.fold_left
        (fun (i, m) { Span.value; span } ->
          if i > 0 then Buffer.add_char b '\n';
          Buffer.add_string b value;
          (i + String.length value + 1, IntMap.add i span m))
        (0, IntMap.empty) xs
    in
    { contents = Buffer.contents b |> Lexing.from_string; mapping; offset = 0 }

  let lex_of_spanned { Span.value; span } =
    { contents = Lexing.from_string value; offset = 0; mapping = IntMap.singleton 0 span }

  let range_of_spanned { Span.value; span } =
    { contents = value; offset = 0; mapping = IntMap.singleton 0 span }

  let mk_span { offset; mapping; _ } start finish =
    let start = start + offset and finish = finish + offset in
    let start_pos, start_span = IntMap.find_last (fun x -> x <= start) mapping
    and finish_pos, finish_span = IntMap.find_last (fun x -> x <= finish) mapping in
    let open Illuaminate.Lens in
    start_span
    |> Span.start_offset ^= ((start_span ^. Span.start_offset) + start - start_pos)
    |> Span.finish_offset ^= ((finish_span ^. Span.start_offset) + finish - finish_pos)

  let mk_span' lbuf start finish =
    let finish = if finish <= start then start else finish - 1 in
    mk_span lbuf start finish

  let to_span ({ contents; _ } as lbuf : string ranged) = mk_span' lbuf 0 (String.length contents)

  let to_comment_lines { offset; mapping; _ } =
    let mapping =
      if offset = 0 then mapping
      else IntMap.fold (fun k v xs -> IntMap.add (k - offset) v xs) mapping IntMap.empty
    in
    Doc_abstract_syntax.Comment_lines.__make mapping

  let to_spanned ({ contents; _ } as lbuf : string ranged) : string Span.spanned =
    { value = contents; span = to_span lbuf }

  let with_span (lbuf : Lexing.lexbuf ranged) parse : 'a Span.spanned =
    let start = lbuf.contents.lex_curr_p.pos_cnum in
    let value = parse lbuf.contents in
    let finish = lbuf.contents.lex_curr_p.pos_cnum in
    { value; span = mk_span' lbuf start finish }

  let with_range (lbuf : Lexing.lexbuf ranged) parse : 'a ranged =
    let start = lbuf.contents.lex_curr_p.pos_cnum in
    let contents = parse lbuf.contents in
    { contents; offset = lbuf.offset + start; mapping = lbuf.mapping }

  let run (lbuf : Lexing.lexbuf ranged) parse = parse lbuf.contents

  let word { offset; mapping; contents } =
    let lbuf = Lexing.from_string contents in
    let word = Doc_lexer.word (Buffer.create 8) lbuf in
    match word with
    | "" -> None
    | _ ->
        Doc_lexer.white lbuf;
        let len = lbuf.lex_curr_p.pos_cnum in
        Some
          ( { offset; mapping; contents = word },
            { offset = offset + len; mapping; contents = CCString.drop len contents } )

  let description ?default_lua (ranged : string ranged) =
    let ranged =
      match Markdown_parser.md_trim ranged.contents with
      | `No_change -> ranged
      | `Empty -> { ranged with contents = "" }
      | `Trim (offset, len) ->
          { ranged with
            offset = ranged.offset + offset;
            contents = String.sub ranged.contents offset len
          }
    in
    { description = parse_description ?default_lua ranged.contents;
      description_pos = to_comment_lines ranged (* FIXME: This is all wrong! *)
    }

  let description' ?default_lua (ranged : string ranged) =
    if CCString.for_all Markdown_parser.is_space ranged.contents then None
    else Some (description ?default_lua ranged)
end

type doc_flag =
  | Marker of string Lex.ranged
  | Named of string Span.spanned * string Lex.ranged

module Build = struct
  type 'a grouped =
    { all : 'a list IntMap.t;
      last : int option
    }

  let empty_group = { all = IntMap.empty; last = None }

  let add_group idx x group =
    let idx =
      match (idx, group.last) with
      | Some idx, _ -> idx
      | None, Some idx -> idx
      | None, None -> 1
    in
    { all =
        IntMap.add idx
          (match IntMap.find_opt idx group.all with
          | None -> [ x ]
          | Some xs -> x :: xs)
          group.all;
      last = Some idx
    }

  let get_group (group : 'a grouped) : 'a list list =
    group.all |> IntMap.bindings |> List.map (fun (_, v) -> List.rev v)

  type t =
    { mutable b_errors : Comment_error.t list;
      (* General. *)
      mutable b_see : see list;
      mutable b_usages : example list;
      mutable b_includes : reference Span.spanned list;
      mutable b_local : bool;
      mutable b_export : bool;
      mutable b_deprecated : deprecation option;
      mutable b_custom_source : position option;
      mutable b_changes : changes;
      (* Functions. *)
      mutable b_args : arg grouped;
      mutable b_rets : return grouped;
      mutable b_throws : description list;
      (* Other *)
      mutable b_module : module_info Span.spanned option;
      mutable b_type : type_info option;
      mutable b_fields : field list
    }

  let create () =
    { b_errors = [];
      b_see = [];
      b_usages = [];
      b_includes = [];
      b_local = false;
      b_export = false;
      b_deprecated = None;
      b_custom_source = None;
      b_changes = [];
      b_args = empty_group;
      b_rets = empty_group;
      b_throws = [];
      b_module = None;
      b_type = None;
      b_fields = []
    }

  let report' b err = b.b_errors <- err :: b.b_errors

  let report b code span f =
    Format.kasprintf
      (fun message -> b.b_errors <- Comment_error { code; span; message } :: b.b_errors)
      f

  let unknown b tag x =
    let span, flag =
      match x with
      | Named ({ span; value }, _) -> (span, value)
      | Marker x -> (Lex.to_span x, x.contents)
    in
    report' b (Unknown_flag { span; tag; flag })

  let parse_arg b arg =
    (None, arg)
    |> List.fold_left @@ fun (idx, arg) flag ->
       let name = arg.arg_name in
       match flag with
       | Marker ({ contents = "opt"; _ } as m) ->
           if arg.arg_opt <> Required then
             report b Tag.malformed_tag (Lex.to_span m) "Parameter '%s' is marked as optional twice"
               name;
           (idx, { arg with arg_opt = Optional })
       | Named ({ value = "opt"; span }, { contents; _ }) ->
           if arg.arg_opt <> Required then
             report b Tag.malformed_tag span "Parameter '%s' is marked as optional twice" name;
           (idx, { arg with arg_opt = Default contents })
       | Named ({ value = "type"; span }, ty) -> (
           if Option.is_some arg.arg_type then
             report b Tag.duplicate_definitions span "Parameter '%s' has multiple types." name;
           match Type_parser.parse ty.contents with
           | Ok ty -> (idx, { arg with arg_type = Some ty })
           | Error msg ->
               (* TODO: Adjust to correct position. *)
               report b Tag.malformed_type (Lex.to_span ty)
                 "Parameter '%s' has malformed type '%s' ('%s')" name ty.contents msg;
               (idx, arg))
       | Marker cont -> (
         match CCInt.of_string cont.contents with
         | Some new_idx ->
             Option.iter
               (fun idx ->
                 report b Tag.duplicate_definitions (Lex.to_span cont)
                   "Parameter '%s' has argument set '%d' and '%d'" name idx new_idx)
               idx;
             (* Check the group is consistent. *)
             (match b.b_args.last with
             | None when new_idx <> 1 ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Parameter '%s' is part of parameter set '%d', but is the first parameter!" name
                   new_idx
             | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Parameter '%s' is part of parameter set '%d', but the previous arg is part of \
                    '%d'"
                   name new_idx idx
             | _ -> ());
             (Some new_idx, arg)
         | None ->
             unknown b (Printf.sprintf "Parameter '%s'" name) flag;
             (idx, arg))
       | Named _ ->
           unknown b (Printf.sprintf "Parameter '%s'" name) flag;
           (idx, arg)

  let parse_return b body =
    (None, body)
    |> List.fold_left @@ fun (idx, ret) flag ->
       match flag with
       | Named ({ value = "type"; span }, ty) -> (
           if Option.is_some ret.ret_type then
             report b Tag.duplicate_definitions span "Return value has multiple types";
           match Type_parser.parse_vararg ty.contents with
           | Ok (many, ty) -> (idx, { ret with ret_type = Some ty; ret_many = many })
           | Error msg ->
               report b Tag.malformed_type (Lex.to_span ty)
                 "Return value has malformed type '%s' ('%s')" ty.contents msg;
               (idx, ret))
       | Marker { contents = "..."; _ } -> (idx, { ret with ret_many = true })
       | Marker cont -> (
         match CCInt.of_string cont.contents with
         | Some new_idx ->
             Option.iter
               (fun idx ->
                 report b Tag.duplicate_definitions (Lex.to_span cont)
                   "Return value is part of set '%d' and '%d'" idx new_idx)
               idx;
             (match b.b_rets.last with
             | None when new_idx <> 1 ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "The first return value should be part of set [1] (is actually '%d')" new_idx
             | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Return value is part of return set '%d', but the previous arg is part of '%d'"
                   new_idx idx
             | _ -> ());
             (Some new_idx, ret)
         | None -> unknown b "Return value" flag; (idx, ret))
       | Named _ -> unknown b "Return value" flag; (idx, ret)

  let parse_change ~change_span ~change_kind b body =
    match Lex.word body with
    | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected version."
    | Some (version, cont) ->
        let change_description = Lex.description' cont in
        b.b_changes <-
          { change_kind; change_version = version.contents; change_span; change_description }
          :: b.b_changes

  let parse_field b field =
    field
    |> List.fold_left @@ fun field flag ->
       let name = field.field_name in
       match flag with
       | Named ({ value = "type"; span }, ty) -> (
           if Option.is_some field.field_type then
             report b Tag.duplicate_definitions span "Field '%s' has multiple types." name;
           match Type_parser.parse ty.contents with
           | Ok ty -> { field with field_type = Some ty }
           | Error msg ->
               (* TODO: Adjust to correct position. *)
               report b Tag.malformed_type (Lex.to_span ty)
                 "Field '%s' has malformed type '%s' ('%s')" name ty.contents msg;
               field)
       | Marker _ | Named _ ->
           unknown b (Printf.sprintf "Parameter '%s'" name) flag;
           field

  (** Extract a {!documented} term from a comment and series of tags. *)
  let rec add_flag b (tag : string Span.spanned) (flags : doc_flag list) (body : string Lex.ranged)
      : unit =
    match tag.value with
    (*******************************
     * General tags
     *******************************)
    | "usage" ->
        List.iter (unknown b "@usage") flags;
        let usage =
          match String.index_opt body.contents '\n' with
          | None -> RawExample (Lex.to_spanned body)
          | Some _ ->
              let desc = Lex.description ~default_lua:true body in
              RichExample desc
        in
        b.b_usages <- usage :: b.b_usages
    | "example" ->
        report b Tag.wrong_tag tag.span "Use @usage instead of '@%s" tag.value;
        add_flag b { tag with value = "usage" } flags body
    | "see" -> (
        List.iter (unknown b "@see") flags;
        match Lex.word body with
        | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected reference name."
        | Some (refr, body) ->
            let see_description = Lex.description' body in
            b.b_see <-
              { see_reference = Reference refr.contents;
                see_label = trim_reference refr.contents;
                see_span = Lex.to_span refr;
                see_description
              }
              :: b.b_see)
    | "include" ->
        List.iter (unknown b "@include") flags;
        b.b_includes <- { value = Reference body.contents; span = Lex.to_span body } :: b.b_includes
    | "local" ->
        List.iter (unknown b "@local") flags;
        (* TODO: Verify not defined as local twice *)
        (* TODO: Handle non-empty body. *)
        b.b_local <- true
    | "export" ->
        (* TODO: As above. *)
        List.iter (unknown b "@export") flags;
        b.b_export <- true
    | "deprecated" ->
        List.iter (unknown b "@deprecated") flags;
        if Option.is_some b.b_deprecated then
          report b Tag.duplicate_definitions tag.span "Duplicate @deprecated tags";
        b.b_deprecated <- Some { deprecation_message = Lex.description' body }
    | "source" ->
        List.iter (unknown b "@source") flags;
        if Option.is_some b.b_deprecated then
          report b Tag.duplicate_definitions tag.span "Duplicate @source tags";

        (* We expect strings of the form [@source path/from/root:2] *)
        let contents = body.contents in
        let path, start_line, end_line =
          match String.rindex_opt contents ':' with
          | None -> (contents, 1, 1)
          | Some pos ->
              let path = CCString.take pos contents and line = CCString.drop (pos + 1) contents in
              let path = CCString.drop_while (fun x -> x = '/') path in
              let line =
                match int_of_string_opt line with
                | None ->
                    report b Tag.duplicate_definitions tag.span "Unknown line number";
                    1
                | Some l -> l
              in
              (path, line, line)
        in
        b.b_custom_source <- Some { path; start_line; end_line }
    | "since" ->
        List.iter (unknown b "@since") flags;
        parse_change ~change_span:tag.span ~change_kind:Added b body
    | "changed" ->
        List.iter (unknown b "@changed") flags;
        parse_change ~change_span:tag.span ~change_kind:Changed b body
    (*******************************
     * Function tags
     *******************************)
    (* Convert @tparam x into @param[type=x] *)
    | "tparam" -> (
      match Lex.word body with
      | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected type."
      | Some (ty, cont) ->
          add_flag b { tag with value = "param" }
            (Named ({ span = Lex.to_span body; value = "type" }, ty) :: flags)
            cont)
    (* Extract the parameter name and then process flags *)
    | "param" ->
        let name, desc =
          match Lex.word body with
          | None ->
              report b Tag.malformed_tag (Lex.to_span body) "Expected argument name.";
              ("?", None)
          | Some (name, body) -> (name.contents, Lex.description' body)
        in
        let idx, arg =
          parse_arg b
            { arg_name = name; arg_opt = Required; arg_type = None; arg_description = desc }
            flags
        in
        b.b_args <- add_group idx arg b.b_args
    (* Convert @treturn x into @return[type=x] *)
    | "treturn" -> (
      match Lex.word body with
      | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected return type."
      | Some (ty, cont) ->
          add_flag b { tag with value = "return" }
            (Named ({ span = Lex.to_span body; value = "type" }, ty) :: flags)
            cont)
    (* And add a return value, processing flags *)
    | "return" ->
        let idx, ret =
          parse_return b
            { ret_type = None; ret_many = false; ret_description = Lex.description' body }
            flags
        in
        b.b_rets <- add_group idx ret b.b_rets
    | "throws" ->
        List.iter (unknown b "Throws annotation") flags;
        b.b_throws <- Lex.description body :: b.b_throws
    | "throw" | "raise" | "raises" ->
        report b Tag.wrong_tag tag.span "Use @throws instead of '@%s" tag.value;
        add_flag b { tag with value = "throws" } flags body
    (*******************************
     * Other types
     *******************************)
    | "module" -> (
        let mod_kind, mod_namespace =
          List.fold_left
            (fun (kind, ns) flag ->
              match flag with
              | Named ({ value = "kind"; _ }, { contents = "library"; _ })
              | Marker { contents = "library"; _ } -> (Some MKLibrary, Some Namespace.library)
              | Named ({ value = "kind"; _ }, { contents = "module"; _ })
              | Marker { contents = "module"; _ } -> (Some MKModule, Some Namespace.module_)
              | Named ({ value = "kind"; _ }, { contents; _ }) ->
                  (Some MKNone, Some (Namespace contents))
              | f -> unknown b "@module" f; (kind, ns))
            (None, None) flags
        in
        match b.b_module with
        | Some { value = { mod_name = inner_name; _ }; _ } ->
            report b Tag.duplicate_definitions tag.span
              "Duplicate @module definitions (named '%s' and '%s')" inner_name body.contents
        | None ->
            b.b_module <-
              Some
                { value = { mod_name = body.contents; mod_kind; mod_namespace }; span = tag.span })
    | "type" -> (
        List.iter (unknown b "@type") flags;
        match Lex.word body with
        | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected type name."
        | Some ({ contents = type_name; _ }, _rest) -> (
          (* TODO: Handle non-empty body. *)
          match b.b_type with
          | Some { type_name = inner_name; _ } ->
              report b Tag.duplicate_definitions tag.span
                "Duplicate @type definitions (named '%s' and '%s')" inner_name type_name
          | None -> b.b_type <- Some { type_name }))
    | "tfield" -> (
      match Lex.word body with
      | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected field type."
      | Some (ty, cont) ->
          add_flag b { tag with value = "field" }
            (Named ({ span = Lex.to_span body; value = "type" }, ty) :: flags)
            cont)
    | "field" ->
        let name, desc =
          match Lex.word body with
          | None ->
              report b Tag.malformed_tag (Lex.to_span body) "Expected field name.";
              ("?", None)
          | Some (name, body) -> (name.contents, Lex.description' body)
        in
        let field =
          parse_field b
            { field_pos = tag.span; field_name = name; field_type = None; field_description = desc }
            flags
        in
        b.b_fields <- field :: b.b_fields
    | _ -> report' b (Unknown_tag { span = tag.span; tag = tag.value })

  let build ~span ~description b =
    { source = span;
      errors = b.b_errors;
      description = Lex.description' description;
      see = List.rev b.b_see;
      examples = List.rev b.b_usages;
      local = b.b_local;
      includes = List.rev b.b_includes;
      export = b.b_export;
      deprecated = b.b_deprecated;
      changes = List.rev b.b_changes;
      custom_source = b.b_custom_source;
      arguments = get_group b.b_args;
      returns = get_group b.b_rets;
      throws = List.rev b.b_throws;
      module_info = b.b_module;
      type_info = b.b_type;
      fields = List.rev b.b_fields
    }
end

module Parse = struct
  let rec gobble_flags ~lbuf xs =
    Lex.(run lbuf white);
    let key = Lex.(with_span lbuf key) in
    let value = Lex.(with_range lbuf @@ value (Buffer.create 8)) in
    let xs =
      match (key.value, value.contents) with
      | Some k, _ ->
          let span = Span.finish_offset.over (fun x -> x - 1) key.span in
          Named ({ span; value = k }, value) :: xs
      | None, "" -> xs (* TODO: Warn. *)
      | None, _ -> Marker value :: xs
    in

    Lex.(run lbuf white);
    match Lex.(run lbuf tag_stop) with
    | Separator -> gobble_flags ~lbuf xs
    | Stop -> xs
    | Unknown -> (* TODO: Warn. *) xs

  (** Parse a doc comment, extracting the description and any tags. *)
  let comment span (lbuf : Lexing.lexbuf Lex.ranged) =
    let gobble_description () =
      let start = lbuf.contents.lex_curr_p.pos_cnum in
      let b = Buffer.create 0 in
      let rec go line =
        let next = Lex.(with_span lbuf maybe_tag) in
        match next.value with
        | Tag t -> (Buffer.contents b, Some { next with value = t })
        | Eof -> (Buffer.contents b, None)
        | NotTag ->
            if line then Buffer.add_char b '\n';
            let line = Lex.(run lbuf until_line) in
            Buffer.add_string b line;
            go Lex.(run lbuf line)
      in
      let description, tag = go false in
      ({ Lex.contents = description; offset = start; mapping = lbuf.mapping }, tag)
    in

    let b = Build.create () in

    let rec gobble_tags tag =
      let flags = if Lex.(run lbuf tag_start) then gobble_flags ~lbuf [] |> List.rev else [] in
      Lex.(run lbuf white);
      let description, next = gobble_description () in
      Build.add_flag b tag flags description;
      Option.iter gobble_tags next
    in

    let description, tag = gobble_description () in
    Option.iter gobble_tags tag; Build.build ~span ~description b

  let markdown attributes (contents : string Span.spanned) =
    let b = Build.create () in
    let add_tag (key, value) =
      let lbuf = Lex.lex_of_spanned value in
      let flags = if Lex.(run lbuf tag_start) then gobble_flags ~lbuf [] |> List.rev else [] in
      Lex.(run lbuf white);
      let line = Lex.(with_range lbuf until_line) in
      Build.add_flag b key flags line
    in
    List.iter add_tag attributes;
    Build.build ~span:contents.span ~description:(Lex.range_of_spanned contents) b
end

(** Utilities for handling indented doc comments. *)
module Indent = struct
  let get line =
    let is_whitespace = function
      | ' ' | '\t' -> true
      | _ -> false
    in
    let len = String.length line in
    let i = ref 0 in
    while !i < len && is_whitespace (String.unsafe_get line !i) do
      incr i
    done;
    if !i >= len then None else Some !i

  (** Get a common indent from all input lines. *)
  let get_common =
    List.fold_left
      (fun acc line ->
        match (get line.Span.value, acc) with
        | None, None -> None
        | (Some _ as x), None | None, (Some _ as x) -> x
        | Some x, Some y -> Some (min x y))
      None

  (** Drop i characters from a string and adjust its span. *)
  let drop i { Span.value; span } =
    { Span.value = (if i >= String.length value then "" else CCString.drop i value);
      span = (span |> Illuaminate.Lens.(Span.start_offset %= fun p -> p + i))
    }

  (** Drop a common indent from all input lines. *)
  let drop_common xs =
    match get_common xs with
    | None | Some 0 -> xs
    | Some i -> List.map (drop i) xs

  (** For input list [x :: xs], drop any amount of indent from the first line, and a common indent
      from all remaining ones. *)
  let drop_rest = function
    | [] -> []
    | x :: xs ->
        let x =
          match get x.Span.value with
          | None | Some 0 -> x
          | Some i -> drop i x
        in
        x :: drop_common xs
end

let split_lines str span =
  let rec build offset xs = function
    | [] -> List.rev xs
    | y :: ys ->
        let len = String.length y in
        let span =
          let open Illuaminate.Lens in
          span
          |> Span.start_offset %= ( + ) offset
          |> Span.start_offset ^= ((span ^. Span.start_offset) + offset)
        in
        build (offset + len + 1) ({ Span.value = y; span } :: xs) ys
  in
  String.split_on_char '\n' str |> build 0 []

(** Extract multiple single-line comments and merge them into one. Returns the last position. *)
let rec extract_block ~root line column lines trivias i =
  if i >= IArray.length trivias then (List.rev lines, (List.hd lines).Span.span, i)
  else
    match IArray.get trivias i with
    (* Skip whitespace *)
    | { Node.Trivia.kind = Whitespace; _ } -> extract_block ~root line column lines trivias (i + 1)
    (* Comments aligned with this one on successive lines are included *)
    | { kind = LineComment; contents; start } as trivia
      when let line', col' = Position_map.position_of (Span.position_map root) start in
           (line' :> int) = line + 1 && (col' :> int) = column ->
        let span =
          Node.Trivia.span root trivia |> Illuaminate.Lens.(Span.start_offset %= fun p -> p + 2)
        in
        let value = CCString.drop 2 contents in
        extract_block ~root (line + 1) column ({ Span.span; value } :: lines) trivias (i + 1)
    | _ -> (List.rev lines, (List.hd lines).span, i)

let get_block_comment_start str =
  let rec worker str i =
    match str.[i] with
    | '=' -> worker str (i + 1)
    | '[' -> i + 1
    | _ -> failwith "Invalid block comment"
  in
  worker str 3

let extract (node : _ Node.t) =
  let root = Node.span node in
  let rec extract_comments cs trivias i =
    if i >= IArray.length trivias then cs
    else
      match IArray.get trivias i with
      | { Node.Trivia.kind = BlockComment; contents; _ } as trivia ->
          let span = Node.Trivia.span root trivia in
          let start = get_block_comment_start contents in
          if contents.[start] == '-' then
            let documented =
              split_lines
                (String.sub contents (start + 1) (String.length contents - start - (start - 1)))
                (Illuaminate.Lens.(Span.start_offset %= fun p -> p + start + 1) span)
              |> Indent.drop_rest |> Lex.lex_of_lines |> Parse.comment span
            in
            extract_comments (documented :: cs) trivias (i + 1)
          else extract_comments cs trivias (i + 1)
      | { kind = LineComment; contents; _ } as trivia
        when String.starts_with ~prefix:"---" contents
             (* Skip comments which start with a line entirely composed of '-'. *)
             && (String.length contents = 3 || CCString.exists (fun x -> x <> '-') contents) ->
          let span = Node.Trivia.span root trivia in
          let lines, last, i =
            extract_block ~root (Span.start_line span) (Span.start_col span)
              [ { span = (span |> Illuaminate.Lens.(Span.start_offset %= fun p -> p + 3));
                  value = CCString.drop 3 contents
                }
              ]
              trivias (i + 1)
          in
          let documented =
            Indent.drop_rest lines |> Lex.lex_of_lines |> Parse.comment (Span.of_span2 span last)
          in
          extract_comments (documented :: cs) trivias i
      | _ -> extract_comments cs trivias (i + 1)
  in
  let open Illuaminate.Lens in
  ( extract_comments [] (node ^. Node.leading_trivia) 0,
    extract_comments [] (node ^. Node.trailing_trivia) 0 |> List.rev )

module Term = struct
  type t = T : 'a Node.t -> t [@@unboxed]

  let hash (T x) = Node.span x |> Span.hash

  let equal (T x) (T y) =
    let xl, xt = (Node.leading_trivia.get x, Node.trailing_trivia.get x) in
    let yl, yt = (Node.leading_trivia.get y, Node.trailing_trivia.get y) in
    xl == yl && xt == yt
end

module TermTbl = Hashtbl.Make (Term)

module Data = struct
  module D = IlluaminateData

  type t =
    { mutable all_comments : (comment list, Syntax.program) result;
      comments : (comment list * comment list) TermTbl.t
    }

  let program =
    D.Programs.key ~name:(__MODULE__ ^ ".program") @@ fun _ _ program ->
    { (* Technically a memory leak here! *)
      all_comments = Error program;
      comments = TermTbl.create 32
    }

  let file =
    D.Programs.file_key ~name:(__MODULE__ ^ ".file") @@ fun data filename contents ->
    match contents with
    | Markdown { attributes; contents } ->
        let c = Parse.markdown attributes contents in
        { all_comments = Ok [ c ]; comments = TermTbl.create 0 }
    | Lua _ -> D.need data program filename |> Option.get

  (** Get the comments before and after a specific node. *)
  let comment node { comments; _ } =
    match TermTbl.find_opt comments (T node) with
    | Some t -> t
    | None ->
        let t = extract node in
        (match t with
        | [], [] -> ()
        | _ -> TermTbl.add comments (T node) t);
        t

  let comments t =
    match t.all_comments with
    | Ok x -> x
    | Error prog ->
        (object
           inherit Syntax.iter

           method! node f x =
             comment x t |> ignore;
             f (Node.contents.get x)
        end)
          #program
          prog;
        let comments =
          TermTbl.to_seq_values t.comments
          |> Seq.flat_map (fun (x, y) -> Seq.flat_map List.to_seq (List.to_seq [ x; y ]))
          |> List.of_seq
        in
        t.all_comments <- Ok comments;
        comments
end
