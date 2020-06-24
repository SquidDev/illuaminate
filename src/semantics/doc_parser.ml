open IlluaminateCore
open Doc_comment
module IntMap = Map.Make (Int)

(** Parse a markdown string into a description. *)
let parse_description =
  let desc x = { description = x; description_pos = None } in
  let open Omd_representation in
  (* Gobble everything from | to }. *)
  let rec gobble_name link accum r p = function
    | Newlines 0 :: _ | [] -> None
    | Cbrace :: l ->
        let name = Omd_lexer.string_of_tokens (List.rev accum) in
        let tag =
          Link.to_tag
            { link_reference = Reference link; link_label = desc [ Text name ]; link_style = `Text }
        in
        Some (tag :: r, Cbrace :: p, l)
    | Newline :: l -> gobble_name link (Space :: accum) r (Newline :: p) l
    | x :: l -> gobble_name link (x :: accum) r (x :: p) l
  in
  (* Gobble everything from @{ to }. Aborts on newlines, switches to gobble_name should we see a
     '|'. *)
  let rec gobble_link accum r p = function
    | Newline :: _ | [] -> None
    | Cbrace :: l ->
        let link = Omd_lexer.string_of_tokens (List.rev accum) in
        let tag =
          Link.to_tag
            { link_reference = Reference link; link_label = desc [ Text link ]; link_style = `Code }
        in
        Some (tag :: r, Cbrace :: p, l)
    | Bar :: l ->
        let link = Omd_lexer.string_of_tokens (List.rev accum) in
        gobble_name link [] r (Bar :: p) l
    | x :: l -> gobble_link (x :: accum) r (x :: p) l
  in
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  (* Register an extension which recognises [[@{ foo }]], [[@{ foo|bar }]] and hex colours
     ([[#fff]]) *)
  let ext : extension =
    object
      method parser_extension r p =
        function
        | At :: Obrace :: l -> gobble_link [] r (Obrace :: At :: p) l
        | Hash :: (Word w | Number w) :: l
          when (String.length w = 3 || String.length w == 6) && CCString.for_all is_hex w ->
            Some
              ( Html ("illuaminate:colour", [ ("colour", Some w) ], [ Text ("#" ^ w) ]) :: r,
                Word w :: Hash :: p,
                l )
        | _ -> None

      method to_string = "Extension"
    end
  in
  let fix_lang =
    Omd_representation.visit @@ function
    | Code_block ("__auto", contents) -> Some [ Code_block ("lua", contents) ]
    | Code ("__auto", contents) -> Some [ Code ("", contents) ]
    | _ -> None
  in

  fun ?(default_lang = "") x ->
    x |> String.trim |> Omd.of_string ~extensions:[ ext ] ~default_lang |> fix_lang

module Tag = struct
  let malformed_tag = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:malformed-tag"

  let malformed_type = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:malformed-type"

  let unknown_flag = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:unknown-flag"

  let unknown_tag = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:unknown-tag"

  let duplicate_definitions =
    Error.Tag.make ~attr:[ Default ] ~level:Error "doc:duplicate-definitions"

  let bad_index = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:bad-index"

  let wrong_tag = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:wrong-tag"

  let all =
    [ bad_index;
      duplicate_definitions;
      malformed_tag;
      malformed_type;
      unknown_flag;
      unknown_tag;
      wrong_tag
    ]
end

module Lex = struct
  include Doc_lexer

  type 'a ranged =
    { contents : 'a;
      offset : int;
      mapping : Span.t IntMap.t
    }

  let lex_of_ranged { contents; offset; mapping } =
    let contents = Lexing.from_string contents in
    { contents; offset; mapping }

  (** Construct a lexer from a list of spanned strings. *)
  let lex_of_lines xs =
    let len = List.fold_left (fun a l -> a + String.length l.Span.value) 0 xs in
    (* TODO: Ideally we could get away without a string buffer here. Instead we can build an array
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

  let mk_span { offset; mapping; _ } start finish =
    let start = start + offset and finish = finish + offset in
    let start_pos, start_span = IntMap.find_last (fun x -> x <= start) mapping
    and finish_pos, finish_span = IntMap.find_last (fun x -> x <= finish) mapping in
    let open Lens in
    start_span
    |> Span.start_offset ^= ((start_span ^. Span.start_offset) + start - start_pos)
    |> Span.finish_offset ^= ((finish_span ^. Span.start_offset) + finish - finish_pos)

  let mk_span' lbuf start finish =
    let finish = if finish <= start then start else finish - 1 in
    mk_span lbuf start finish

  let to_span ({ contents; _ } as lbuf : string ranged) = mk_span' lbuf 0 (String.length contents)

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

  let description ?default_lang (ranged : string ranged) =
    { description = parse_description ?default_lang ranged.contents;
      description_pos = Some (to_span ranged)
    }

  let description' ?default_lang (ranged : string ranged) =
    match ranged.contents with
    | "" -> None
    | _ -> Some (description ?default_lang ranged)
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
          ( match IntMap.find_opt idx group.all with
          | None -> [ x ]
          | Some xs -> x :: xs )
          group.all;
      last = Some idx
    }

  let get_group (group : 'a grouped) : 'a list list =
    group.all |> IntMap.bindings |> List.map (fun (_, v) -> List.rev v)

  type t =
    { mutable b_errors : (Error.Tag.t * Span.t * string) list;
      (* General. *)
      mutable b_see : see list;
      mutable b_usages : example list;
      mutable b_includes : reference Span.spanned list;
      mutable b_local : bool;
      mutable b_export : bool;
      mutable b_deprecated : deprecation option;
      (* Functions. *)
      mutable b_args : arg grouped;
      mutable b_rets : return grouped;
      mutable b_throws : description list;
      (* Other *)
      mutable b_module : module_info Span.spanned option;
      mutable b_type : type_info option
    }

  let create () =
    { b_errors = [];
      b_see = [];
      b_usages = [];
      b_includes = [];
      b_local = false;
      b_export = false;
      b_deprecated = None;
      b_args = empty_group;
      b_rets = empty_group;
      b_throws = [];
      b_module = None;
      b_type = None
    }

  let report b tag span f = Format.kasprintf (fun x -> b.b_errors <- (tag, span, x) :: b.b_errors) f

  let unknown b name x =
    let span, value =
      match x with
      | Named ({ span; value }, _) -> (span, value)
      | Marker x -> (Lex.to_span x, x.contents)
    in
    report b Tag.unknown_flag span "%s has unknown flag '%s'" name value

  let parse_arg b arg =
    (None, arg)
    |> List.fold_left @@ fun (idx, arg) flag ->
       let name = arg.arg_name in
       match flag with
       | Marker ({ contents = "opt"; _ } as m) ->
           if arg.arg_opt then
             report b Tag.malformed_tag (Lex.to_span m) "Parameter '%s' is marked as optional twice"
               name;
           (idx, { arg with arg_opt = true })
       | Named ({ value = "type"; span }, ty) -> (
           if Option.is_some arg.arg_type then
             report b Tag.duplicate_definitions span "Parameter '%s' has multiple types." name;
           match Type_parser.parse ty.contents with
           | Ok ty -> (idx, { arg with arg_type = Some ty })
           | Error msg ->
               (* TODO: Adjust to correct position. *)
               report b Tag.malformed_type (Lex.to_span ty)
                 "Parameter '%s' has malformed type '%s' ('%s')" name ty.contents msg;
               (idx, arg) )
       | Marker cont -> (
         match CCInt.of_string cont.contents with
         | Some new_idx ->
             Option.iter
               (fun idx ->
                 report b Tag.duplicate_definitions (Lex.to_span cont)
                   "Parameter '%s' has argument set '%d' and '%d'" name idx new_idx)
               idx;
             (* Check the group is consistent. *)
             ( match b.b_args.last with
             | None when new_idx <> 1 ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Parameter '%s' is part of parameter set '%d', but is the first parameter!" name
                   new_idx
             | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Parameter '%s' is part of parameter set '%d', but the previous arg is part of \
                    '%d'"
                   name new_idx idx
             | _ -> () );
             (Some new_idx, arg)
         | None ->
             unknown b (Printf.sprintf "Parameter '%s'" name) flag;
             (idx, arg) )
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
               (idx, ret) )
       | Marker cont -> (
         match CCInt.of_string cont.contents with
         | Some new_idx ->
             Option.iter
               (fun idx ->
                 report b Tag.duplicate_definitions (Lex.to_span cont)
                   "Return value is part of set '%d' and '%d'" idx new_idx)
               idx;
             ( match b.b_rets.last with
             | None when new_idx <> 1 ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "The first return value should be part of set [1] (is actually '%d')" new_idx
             | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                 report b Tag.bad_index (Lex.to_span cont)
                   "Return value is part of return set '%d', but the previous arg is part of '%d'"
                   new_idx idx
             | _ -> () );
             (Some new_idx, ret)
         | None -> unknown b "Return value" flag; (idx, ret) )
       | Named _ -> unknown b "Return value" flag; (idx, ret)

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
              let desc = Lex.description ~default_lang:"__auto" body in
              RichExample desc
        in
        b.b_usages <- usage :: b.b_usages
    | "example" ->
        report b Tag.wrong_tag tag.span "Use @usage instead of '@%s" tag.value;
        add_flag b { tag with value = "usage" } flags body
    | "see" -> (
        List.iter (unknown b "@see") flags;
        match Lex.word body with
        | None -> report b Tag.malformed_tag (Lex.to_span body) "Expected type name."
        | Some (refr, body) ->
            let see_description = Lex.description' body in
            b.b_see <-
              { see_reference = Reference refr.contents;
                see_label = refr.contents;
                see_span = Lex.to_span refr;
                see_description
              }
              :: b.b_see )
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
            cont )
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
            { arg_name = name; arg_opt = false; arg_type = None; arg_description = desc }
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
            cont )
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
        let mod_kind =
          List.fold_left
            (fun kind flag ->
              match flag with
              | Named ({ value = "kind"; _ }, { contents = "library"; _ })
              | Marker { contents = "library"; _ } ->
                  Some Library
              | Named ({ value = "kind"; _ }, { contents = "module"; _ })
              | Marker { contents = "module"; _ } ->
                  Some Module
              | Named ({ value = "kind"; _ }, { contents; _ }) -> Some (Custom contents)
              | f -> unknown b "@module" f; kind)
            None flags
        in
        match b.b_module with
        | Some { value = { mod_name = inner_name; _ }; _ } ->
            report b Tag.duplicate_definitions tag.span
              "Duplicate @module definitions (named '%s' and '%s')" inner_name body.contents
        | None ->
            b.b_module <- Some { value = { mod_name = body.contents; mod_kind }; span = tag.span } )
    | "type" -> (
        List.iter (unknown b "@type") flags;
        match b.b_type with
        | Some { type_name = inner_name; _ } ->
            report b Tag.duplicate_definitions tag.span
              "Duplicate @type definitions (named '%s' and '%s')" inner_name body.contents
        | None -> b.b_type <- Some { type_name = body.contents } )
    | _ -> report b Tag.unknown_tag tag.span "Unknown tag @%s" tag.value
end

(** Parse a doc comment, extracting the description and any tags. *)
let parse span lbuf =
  let rec gobble_flags xs =
    Lex.(run lbuf white);
    let key = Lex.(with_span lbuf key) in
    let value = Lex.(with_range lbuf @@ value (Buffer.create 8)) in
    let xs =
      match (key.value, value.contents) with
      | Some k, _ -> Named ({ key with value = k }, value) :: xs
      | None, "" -> xs (* TODO: Warn. *)
      | None, _ -> Marker value :: xs
    in

    Lex.(run lbuf white);
    match Lex.(run lbuf tag_stop) with
    | Separator -> gobble_flags xs
    | Stop -> xs
    | Unknown -> (* TODO: Warn. *) xs
  in

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
    let flags = if Lex.(run lbuf tag_start) then gobble_flags [] |> List.rev else [] in
    Lex.(run lbuf white);
    let description, next = gobble_description () in
    Build.add_flag b tag flags description;
    Option.iter gobble_tags next
  in

  let description, tag = gobble_description () in
  Option.iter gobble_tags tag;
  { source = span;
    errors = b.b_errors;
    description = Lex.description' description;
    see = List.rev b.b_see;
    examples = List.rev b.b_usages;
    local = b.b_local;
    includes = List.rev b.b_includes;
    export = b.b_export;
    deprecated = b.b_deprecated;
    arguments = Build.get_group b.b_args;
    returns = Build.get_group b.b_rets;
    throws = List.rev b.b_throws;
    module_info = b.b_module;
    type_info = b.b_type
  }

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
      span = (span |> Lens.(Span.start_offset %= fun p -> p + i))
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

(** Extract multiple single-line comments and merge them into one. Returns the last position. *)
let rec extract_block line column lines = function
  (* Skip whitespace *)
  | { Span.value = Node.Whitespace _; _ } :: xs -> extract_block line column lines xs
  (* Comments aligned with this one on successive lines are included *)
  | { Span.value = Node.LineComment value; span } :: xs
    when Span.start_line span = line + 1 && Span.start_col.get span = column ->
      let span = span |> Lens.(Span.start_offset %= fun p -> p + 2) in
      extract_block (line + 1) column ({ Span.span; value } :: lines) xs
  | xs -> (List.rev lines, (List.hd lines).span, xs)

let extract node =
  (* Extract all comments before this token *)
  let rec extract_comments cs = function
    | [] -> cs
    | { Span.value = Node.BlockComment (n, c); span } :: xs when String.length c > 0 && c.[0] == '-'
      ->
        let lbuf =
          Lex.lex_of_ranged
            { contents = CCString.drop 1 c;
              offset = 0;
              mapping = IntMap.singleton 0 (Lens.(Span.start_offset %= fun p -> p + n + 5) span)
            }
        in
        let documented = parse span lbuf in
        extract_comments (documented :: cs) xs
    | { Span.value = Node.LineComment c; span } :: xs
      when String.length c > 0
           && c.[0] == '-'
           && (* Skip comments which start with a line entirely composed of '-'. *)
           (String.length c = 1 || CCString.exists (fun x -> x <> '-') c) ->
        let lines, last, xs =
          extract_block (Span.start_line span) (Span.start_col.get span)
            [ { span = (span |> Lens.(Span.start_offset %= fun p -> p + 3));
                value = CCString.drop 1 c
              }
            ]
            xs
        in
        let documented =
          Indent.drop_rest lines |> Lex.lex_of_lines |> parse (Span.of_span2 span last)
        in
        extract_comments (documented :: cs) xs
    | _ :: xs -> extract_comments cs xs
  in
  let open Lens in
  ( extract_comments [] (node ^. Node.leading_trivia),
    extract_comments [] (node ^. Node.trailing_trivia) |> List.rev )

module Term = struct
  type t = Node.trivial Span.spanned list * Node.trivial Span.spanned list

  let hash (x, y) = (Hashtbl.hash x * 31) + Hashtbl.hash y

  let equal (al, at) (bl, bt) = al == bl && at == bt
end

module TermTbl = Hashtbl.Make (Term)

module Data = struct
  type t =
    { mutable all_comments : (comment list, Syntax.program) result;
      mutable comments : (comment list * comment list) TermTbl.t
    }

  let key =
    IlluaminateData.Programs.key ~name:__MODULE__ (fun _ program ->
        { (* Technically a memory leak here! *)
          all_comments = Error program;
          comments = TermTbl.create 32
        })

  (** Get the comments before and after a specific node. *)
  let comment node { comments; _ } =
    let key = (Node.leading_trivia.get node, Node.trailing_trivia.get node) in
    match TermTbl.find_opt comments key with
    | Some t -> t
    | None ->
        let t = extract node in
        ( match t with
        | [], [] -> ()
        | _ -> TermTbl.add comments key t );
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
          #program prog;
        let comments =
          TermTbl.to_seq_values t.comments
          |> Seq.flat_map (fun (x, y) -> Seq.flat_map List.to_seq (List.to_seq [ x; y ]))
          |> List.of_seq
        in
        t.all_comments <- Ok comments;
        comments
end
