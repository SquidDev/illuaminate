open IlluaminateCore
open Doc_comment

type doc_flag =
  | Marker of string
  | Named of string * string

let parse_description =
  let open Omd_representation in
  let make link name = Html ("lsp:ref", [ ("link", Some link); ("name", Some name) ], []) in
  (* Gobble everything from | to }. *)
  let rec gobble_name link accum r p = function
    | Newlines 0 :: _ | [] -> None
    | Cbrace :: l ->
        let name = Omd_lexer.string_of_tokens (List.rev accum) in
        Some (make link name :: r, Cbrace :: p, l)
    | Newline :: l -> gobble_name link (Space :: accum) r (Newline :: p) l
    | x :: l -> gobble_name link (x :: accum) r (x :: p) l
  in
  (* Gobble everything from @{ to }. Aborts on newlines, switches to gobble_name should we see a
     '|'. *)
  let rec gobble_link accum r p = function
    | Newline :: _ | [] -> None
    | Cbrace :: l ->
        let link = Omd_lexer.string_of_tokens (List.rev accum) in
        Some (make link link :: r, Cbrace :: p, l)
    | Bar :: l ->
        let link = Omd_lexer.string_of_tokens (List.rev accum) in
        gobble_name link [] r (Bar :: p) l
    | x :: l -> gobble_link (x :: accum) r (x :: p) l
  in
  (* Register an extension which recognises @{foo} and @ foo|bar *)
  let ext : extension =
    object
      method parser_extension r p =
        function
        | At :: Obrace :: l -> gobble_link [] r (Obrace :: At :: p) l
        | _ -> None

      method to_string = "Extension"
    end
  in
  fun ?(default_lang = "") x ->
    Description (x |> String.trim |> Omd.of_string ~extensions:[ ext ] ~default_lang)

module Tag = struct
  let malformed_tag = Error.Tag.make Error.Error "doc:malformed-tag"

  let malformed_type = Error.Tag.make Error.Error "doc:malformed-type"

  let unknown_flag = Error.Tag.make Error.Error "doc:unknown-flag"

  let unknown_tag = Error.Tag.make Error.Error "doc:unknown-tag"

  let duplicate_definitions = Error.Tag.make Error.Error "doc:duplicate-definitions"

  let bad_index = Error.Tag.make Error.Error "doc:bad-index"

  let wrong_throws = Error.Tag.make Error.Error "doc:wrong-throws"

  let all =
    [ malformed_tag;
      malformed_type;
      unknown_flag;
      unknown_tag;
      duplicate_definitions;
      bad_index;
      wrong_throws
    ]
end

(** Extracts a string until a condition is met, but also taking into account balancing of brackets. *)
let match_string terminate str start =
  let n = String.length str in
  let rec find_start pos =
    if pos >= n then pos
    else
      match str.[pos] with
      | ' ' | '\t' | '\n' -> find_start (pos + 1)
      | _ -> pos
  in
  let rec worker pos closes =
    if pos >= n then
      match closes with
      | [] -> Some pos
      | _ -> None
    else
      match (str.[pos], closes) with
      (* Close bracket *)
      | x, c :: cs when x == c -> worker (pos + 1) cs
      | '(', _ -> worker (pos + 1) (')' :: closes)
      | '{', _ -> worker (pos + 1) ('}' :: closes)
      | '[', _ -> worker (pos + 1) (']' :: closes)
      (* End of string *)
      | x, [] when terminate x -> Some pos
      (* Boring character *)
      | _ -> worker (pos + 1) closes
  in
  let start = find_start start in
  match worker start [] with
  | None -> None
  | Some pos -> Some (String.sub str start (pos - start), pos)

(** Extracts a string until observing whitespace, taking into account balancing of brackets. *)
let match_word =
  match_string (function
    | ' ' | '\t' | '\n' -> true
    | _ -> false)

let tag_start = Str.regexp "^[ \t]*@\\([a-zA-Z0-9_]+\\)"

module IntMap = Map.Make (CCInt)

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

(** Parse a doc comment, extracting the description and any tags. *)
let parse comment =
  (* Find the next tag after a position *)
  let find_tag pos =
    try
      let first = Str.search_forward tag_start comment pos in
      Some first
    with Not_found -> None
  in
  match find_tag 0 with
  | None -> (comment, [])
  | Some _ ->
      let description = Str.string_before comment (Str.match_beginning ()) in
      let get_flags start group_end =
        if start >= group_end || comment.[start] <> '[' then ([], start)
        else
          let rec go flags pos =
            if pos >= group_end then (flags, pos)
            else if comment.[pos] = ']' then (flags, pos + 1)
            else
              match match_string (fun x -> x == ',' || x == ']') comment pos with
              | None -> (flags, pos)
              | Some (flag, pos) -> go (Marker (String.trim flag) :: flags) pos
          in
          go [] (start + 1)
      in
      let rec get_tags xs =
        let tag_name = Str.matched_group 1 comment and tag_end = Str.match_end () in
        let group_end =
          match find_tag tag_end with
          | Some fin -> fin
          | None -> String.length comment
        in
        let flags, tag_end = get_flags tag_end group_end in
        let xs =
          (tag_name, flags, String.sub comment tag_end (group_end - tag_end) |> String.trim) :: xs
        in
        if group_end < String.length comment then get_tags xs else List.rev xs
      in
      (description, get_tags [])

type comment_builder =
  { mutable b_unknown : string list;
    mutable b_errors : (Error.Tag.t * string) list;
    (* General. *)
    mutable b_see : see list;
    mutable b_usages : example list;
    mutable b_includes : reference list;
    mutable b_local : bool;
    (* Functions. *)
    mutable b_args : arg grouped;
    mutable b_rets : return grouped;
    mutable b_throws : description list;
    (* Other *)
    mutable b_module : module_info option;
    mutable b_type : type_info option
  }

(** Extract a {!documented} term from a comment and series of tags. *)
let build span (description, (tags : (string * doc_flag list * string) list)) =
  (* Temporary state for error reporting *)
  let b =
    { b_unknown = [];
      b_errors = [];
      b_see = [];
      b_usages = [];
      b_includes = [];
      b_local = false;
      b_args = empty_group;
      b_rets = empty_group;
      b_throws = [];
      b_module = None;
      b_type = None
    }
  in
  let report tag x = b.b_errors <- (tag, x) :: b.b_errors in
  let unknown name (Named (flag, _) | Marker flag) =
    Printf.sprintf "%s has unknown flag '%s'" name flag |> report Tag.unknown_flag
  in
  let rec tag_worker = function
    (*******************************
     * General tags
     *******************************)
    | "usage", flags, body ->
        List.iter (unknown "@usage") flags;
        let usage =
          match String.index_opt body '\n' with
          | None -> RawExample body
          | Some _ -> RichExample (parse_description ~default_lang:"lua" body)
        in
        b.b_usages <- usage :: b.b_usages
    | "see", flags, body ->
        List.iter (unknown "@see") flags;
        (* TODO: Split into reference and (optional) description *)
        b.b_see <- { see_reference = Reference body; see_description = None } :: b.b_see
    | "include", flags, body ->
        List.iter (unknown "@include") flags;
        b.b_includes <- Reference body :: b.b_includes
    | "local", flags, "" ->
        List.iter (unknown "@local") flags;
        (* TODO: Verify not defined as local twice *)
        (* TODO: Handle non-empty body. *)
        b.b_local <- true
    (*******************************
     * Function tags
     *******************************)
    (* Convert @tparam x into @param[type=x] *)
    | "tparam", flags, body -> (
      match match_word body 0 with
      | None ->
          Printf.sprintf "Expected type for parameter (from body '%s')" (String.escaped body)
          |> report Tag.malformed_tag
      | Some (ty, cont) ->
          tag_worker ("param", Named ("type", ty) :: flags, Str.string_after body cont) )
    (* Extract the parameter name and then process flags *)
    | "param", flags, body -> (
      match match_word body 0 with
      | None ->
          Printf.sprintf "Expected method name for @tparam with '%s'" (String.escaped body)
          |> report Tag.malformed_tag
      | Some (name, cont) ->
          let idx, arg =
            List.fold_left
              (fun (idx, arg) flag ->
                match flag with
                | Marker "opt" ->
                    if arg.arg_opt then
                      Printf.sprintf "Parameter '%s' is marked as optional twice" name
                      |> report Tag.malformed_tag;
                    (idx, { arg with arg_opt = true })
                | Named ("type", ty) -> (
                    Option.iter
                      (fun _ ->
                        Printf.sprintf "Parameter '%s' has multiple types." name
                        |> report Tag.duplicate_definitions)
                      arg.arg_type;
                    match Type_parser.parse ty with
                    | Ok ty -> (idx, { arg with arg_type = Some ty })
                    | Error msg ->
                        Printf.sprintf "Parameter '%s' has malformed type '%s' ('%s')" name ty msg
                        |> report Tag.malformed_type;
                        (idx, arg) )
                | Marker cont -> (
                  match CCInt.of_string cont with
                  | Some new_idx ->
                      Option.iter
                        (fun idx ->
                          Printf.sprintf "Parameter '%s' has argument set '%d' and '%d'" name idx
                            new_idx
                          |> report Tag.duplicate_definitions)
                        idx;
                      ( match b.b_args.last with
                      | None when new_idx <> 1 ->
                          Printf.sprintf
                            "Parameter '%s' is part of parameter set '%d', but is the first \
                             parameter!"
                            name new_idx
                          |> report Tag.bad_index
                      | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                          Printf.sprintf
                            "Parameter '%s' is part of parameter set '%d', but the previous arg is \
                             part of '%d'"
                            name new_idx idx
                          |> report Tag.bad_index
                      | _ -> () );
                      (Some new_idx, arg)
                  | None ->
                      unknown (Printf.sprintf "Parameter '%s'" name) flag;
                      (idx, arg) )
                | Named _ ->
                    unknown (Printf.sprintf "Parameter '%s'" name) flag;
                    (idx, arg))
              ( None,
                { arg_name = name;
                  arg_opt = false;
                  arg_type = None;
                  arg_description = Some (Str.string_after body cont |> parse_description)
                } )
              flags
          in
          b.b_args <- add_group idx arg b.b_args )
    (* Convert @treturn x into @return[type=x] *)
    | "treturn", flags, body -> (
      match match_word body 0 with
      | None ->
          Printf.sprintf "Expected type for return (from body '%s')" (String.escaped body)
          |> report Tag.malformed_tag
      | Some (ty, cont) ->
          tag_worker ("return", Named ("type", ty) :: flags, Str.string_after body cont) )
    (* And add a return value, processing flags *)
    | "return", flags, body ->
        let idx, ret =
          List.fold_left
            (fun (idx, ret) flag ->
              match flag with
              | Named ("type", ty) -> (
                  Option.iter
                    (fun _ -> "Return value has multiple types" |> report Tag.duplicate_definitions)
                    ret.ret_type;
                  match Type_parser.parse_vararg ty with
                  | Ok (many, ty) -> (idx, { ret with ret_type = Some ty; ret_many = many })
                  | Error msg ->
                      Printf.sprintf "Return value has malformed type '%s' ('%s')" ty msg
                      |> report Tag.malformed_type;
                      (idx, ret) )
              | Marker cont -> (
                match CCInt.of_string cont with
                | Some new_idx ->
                    Option.iter
                      (fun idx ->
                        Printf.sprintf "Return value is part of set '%d' and '%d'" idx new_idx
                        |> report Tag.duplicate_definitions)
                      idx;
                    ( match b.b_rets.last with
                    | None when new_idx <> 1 ->
                        Printf.sprintf
                          "The first return value should be part of set [1] (is actually '%d')"
                          new_idx
                        |> report Tag.bad_index
                    | Some idx when new_idx <> idx + 1 && new_idx <> idx ->
                        Printf.sprintf
                          "Return value is part of return set '%d', but the previous arg is part \
                           of '%d'"
                          new_idx idx
                        |> report Tag.bad_index
                    | _ -> () );
                    (Some new_idx, ret)
                | None -> unknown "Return value" flag; (idx, ret) )
              | Named _ -> unknown "Return value" flag; (idx, ret))
            ( None,
              { ret_type = None; ret_many = false; ret_description = Some (parse_description body) }
            )
            flags
        in
        b.b_rets <- add_group idx ret b.b_rets
    | "throws", flags, body ->
        List.iter (unknown "Throws annotation") flags;
        b.b_throws <- parse_description body :: b.b_throws
    | (("throw" | "raise" | "raises") as tag), flags, body ->
        Printf.sprintf "Use @throws instead of '@%s" tag |> report Tag.wrong_throws;
        tag_worker ("throws", flags, body)
    (*******************************
     * Other types
     *******************************)
    | "module", flags, name -> (
        List.iter (unknown "@module") flags;
        match b.b_module with
        | Some { mod_name = inner_name; _ } ->
            Printf.sprintf "Duplicate @module definitions (named '%s' and '%s')" inner_name name
            |> report Tag.duplicate_definitions
        | None -> b.b_module <- Some { mod_name = name; mod_kind = Module } )
    | "type", flags, name -> (
        List.iter (unknown "@type") flags;
        match b.b_module with
        | Some { mod_name = inner_name; _ } ->
            Printf.sprintf "Duplicate @module definitions (named '%s' and '%s')" inner_name name
            |> report Tag.duplicate_definitions
        | None -> b.b_module <- Some { mod_name = name; mod_kind = Module } )
    | tag, _, _ -> b.b_unknown <- tag :: b.b_unknown
  in
  List.iter tag_worker tags;
  ( match List.sort_uniq String.compare b.b_unknown with
  | [] -> ()
  | [ x ] -> "Using unknown tag @" ^ x |> report Tag.unknown_tag
  | xs -> "Using unknown tags @" ^ String.concat ", @" xs |> report Tag.unknown_tag );
  { source = span;
    errors = b.b_errors;
    description =
      ( match description with
      | "" -> None
      | _ -> Some (parse_description description) );
    see = List.rev b.b_see;
    examples = List.rev b.b_usages;
    local = b.b_local;
    includes = List.rev b.b_includes;
    arguments = get_group b.b_args;
    returns = get_group b.b_rets;
    throws = List.rev b.b_throws;
    module_info = b.b_module;
    type_info = b.b_type
  }

let extract node =
  (* Add a substring to the buffer, dropping the first character if it is whitespace. This is a
     little naive - it'd be better to trim if all lines have the same indent. *)
  let add_trimmed buffer str start =
    if String.length str > start && str.[start] == ' ' then
      Buffer.add_substring buffer str (start + 1) (String.length str - start - 1)
    else Buffer.add_substring buffer str start (String.length str - start)
  in
  (* Extract multiple single-line comments and merge them into one. Returns the last position. *)
  let rec extract_block buffer last line column = function
    | { Span.value = Node.Whitespace _; _ } :: xs ->
        (* Skip whitespace *)
        extract_block buffer last line column xs
    | { Span.value = Node.LineComment c; span = { start_line = sl; start_col = sc; _ } as span }
      :: xs
      when sl = line + 1 && sc = column ->
        (* Comments aligned with this one on successive lines are included *)
        Buffer.add_char buffer '\n';
        add_trimmed buffer c 0;
        extract_block buffer span sl column xs
    | xs -> (last, xs)
  in
  (* Extract all comments before this token *)
  let rec extract_comments cs = function
    | [] -> cs
    | { Span.value = Node.BlockComment (_, c); span } :: xs when String.length c > 0 && c.[0] == '-'
      ->
        let documented = parse c |> build span in
        extract_comments (documented :: cs) xs
    | { Span.value = Node.LineComment c; span } :: xs when String.length c > 0 && c.[0] == '-' ->
        let buffer = Buffer.create 16 in
        let last, xs = extract_block buffer span span.start_line span.start_col xs in
        let documented = Buffer.contents buffer |> parse |> build (Span.of_span2 span last) in
        extract_comments (documented :: cs) xs
    | _ :: xs -> extract_comments cs xs
  in
  let open Lens in
  ( extract_comments [] (node ^. Node.leading_trivia),
    extract_comments [] (node ^. Node.trailing_trivia) |> List.rev )

module Term = struct
  type t = Wrap : 'a Node.t -> t

  let hash (Wrap x) = Hashtbl.hash x

  let equal (Wrap a) (Wrap b) = Obj.repr a == Obj.repr b
end

module TermTbl = Hashtbl.Make (Term)

module Data = struct
  type t =
    { mutable all_comments : (comment list, Syntax.program) result;
      mutable comments : (comment list * comment list) TermTbl.t
    }

  let key =
    Data.key ~name:__MODULE__ (fun _ program ->
        { (* Technically a memory leak here! *)
          all_comments = Error program;
          comments = TermTbl.create 32
        })

  (** Get the comments before and after a specific node. *)
  let comment node { comments; _ } =
    match TermTbl.find_opt comments (Term.Wrap node) with
    | Some t -> t
    | None ->
        let t = extract node in
        ( match t with
        | [], [] -> ()
        | _ -> TermTbl.add comments (Term.Wrap node) t );
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
