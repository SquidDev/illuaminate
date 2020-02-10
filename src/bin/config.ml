open Stdlib
open IlluaminateCore
open IlluaminateConfig
open IlluaminateLint
module Pattern = IlluaminatePattern
module TagSet = Set.Make (Error.Tag)

(** A specification of a tag. Either all tags, or specific one. *)
type tag_spec =
  | All
  | Tag of Error.Tag.t

(** An action to modify a set of tags. One may either add or remove items from the set. *)
type tag_set_mod =
  | Include of tag_spec
  | Exclude of tag_spec

(** Configuration for a specific file pattern. *)
type pat_config =
  { pattern : Pattern.t list;
    tags : tag_set_mod list;  (** Modifications to apply to the tag set. *)
    linter_options : Schema.store  (** Modifications to the linter. *)
  }

type doc_options =
  { site_title : string option;
    index : Fpath.t option;
    destination : Fpath.t;
    source_link : Span.t -> string option
  }

let doc_options_term =
  let open Term in
  let option ~ty (parse, print) =
    Converter.atom ~ty
      (function
        | ":none" -> Ok None
        | x -> parse x |> Result.map Option.some)
      (function
        | None -> ":none"
        | Some x -> print x)
  and base ~ty (parse, print) = Converter.atom ~ty parse print
  and string = (Result.ok, Fun.id)
  and path =
    ( (fun p ->
        CCString.drop_while (fun x -> x = '/') p
        |> Fpath.of_string
        |> Result.map_error (fun (`Msg msg) -> msg)),
      Fpath.to_string )
  and template =
    ( (fun p ->
        Lexing.from_string p
        |> Lex_template.main (Buffer.create 8) []
        |> CCResult.flat_map @@ fun r ->
           let rec go = function
             | [] -> Ok r
             | Lex_template.Raw _ :: xs -> go xs
             | Key ("path" | "line" | "sline" | "eline" | "commit") :: xs -> go xs
             | Key k :: _ -> Error (Printf.sprintf "Unknown key %S" k)
           in
           go r),
      Format.asprintf "%a" (fun out -> List.iter (Lex_template.pp out)) )
  in
  let+ site_title =
    field ~name:"title" ~comment:"A title to display for the site" ~default:None
      (option ~ty:"string" string)
  and+ index =
    field ~name:"index" ~comment:"A path to an index file." ~default:None (option ~ty:"path" path)
  and+ destination =
    field ~name:"destination" ~comment:"The folder to write to" ~default:(Fpath.v "doc")
      (base ~ty:"path" path)
  and+ source_link =
    field ~name:"source-link"
      ~comment:
        "A link to an website containing hosting code. The URL is a templated string, where \
         `${foo}` is replaced by the contents of `foo` variable.\n\n\
         This accepts the following variables:\n\
        \ - path: The documented source's path, relative to the project root.\n\
        \ - sline/eline: The start and end line of the variable's definition.\n\
        \ - commit: The current commit hash, as returned by git rev-parse HEAD." ~default:None
      (option ~ty:"template" template)
  in
  fun root ->
    let git_commit =
      lazy
        ( match
            IlluaminateExec.exec "git" [| "git"; "-C"; Fpath.to_string root; "rev-parse"; "HEAD" |]
          with
        | Error e ->
            Printf.eprintf "Cannot find git commit (%s)\n%!" e;
            None
        | Ok l -> Some (String.trim l) )
    in
    let source_link (span : Span.t) =
      source_link
      |> CCOpt.flat_map @@ fun template ->
         let out = Buffer.create 32 in
         let rec go = function
           | [] -> Some (Buffer.contents out)
           | Lex_template.Raw x :: xs -> Buffer.add_string out x; go xs
           | Key "path" :: xs -> (
             match Fpath.(v span.filename.path |> relativize ~root) with
             | None -> None
             | Some x ->
                 Fpath.to_string x |> Buffer.add_string out;
                 go xs )
           | Key ("line" | "sline") :: xs ->
               string_of_int span.start_line |> Buffer.add_string out;
               go xs
           | Key "eline" :: xs ->
               string_of_int span.finish_line |> Buffer.add_string out;
               go xs
           | Key "commit" :: xs -> (
             match Lazy.force git_commit with
             | None -> None
             | Some commit -> Buffer.add_string out commit; go xs )
           | Key _ :: _ -> None
         in

         go template
    in
    { site_title;
      index = Option.map (Fpath.append root) index;
      destination = Fpath.append root destination;
      source_link
    }

let doc_options = Category.add doc_options_term IlluaminateSemantics.Doc.Extract.Config.workspace

(** The main config file. *)
type t =
  | Config of
      { root : Fpath.t;
        sources : Pattern.t list;  (** Patterns which include source files to load. *)
        pat_config : pat_config list;
            (** Path-specific overrides. These are applied in order, so later ones will override
                earlier ones. *)
        store : Schema.store
      }
  | Default

(** The schema for linters. Used to generate *)
let linter_schema =
  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    Schema.empty Linters.all

let global_schema =
  let add k s = Schema.union (Schema.singleton k) s in
  Schema.empty |> add IlluaminateSemantics.Doc.Extract.Config.key |> add doc_options

let root_pat = Pattern.parse "/"

(** The parser for Illuaminate's config. *)
let parser =
  let open Parser in
  let pattern = atom ~ty:"string" (fun x -> Some (Pattern.parse x)) in
  let tag_parser name =
    match Error.Tag.find name with
    | None when name = "all" -> Ok All
    | None -> Error (Printf.sprintf "Unknown tag %S" name)
    | Some t -> Ok (Tag t)
  in
  let tag_set_mod name =
    if String.length name = 0 then Error "Expected a tag, but this is an empty string."
    else
      match name.[0] with
      | '+' -> CCString.drop 1 name |> tag_parser |> Result.map (fun x -> Include x)
      | '-' -> CCString.drop 1 name |> tag_parser |> Result.map (fun x -> Exclude x)
      | _ -> tag_parser name |> Result.map (fun x -> Include x)
  in
  let pat_config_fields =
    let+ linter_options = Schema.to_parser linter_schema
    and+ tags = some (atom_res ~ty:"string" tag_set_mod) |> field_opt ~name:"linters" in
    (linter_options, Option.value ~default:[] tags)
  in
  let pat_config =
    let+ pattern = list_or_one pattern and+ linter_options, tags = fields pat_config_fields in
    { pattern; linter_options; tags }
  in
  let main =
    let+ sources = field_opt ~name:"sources" (some pattern)
    and+ pat_config = field_repeated ~name:"at" pat_config
    and+ store = Schema.to_parser global_schema in
    fun root ->
      Config { root; sources = Option.value ~default:[ root_pat ] sources; pat_config; store }
  in

  fields main

let parse_error = Error.(Tag.make Critical "config:parse")

let of_file err (file : Span.filename) =
  let parsed =
    CCIO.with_in file.path (fun channel ->
        let lexbuf = Lexing.from_channel channel in
        IlluaminateConfig.Parser.parse_buf file lexbuf parser)
  in
  match parsed with
  | Ok x -> Fpath.v file.path |> Fpath.parent |> x |> Option.some
  | Error (pos, msg) ->
      Error.report err parse_error pos msg;
      None

let default = Default

let generate out =
  let open Format in
  let comment txt = pp_print_string out ";; "; pp_print_string out txt; pp_force_newline out ()
  and blank = pp_force_newline out in
  (* Start off with a mode comment, to force your editor (what, you mean you don't use Emacs?) to do
     the right thing. *)
  pp_print_string out "; -*- mode: Lisp;-*-";
  pp_force_newline out ();

  (* (sources /) *)
  blank ();
  comment "Folders, files, or patterns to include.";
  pp_print_string out "(sources /)";
  blank ();
  blank ();

  (* (at x (tags ...) ...) *)
  comment "Overrides for a specific pattern, file or directory. These are matched";
  comment "in order, so later `at` blocks overwrite earlier ones.";
  pp_open_box out 1;
  pp_print_string out "(at /";
  pp_print_space out ();

  comment "Modifications to make to the linter set. For instance, `+all -var:unused`";
  comment "will enable all warnings but var:unused.";
  pp_print_string out "(linters all)";
  blank ();
  blank ();

  Schema.write_default out linter_schema;

  pp_print_string out ")";
  pp_close_box out ();

  blank ();
  Schema.write_default out global_schema

let all_tags =
  List.fold_left
    (fun tags (Linter.Linter linter) -> List.fold_left (Fun.flip TagSet.add) tags linter.tags)
    TagSet.empty Linters.all

let apply_mod set = function
  | Include All -> all_tags
  | Include (Tag t) -> TagSet.add t set
  | Exclude All -> TagSet.empty
  | Exclude (Tag t) -> TagSet.remove t set

let is_source config path =
  match config with
  | Default -> true
  | Config { root; sources; _ } ->
      Fpath.is_rooted ~root path
      && List.exists
           (Fpath.relativize ~root path |> Option.get |> Fpath.to_string |> Pattern.matches)
           sources

(** Get all linters and their configuration options for a particular file. *)
let get_linters config path =
  match config with
  | Default -> ((fun _ -> true), Schema.default linter_schema)
  | Config { root; pat_config; _ } ->
      if not (Fpath.is_rooted ~root path) then
        failwith "Path should be a child of the config's root.";
      let path = Fpath.relativize ~root path |> Option.get |> Fpath.to_string in
      let enabled, store =
        List.fold_left
          (fun (linters, store) { pattern; tags; linter_options } ->
            if List.exists (Pattern.matches path) pattern then
              let linters = List.fold_left apply_mod linters tags in
              (linters, Schema.merge store linter_options)
            else (linters, store))
          (all_tags, Schema.default linter_schema)
          pat_config
      in
      (Fun.flip TagSet.mem enabled, store)

let get_doc_options = function
  | Default ->
      (* TODO: Better handling of the root when there is no config present. *)
      Term.default doc_options_term Fpath.(v (Sys.getcwd ()) / "out")
  | Config { root; store; _ } -> (Schema.get doc_options store) root

let get_store = function
  | Default -> Schema.default global_schema
  | Config { store; _ } -> store
