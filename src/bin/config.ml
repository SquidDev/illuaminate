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

let global_schema = Schema.empty

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

let get_store = function
  | Default -> Schema.default global_schema
  | Config { store; _ } -> store
