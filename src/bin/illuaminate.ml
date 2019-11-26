open IlluaminateCore
open IlluaminateConfig
open IlluaminateLint
module TagSet = Set.Make (Error.Tag)

module Config = struct
  type dir_config =
    { dir : Str.regexp;
      enabled : Error.Tag.t list;
      disabled : Error.Tag.t list;
      linter_options : Schema.store
    }

  type t = dir_config list

  let linter_schema =
    List.fold_left
      (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
      Schema.empty Linters.all
    |> Schema.to_term

  (** The parser for Illuaminate's config. *)
  let parser =
    let open Term in
    let dir_schema =
      let+ linter_options = linter_schema
      and+ enabled =
        field ~name:"+linters" ~comment:"Enabled linters. This will extend the parent set."
          ~default:[]
          Converter.(list string)
      and+ disabled =
        field ~name:"-linters" ~comment:"Disabled linters. This will extend from the parent."
          ~default:[]
          Converter.(list string)
      in
      fun dir ->
        { dir;
          linter_options;
          enabled = List.filter_map Error.Tag.find enabled;
          disabled = List.filter_map Error.Tag.find disabled
        }
    in
    let open Parser in
    let at_parser =
      let+ dir = string and+ options = Term.to_parser dir_schema |> Parser.fields in
      options (Glob.parse dir)
    in
    field_repeated ~name:"at" at_parser |> fields

  let default =
    [ { dir = Str.regexp ".*";
        enabled = [];
        disabled = [];
        linter_options = IlluaminateConfig.Term.default linter_schema
      }
    ]

  let generater formatter () = Term.write_default formatter linter_schema

  let all_tags =
    List.fold_left
      (fun tags (Linter.Linter linter) -> List.fold_left (Fun.flip TagSet.add) tags linter.tags)
      TagSet.empty Linters.all

  (** Get all linters and their configuration options for a particular file. *)
  let get_linters (store : t) path =
    let enabled, store =
      List.fold_left
        (fun (linters, store) { dir; enabled; disabled; linter_options } ->
          if Str.string_match dir path 0 then
            let linters = List.fold_left (Fun.flip TagSet.remove) linters disabled in
            let linters = List.fold_left (Fun.flip TagSet.add) linters enabled in
            (linters, linter_options)
          else (linters, store))
        (all_tags, Term.default linter_schema)
        store
    in
    ( List.filter
        (fun (Linter.Linter l) -> List.exists (Fun.flip TagSet.mem enabled) l.tags)
        Linters.all,
      TagSet.elements (TagSet.diff all_tags enabled),
      store )
end

let gather_files paths =
  let cwd = Unix.getcwd () in
  let absolute x = if Filename.is_relative x then Filename.concat cwd x else x in
  let module StringMap = Map.Make (String) in
  let files = ref StringMap.empty in
  let rec add top abs rel =
    if Sys.is_directory abs then
      let handle = Unix.opendir abs in
      try
        while true do
          let child = Unix.readdir handle in
          if child <> "." && child <> ".." then
            add false (Filename.concat abs child) (if top then child else Filename.concat rel child)
        done
      with End_of_file -> ()
    else if top || CCString.suffix ~suf:".lua" abs then files := StringMap.add abs rel !files
  in
  List.iter (fun x -> add true (absolute x) (Filename.basename x)) paths;
  StringMap.bindings !files

(** Parse a file and report its errors. *)
let parse errs (path, name) =
  let channel = open_in path in
  let lexbuf = Lexing.from_channel channel in
  let res =
    match IlluaminateParser.parse { Span.name; path } lexbuf with
    | Error err ->
        IlluaminateParser.Error.report errs err.span err.value;
        None
    | Ok tree -> Some tree
  in
  close_in channel; res

let lint paths (_, (store : Config.t)) =
  let errs = Error.make () in
  let modules = gather_files paths |> List.map (parse errs) |> CCList.all_some in
  ( match modules with
  | None -> ()
  | Some modules ->
      let data = Data.create () in
      modules
      |> List.iter (fun parsed ->
             let path = (Syntax.Spanned.program parsed).filename.name in
             let linters, muted, store = Config.get_linters store path in
             let errs = Error.mute muted errs in
             linters
             |> List.iter (fun l ->
                    Driver.lint ~store ~data l parsed |> List.iter (Driver.report_note errs))) );
  Error.display_of_channel (fun (x : Span.filename) -> Some (open_in x.path)) errs

let fix paths (_, (store : Config.t)) =
  let errs = Error.make () in
  let modules = gather_files paths |> List.map (parse errs) |> CCList.all_some in
  ( match modules with
  | None -> ()
  | Some modules ->
      let data = Data.create () in
      let modules' =
        modules
        |> List.map (fun parsed ->
               let path = (Syntax.Spanned.program parsed).filename.name in
               let linters, _, store = Config.get_linters store path in
               let program, _ = Driver.lint_and_fix_all ~store ~data linters parsed in
               program)
      in
      let rewrite old_module new_module =
        if old_module != new_module then
          let name = (Syntax.Spanned.program new_module).filename in
          try
            let ch = open_out name.path in
            let fmt = Format.formatter_of_out_channel ch in
            Emit.program fmt new_module; Format.pp_print_flush fmt (); close_out ch
          with e -> Printf.eprintf "Error fixing %s (%s).\n" name.path (Printexc.to_string e)
      in
      List.iter2 rewrite modules modules' );
  Error.display_of_channel (fun (x : Span.filename) -> Some (open_in x.path)) errs

let init_config config =
  if Sys.file_exists config then (
    Printf.eprintf "File already exists";
    exit 1 );
  let out = open_out config in
  let formatter = Format.formatter_of_out_channel out in
  Config.generater formatter ();
  Format.pp_print_flush formatter ();
  close_out out

let () =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let config_parser =
    let parse s =
      if not (Sys.file_exists s) then Error (`Msg (Printf.sprintf "Not a file %S" s))
      else if Sys.is_directory s then Error (`Msg (Printf.sprintf "%S is a directory" s))
      else
        let channel = open_in s in
        let lexbuf = Lexing.from_channel ~with_positions:true channel in
        lexbuf.lex_curr_p <- { Lexing.pos_fname = s; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 };
        IlluaminateConfig.Parser.parse_buf lexbuf Config.parser
        |> Result.map (fun c -> (s, c))
        |> Result.map_error (fun ({ IlluaminateConfig.Parser.row; col }, e) ->
               `Msg (Printf.sprintf "%s:%d:%d: %s" s row col e))
    in
    conv (parse, fun f (x, _) -> Format.pp_print_string f x)
  in
  let files_arg =
    non_empty & pos_all file [] & info ~docv:"FILE" ~doc:"Files and directories to check." []
  in
  let config_arg =
    value
    & opt config_parser ("<default>", Config.default)
    & info ~doc:"The config file to load." ~docv:"CONFIG" [ "config" ]
  in
  let lint_cmd =
    let doc = "Checks all files, and reports errors." in
    (Term.(const lint $ files_arg $ config_arg), Term.info "lint" ~doc)
  in
  let fix_cmd =
    let doc = "Checks all files and fixes problems in them." in
    (Term.(const fix $ files_arg $ config_arg), Term.info "fix" ~doc)
  in
  let init_config_cmd =
    let doc = "Generates a new config file." in
    let config_arg =
      required & pos 0 (some string) None & info ~doc:"The config to generate." ~docv:"CONFIG" []
    in
    (Term.(const init_config $ config_arg), Term.info "init-config" ~doc)
  in

  let default_cmd =
    let doc = "Provides basic source code analysis of Lua projects." in
    ( Term.(ret (const (`Help (`Pager, None)))),
      Term.info "illuaminate" ~doc ~exits:Term.default_exits )
  in
  Term.exit @@ Term.eval_choice default_cmd [ lint_cmd; fix_cmd; init_config_cmd ]
