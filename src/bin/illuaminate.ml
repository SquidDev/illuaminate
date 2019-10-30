open IlluaminateCore
open IlluaminateConfig
open IlluaminateLint

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

(** The schema for Illuaminate's config. *)
let config_schema =
  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    Schema.empty Linters.all
  |> Schema.to_term

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

(** Determine whether a linter is enabled. *)
let linter_enabled muted only (Linter.Linter l) =
  List.exists (fun x -> not (List.mem x muted)) l.tags
  &&
  match only with
  | [] -> true
  | _ -> List.exists (fun x -> List.mem x l.tags) only

let lint (muted, only) paths (_, store) =
  let errs = Error.make ~muted () in
  let modules = gather_files paths |> List.map (parse errs) |> CCList.all_some in
  ( match modules with
  | None -> ()
  | Some modules ->
      let data = Data.create () in
      let linters = Linters.all |> List.filter (linter_enabled muted only) in
      modules
      |> List.iter (fun parsed ->
             List.iter
               (fun l -> Driver.lint ~store ~data l parsed |> List.iter (Driver.report_note errs))
               linters) );
  Error.display_of_channel (fun (x : Span.filename) -> Some (open_in x.path)) errs

let fix (muted, only) paths (_, store) =
  let errs = Error.make ~muted () in
  let modules = gather_files paths |> List.map (parse errs) |> CCList.all_some in
  ( match modules with
  | None -> ()
  | Some modules ->
      let data = Data.create () in
      let linters = Linters.all |> List.filter (linter_enabled muted only) in
      let modules' =
        modules
        |> List.map (fun parsed ->
               let program, notes = Driver.lint_and_fix_all ~store ~data linters parsed in
               List.iter (Driver.report_note errs) notes;
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

let () =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let tag_parser =
    let parse s =
      match Error.find_tag s with
      | Some tag -> Ok tag
      | None -> Error (`Msg (Printf.sprintf "Unknown warning %S" s))
    in
    conv ~docv:"WARNING" (parse, Error.pp_tag)
  in
  let config_parser =
    let parse s =
      if not (Sys.file_exists s) then Error (`Msg (Printf.sprintf "Not a file %S" s))
      else if Sys.is_directory s then Error (`Msg (Printf.sprintf "%S is a directory" s))
      else
        let channel = open_in s in
        let lexbuf = Lexing.from_channel ~with_positions:true channel in
        lexbuf.lex_curr_p <- { Lexing.pos_fname = s; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 };
        Storage.read lexbuf config_schema
        |> Result.map (fun c -> (s, c))
        |> Result.map_error (fun x -> `Msg x)
    in
    conv (parse, fun f (x, _) -> Format.pp_print_string f x)
  in
  let tags_arg =
    let muted =
      value & opt_all tag_parser []
      & info ~docv:"WARNING"
          ~doc:
            "Mute a specific warning. Any linters which have all warnings muted will be disabled."
          [ "W" ]
    in
    let only =
      value & opt_all tag_parser []
      & info ~docv:"WARNING"
          ~doc:
            "Enable a specific warning. When used, only linters whose warnings are explicitly \
             allowed will be run."
          [ "w" ]
    in
    Term.(
      ret
        ( const (fun muted only ->
              match (muted, only) with
              | _ :: _, _ :: _ -> `Error (true, "Cannot use -W and -w at the same time")
              | _, _ -> `Ok (muted, only))
        $ muted $ only ))
  in
  let files_arg =
    non_empty & pos_all file [] & info ~docv:"FILE" ~doc:"Files and directories to check." []
  in
  let config_arg =
    value
    & opt config_parser ("<default>", IlluaminateConfig.Term.default config_schema)
    & info ~doc:"The config file to load." ~docv:"CONFIG" [ "config" ]
  in
  let lint_cmd =
    let doc = "Checks all files, and reports errors." in
    (Term.(const lint $ tags_arg $ files_arg $ config_arg), Term.info "lint" ~doc)
  in
  let fix_cmd =
    let doc = "Checks all files and fixes problems in them." in
    (Term.(const fix $ tags_arg $ files_arg $ config_arg), Term.info "fix" ~doc)
  in
  let default_cmd =
    let doc = "Provides basic source code analysis of Lua projects." in
    ( Term.(ret (const (`Help (`Pager, None)))),
      Term.info "illuaminate" ~doc ~exits:Term.default_exits )
  in
  Term.exit @@ Term.eval_choice default_cmd [ lint_cmd; fix_cmd ]
