open IlluaminateCore
open IlluaminateLint
open IlluaminateSemantics

let uses_ansi channel =
  let dumb =
    try
      match Sys.getenv "TERM" with
      | "dumb" | "" -> true
      | _ -> false
    with Not_found -> true
  in
  let isatty = try Unix.isatty channel with Unix.Unix_error _ -> false in
  (not dumb) && isatty

let lint paths github =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, file_store = List.map Fpath.v paths |> Loader.load_from_many ~loader in
  let data = Data.of_files file_store in
  modules
  |> List.iter (function
       | { Loader.parsed = None; _ } -> ()
       | { path; config; parsed = Some parsed; _ } ->
           let tags, store = Config.get_linters config path in
           Linters.all
           |> List.iter (fun l ->
                  Driver.lint ~store ~data ~tags l parsed |> List.iter (Driver.report_note errs)));
  Error.display_of_files errs;
  ( if github then
    match IlluaminateGithub.publish_errors errs with
    | Ok () -> ()
    | Error msg -> Printf.eprintf "%s\n" msg; exit 2 );
  if Error.has_problems errs then exit 1

let fix paths =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, files = List.map Fpath.v paths |> Loader.load_from_many ~loader in
  let modules' =
    modules
    |> List.map (function
         | { Loader.parsed = None; _ } as f -> f
         | { path; config; parsed = Some parsed; _ } as f ->
             (* TODO: Have a separate linter list for fixers - so we can have things which are
                linted but not fixed? *)
             let tags, store = Config.get_linters config path in
             let program, _ = Driver.lint_and_fix_all ~store ~files ~tags Linters.all parsed in
             { f with parsed = Some program })
  in
  let rewrite old_module new_module =
    match (old_module, new_module) with
    | { Loader.file; parsed = Some oldm; _ }, { Loader.parsed = Some newm; _ } when oldm != newm
      -> (
      try
        let ch = open_out file.path in
        let fmt = Format.formatter_of_out_channel ch in
        Emit.program fmt newm; Format.pp_print_flush fmt (); close_out ch
      with e -> Printf.eprintf "Error fixing %s (%s).\n" file.path (Printexc.to_string e) )
    | _ -> ()
  in
  List.iter2 rewrite modules modules';
  Error.display_of_files errs;
  if Error.has_problems errs then exit 1

let init_config config force =
  if (not force) && Sys.file_exists config then (
    Printf.eprintf "File already exists";
    exit 1 );
  let out = open_out config in
  let formatter = Format.formatter_of_out_channel out in
  Config.generate formatter;
  Format.pp_print_flush formatter ();
  close_out out

let run () =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let files_arg =
    value & pos_all file [] & info ~docv:"FILE" ~doc:"Files and directories to check." []
  in
  let lint_cmd =
    let doc = "Checks all files, and reports errors." in
    let github =
      value & flag
      & info
          ~doc:
            "Annotate your code with any warnings using GitHub's Checks API. This is designed to \
             be run under GitHub actions, and so will search for appropriate information from the \
             $(b,GITHUB_TOKEN), $(b,GITHUB_SHA) and $(b,GITHUB_REPOSITORY) environment variables."
          [ "github" ]
    in

    (Term.(const lint $ files_arg $ github), Term.info "lint" ~doc)
  in
  let fix_cmd =
    let doc = "Checks all files and fixes problems in them." in
    (Term.(const fix $ files_arg), Term.info "fix" ~doc)
  in
  let init_config_cmd =
    let doc = "Generates a new config file." in
    let config_arg =
      required & pos 0 (some string) None & info ~doc:"The config to generate." ~docv:"CONFIG" []
    in
    let force =
      value & flag
      & info ~doc:"Always write the config file, even if it already exists." [ "f"; "force" ]
    in
    (Term.(const init_config $ config_arg $ force), Term.info "init-config" ~doc)
  in

  let default_cmd =
    let doc = "Provides basic source code analysis of Lua projects." in
    ( Term.(ret (const (`Help (`Pager, None)))),
      Term.info "illuaminate" ~doc ~exits:Term.default_exits )
  in

  if uses_ansi Unix.stdout then Error.Style.setup_ansi Format.std_formatter;
  if uses_ansi Unix.stderr then Error.Style.setup_ansi Format.err_formatter;
  Term.exit @@ Term.eval_choice default_cmd [ lint_cmd; fix_cmd; init_config_cmd ]
