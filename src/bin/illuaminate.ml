open IlluaminateCore
open IlluaminateLint

module LoadedFile = struct
  type t =
    { file : Span.filename;
      config : Config.t;
      parsed : Syntax.program option
    }

  (** Parse a file and report its errors. *)
  let parse errs ({ Span.path; _ } as file) =
    CCIO.with_in path (fun channel ->
        let lexbuf = Lexing.from_channel channel in
        match IlluaminateParser.parse file lexbuf with
        | Error err ->
            IlluaminateParser.Error.report errs err.span err.value;
            None
        | Ok tree -> Some tree)

  let gather errs paths : t list =
    let cwd = Unix.getcwd () in
    let mk_name path =
      let name =
        IlluaminatePattern.Paths.make_relative ~path ~dir:cwd |> Option.value ~default:path
      in
      { Span.path; name }
    in
    let paths =
      match paths with
      | [] -> [ cwd ]
      | xs -> List.map (IlluaminatePattern.Paths.to_absolute ~cwd) xs
    in
    (* First build a cache of config files *)
    let config_cache = Hashtbl.create 32 in
    let rec get_config dir =
      match Hashtbl.find_opt config_cache dir with
      | Some c -> c
      | None ->
          let config_path = Filename.concat dir "illuaminate.sexp" in
          let parent = Filename.dirname dir in
          let c =
            if Sys.file_exists config_path && not (Sys.is_directory config_path) then
              Config.of_file errs (mk_name config_path)
            else if parent = dir then Some Config.default
            else get_config parent
          in
          Hashtbl.add config_cache dir c; c
    in
    let get_config_for path =
      if Sys.is_directory path then get_config path else get_config (Filename.dirname path)
    in
    let files = ref [] in
    paths
    |> List.filter_map (fun path -> get_config_for path |> Option.map (fun c -> (path, c)))
    |> List.iter (fun (path, config) ->
           let rec add is_root path =
             if Sys.is_directory path then (
               let handle = Unix.opendir path in
               try
                 while true do
                   let child = Unix.readdir handle in
                   if child <> Filename.current_dir_name && child <> Filename.parent_dir_name then
                     add false (Filename.concat path child)
                 done
               with End_of_file -> (); Unix.closedir handle )
             else if is_root || Filename.extension path = ".lua" then
               let file = mk_name path in
               if Config.is_source config file.path then
                 files := { file; config; parsed = parse errs file } :: !files
           in

           add true path);
    !files
end

let uses_ansi channel =
  let dumb =
    try
      match Sys.getenv "TERM" with
      | "dumb" | "" -> true
      | _ -> false
    with Not_found -> true
  in
  let isatty = try Unix.(isatty (descr_of_out_channel channel)) with Unix.Unix_error _ -> false in
  (not dumb) && isatty

let lint paths =
  let errs = Error.make () in
  let modules = LoadedFile.gather errs paths in
  let data = Data.create () in
  modules
  |> List.iter (function
       | { LoadedFile.parsed = None; _ } -> ()
       | { file; config; parsed = Some parsed } ->
           let linters, muted, store = Config.get_linters config file.path in
           let errs = Error.mute muted errs in
           linters
           |> List.iter (fun l ->
                  Driver.lint ~store ~data l parsed |> List.iter (Driver.report_note errs)));
  Error.display_of_channel errs

let fix paths =
  let errs = Error.make () in
  let modules = LoadedFile.gather errs paths in
  let data = Data.create () in
  let modules' =
    modules
    |> List.map (function
         | { LoadedFile.parsed = None; _ } as f -> f
         | { file; config; parsed = Some parsed } as f ->
             let linters, _, store = Config.get_linters config file.path in
             let program, _ = Driver.lint_and_fix_all ~store ~data linters parsed in
             { f with parsed = Some program })
  in
  let rewrite old_module new_module =
    match (old_module, new_module) with
    | { LoadedFile.file; parsed = Some oldm; _ }, { LoadedFile.parsed = Some newm; _ }
      when oldm != newm -> (
      try
        let ch = open_out file.path in
        let fmt = Format.formatter_of_out_channel ch in
        Emit.program fmt newm; Format.pp_print_flush fmt (); close_out ch
      with e -> Printf.eprintf "Error fixing %s (%s).\n" file.path (Printexc.to_string e) )
    | _ -> ()
  in
  List.iter2 rewrite modules modules';
  Error.display_of_channel errs

let init_config config =
  if Sys.file_exists config then (
    Printf.eprintf "File already exists";
    exit 1 );
  let out = open_out config in
  let formatter = Format.formatter_of_out_channel out in
  Config.generate formatter;
  Format.pp_print_flush formatter ();
  close_out out

let () =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let files_arg =
    value & pos_all file [] & info ~docv:"FILE" ~doc:"Files and directories to check." []
  in
  let lint_cmd =
    let doc = "Checks all files, and reports errors." in
    (Term.(const lint $ files_arg), Term.info "lint" ~doc)
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
    (Term.(const init_config $ config_arg), Term.info "init-config" ~doc)
  in

  let default_cmd =
    let doc = "Provides basic source code analysis of Lua projects." in
    ( Term.(ret (const (`Help (`Pager, None)))),
      Term.info "illuaminate" ~doc ~exits:Term.default_exits )
  in

  if uses_ansi stdout then Error.Style.setup_ansi Format.std_formatter;
  if uses_ansi stderr then Error.Style.setup_ansi Format.err_formatter;
  Term.exit @@ Term.eval_choice default_cmd [ lint_cmd; fix_cmd; init_config_cmd ]
