open IlluaminateCore
open IlluaminateLint

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
  Config.generate formatter ();
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

  if uses_ansi stdout then Error.Style.setup_ansi Format.std_formatter;
  if uses_ansi stderr then Error.Style.setup_ansi Format.err_formatter;
  Term.exit @@ Term.eval_choice default_cmd [ lint_cmd; fix_cmd; init_config_cmd ]
