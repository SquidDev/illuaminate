open IlluaminateCore
open IlluaminateLint
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Config = IlluaminateConfigFormat
open CCFun

let src = Logs.Src.create ~doc:"The core CLI for illuaminate" __MODULE__

module Log = (val Logs.src_log src)

let write_file path f =
  CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] (Fpath.to_string path) @@ fun ch ->
  let formatter = Format.formatter_of_out_channel ch in
  f formatter;
  Format.pp_print_flush formatter ()

let reporter () =
  let open Error.Style in
  let app_name =
    match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | _ -> Filename.basename Sys.argv.(0)
  in
  let pp_with out style h =
    Format.fprintf out "%s: [" app_name;
    printf style out "%s" h;
    Format.fprintf out "] "
  in
  let pp_header out (level, header) =
    let style, default =
      match level with
      | Logs.App -> (DullColor Cyan, "APP")
      | Logs.Error -> (DullColor Red, "ERROR")
      | Logs.Warning -> (DullColor Yellow, "WARN")
      | Logs.Info -> (DullColor Blue, "INFO")
      | Logs.Debug -> (DullColor Green, "DEBUG")
    in
    pp_with out style (Option.value ~default header)
  in
  Logs.format_reporter ~pp_header ()

let lint paths github =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, builder = Loader.load_from_many ~loader paths in
  let data = IlluaminateData.Builder.(empty |> builder |> build) in
  modules
  |> List.iter (function
       | { Loader.parsed = None; _ } -> ()
       | { path; config; parsed = Some parsed; _ } ->
           let tags, store = Config.get_linters config ~path () in
           Linters.all
           |> List.iter @@ fun l ->
              Driver.lint ~store ~data ~tags l parsed
              |> Driver.Notes.to_seq
              |> Seq.iter (Driver.Note.report_any errs));
  Error.display_of_files errs;
  ( if github then
    match Github.publish_errors errs with
    | Ok () -> ()
    | Error msg -> Printf.eprintf "%s\n" msg; exit 2 );
  if Error.has_problems errs then exit 1

let fix paths =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, builder = Loader.load_from_many ~loader paths in
  let data = IlluaminateData.Builder.(empty |> builder |> build) in
  let modules' =
    modules
    |> List.map (function
         | { Loader.parsed = None; _ } as f -> f
         | { path; config; parsed = Some parsed; _ } as f ->
             (* TODO: Have a separate linter list for fixers - so we can have things which are
                linted but not fixed? *)
             let tags, store = Config.get_linters config ~path () in
             let program, _ = Driver.lint_and_fix_all ~store ~data ~tags Linters.all parsed in
             { f with parsed = Some program })
  in
  let rewrite old_module new_module =
    match (old_module, new_module) with
    | ( { Loader.file = { name; path = Some path; _ }; parsed = Some oldm; _ },
        { Loader.parsed = Some newm; _ } )
      when oldm != newm -> (
      try write_file path (fun out -> Emit.program out newm)
      with e -> Log.err (fun f -> f "Error fixing %s (%s).\n" name (Printexc.to_string e)) )
    | _ -> ()
  in
  List.iter2 rewrite modules modules';
  Error.display_of_files errs;
  if Error.has_problems errs then exit 1

let mkdirs =
  let rec go xs path =
    let path_s = Fpath.to_string path in
    if Sys.file_exists path_s then List.iter (fun p -> Unix.mkdir p 0o755) xs
    else if Fpath.is_root path then failwith "Root directory doesn't exist!"
    else go (path_s :: xs) Fpath.(parent path |> normalize)
  in
  go []

let doc_gen path =
  let open IlluaminateSemantics in
  let module E = IlluaminateDocEmit in
  let open struct
    let emit_doc node out =
      let fmt = Format.formatter_of_out_channel out in
      Html.Default.emit_doc fmt node; Format.pp_print_flush fmt ()

    let resolve_logo ~destination logo =
      if Fpath.is_rooted ~root:destination logo then
        Fpath.relativize ~root:destination logo |> Option.get |> Fpath.to_string
      else
        let our_logo = Fpath.(base logo |> to_string) in
        CCIO.(
          with_in (Fpath.to_string logo) @@ fun i ->
          with_out Fpath.(destination / our_logo |> to_string) @@ fun o -> copy_into i o);
        our_logo

    let parse_index path =
      E.Html_loader.load_file ~resolve:Fun.id path
      |> Result.fold ~ok:Fun.id ~error:(fun e -> Printf.eprintf "%s\n%!" e; exit 1)

    let gen_appended ~destination ~name ~contents extra =
      ( CCIO.with_out Fpath.(destination / name |> to_string) @@ fun out ->
        output_string out contents;
        Option.iter
          (fun extra -> CCIO.with_in (Fpath.to_string extra) @@ fun i -> CCIO.copy_into i out)
          extra );
      name
  end in
  let errs = Error.make () in
  let loader = Loader.create errs in
  ( CCOpt.get_lazy (fun () -> Sys.getcwd () |> Fpath.v) path
  |> Loader.load_from ~loader
  |> Option.iter @@ fun (config, _, builder) ->
     let data = IlluaminateData.Builder.(empty |> builder |> build) in
     let modules =
       IlluaminateData.get data Doc.Extract.get_modules ()
       |> StringMap.to_seq
       |> Seq.map (fun (_, x) -> x)
       |> List.of_seq
       |> List.sort (fun a b ->
              Doc.Syntax.(String.compare a.descriptor.mod_name b.descriptor.mod_name))
     in
     let { Config.site_title;
           site_image;
           embed_js;
           embed_css;
           index;
           destination;
           source_link;
           json_index
         } =
       Config.get_doc_options config
     in
     mkdirs Fpath.(destination / "module");

     let site_image = Option.map (resolve_logo ~destination) site_image in
     let site_css =
       gen_appended ~destination ~name:"main.css" ~contents:E.Html_embedded_styles.contents
         embed_css
     in
     let site_js =
       gen_appended ~destination ~name:"main.js" ~contents:E.Html_embedded_scripts.contents embed_js
     in
     let index = Option.fold ~none:Html.Default.nil ~some:parse_index index in
     let module_dir = Fpath.v "module" in
     let custom =
       let config =
         Config.get_store config |> IlluaminateConfig.Schema.get Doc.Extract.Config.key
       in
       config.module_kinds
     in

     let options resolve : E.Html_main.Options.t =
       E.Html_main.Options.make ?site_title ?site_image ~site_js ~site_css ~resolve ~source_link
         ~custom ()
     in
     let module_options =
       options @@ fun x ->
       let open Fpath in
       v x |> relativize ~root:module_dir |> Option.fold ~none:("../" ^ x) ~some:to_string
     in
     modules
     |> List.iter (fun (modu : Doc.Syntax.module_info Doc.Syntax.documented) ->
            let path = Fpath.(destination / "module" / (modu.descriptor.mod_name ^ ".html")) in
            E.Html_main.emit_module ~options:module_options ~modules modu
            |> emit_doc
            |> CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] (Fpath.to_string path));

     let path = Fpath.(destination / "index.html") in
     E.Html_main.emit_modules ~options:(options Fun.id) ~modules index
     |> emit_doc
     |> CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] (Fpath.to_string path);

     if json_index then
       let path = Fpath.(destination / "index.json") in
       E.Summary.(everything ~source_link modules |> to_json)
       |> Yojson.Safe.to_file (Fpath.to_string path) );
  Error.display_of_files errs;
  if Error.has_problems errs then exit 1

let dump_globals ~defined paths =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, builder = Loader.load_from_many ~loader paths in
  let data = IlluaminateData.Builder.(empty |> builder |> build) in
  let gather =
    Seq.fold_left (fun (unbound, defined) var ->
        let open IlluaminateSemantics.Resolve in
        match var.definitions with
        | [] -> (StringSet.add var.name unbound, defined)
        | _ :: _ -> (unbound, StringSet.add var.name defined))
  in
  let dump name xs =
    if not (StringSet.is_empty xs) then (
      Printf.printf "%s:\n" name;
      StringSet.iter (Printf.printf " - %s\n") xs )
  in
  let unbound_names, defined_names =
    List.fold_left
      (fun (unbound, defined) program ->
        match program with
        | { Loader.parsed = None; _ } -> (unbound, defined)
        | { parsed = Some parsed; _ } ->
            IlluaminateData.get data IlluaminateSemantics.Resolve.key parsed
            |> IlluaminateSemantics.Resolve.globals
            |> gather (unbound, defined))
      (StringSet.empty, StringSet.empty)
      modules
  and module_names =
    IlluaminateData.get data IlluaminateSemantics.Module_resolve.global_modules ()
  in
  dump "Undefined globals" (StringSet.diff unbound_names module_names);
  if defined then dump "Defined globals" defined_names

let init_config config force =
  if (not force) && Sys.file_exists config then (
    Printf.eprintf "File already exists";
    exit 1 );
  write_file (Fpath.v config) Config.generate

let minify file =
  let module D = IlluaminateData in
  let module M = IlluaminateMinify in
  let errs = Error.make () in
  let program =
    match file with
    | Some file ->
        let path = Fpath.(v (Sys.getcwd ()) // file) in
        let path_s = Fpath.to_string path in
        let filename = Span.Filename.mk ~name:(Fpath.to_string file) ~path path_s in
        CCIO.with_in path_s @@ (IlluaminateParser.program filename % Lexing.from_channel)
    | None ->
        let filename = Span.Filename.mk "=stdin" in
        Lexing.from_channel stdin |> IlluaminateParser.program filename
  in
  match program with
  | Error err -> IlluaminateParser.Error.report errs err.span err.value
  | Ok program ->
      let out = Format.formatter_of_out_channel stdout in
      D.compute (fun ctx -> M.minify ctx program) D.Builder.(build empty)
      |> M.Emit.(with_wrapping out "%a" program)

module Args = struct
  include Cmdliner
  include Cmdliner.Arg

  let ( let+ ) x f = Term.(const f $ x)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

  type common =
    { verbose : bool list;
      colour : bool option
    }

  let common =
    let+ verbose =
      value & flag_all
      & info ~docs:Cmdliner.Manpage.s_common_options
          ~doc:
            "Show log messages. One $(b,-v) shows errors, warnings and information messages. \
             Additional usages will also show debug messages."
          [ "verbose"; "v" ]
    and+ colour =
      value
      & opt ~vopt:(Some true)
          (enum [ ("never", Some false); ("always", Some true); ("auto", None) ])
          None
      & info ~docv:"when" ~docs:Cmdliner.Manpage.s_common_options
          ~doc:"Show coloured messages. When auto, we attempt to determine if the output is a TTY."
          [ "color"; "colour" ]
    in
    { verbose; colour }

  let common_docs : Cmdliner.Manpage.block list =
    [ `S Cmdliner.Manpage.s_common_options;
      `P
        "These options are common to all commands. Note that they must be passed after the \
         sub-command, not before."
    ]

  let uses_ansi force channel =
    match force with
    | Some x -> x
    | None ->
        let dumb =
          match Sys.getenv_opt "TERM" with
          | Some ("dumb" | "") | None -> true
          | Some _ -> false
        in
        let isatty = try Unix.isatty channel with Unix.Unix_error _ -> false in
        (not dumb) && isatty

  let setup_common { verbose; colour } =
    if uses_ansi colour Unix.stdout then Error.Style.setup_ansi Format.std_formatter;
    if uses_ansi colour Unix.stderr then Error.Style.setup_ansi Format.err_formatter;

    let l =
      match verbose with
      | [] -> Some Logs.Warning
      | [ _ ] -> Some Logs.Info
      | _ :: _ :: _ -> Some Logs.Debug
    in
    Logs.(set_level ~all:true l);
    reporter () |> Logs.set_reporter

  let file =
    let parse s =
      let open CCResult.Infix in
      Fpath.of_string s >>= fun path ->
      (* There's a strange bug on Windows where "foo\\" will not exist, but "foo" will. In order to
         avoid this, we remove the trailing path segment. *)
      let path = Fpath.rem_empty_seg path in
      let s = Fpath.to_string path in
      match Sys.file_exists s with
      | true -> Ok path
      | false -> Error (`Msg ("no file or directory " ^ s))
    in
    conv ~docv:"FILE" (parse, Fpath.pp)

  let files_arg =
    value & pos_all file [ Fpath.v "." ] & info ~doc:"Files and directories to check." []

  let doc_file_arg =
    value
    & pos 0 (some ~none:"current directory" file) None
    & info ~doc:"File/directory to generate docs for." []

  let minify_file_arg =
    value & pos 0 (some ~none:"stdin" file) None & info ~doc:"File to minify." []
end

let run () =
  let open Args in
  let lint_cmd =
    let doc = "Checks all files, and reports errors." in
    let term =
      let+ common = common
      and+ github =
        value & flag
        & info
            ~doc:
              "Annotate your code with any warnings using GitHub's Checks API. This is designed to \
               be run under GitHub actions, and so will search for appropriate information from \
               the $(b,GITHUB_TOKEN), $(b,GITHUB_SHA) and $(b,GITHUB_REPOSITORY) environment \
               variables."
            [ "github" ]
      and+ files = files_arg in
      setup_common common; lint files github
    in
    (term, Term.info "lint" ~doc ~man:[ `Blocks common_docs ])
  in
  let fix_cmd =
    let doc = "Checks all files and fixes problems in them." in
    let term =
      let+ common = common and+ files = files_arg in
      setup_common common; fix files
    in
    (term, Term.info "fix" ~doc ~man:[ `Blocks common_docs ])
  in
  let dump_global_cmd =
    let doc = "Dumps all usages of \"undefined\" globals, and global definitions." in
    let term =
      let+ common = common
      and+ files = files_arg
      and+ defined =
        value & flag & info ~doc:"Display definitions of global variables." [ "d"; "defined" ]
      in
      setup_common common; dump_globals ~defined files
    in
    (term, Term.info "dump-global" ~doc ~man:[ `Blocks common_docs ])
  in
  let doc_gen_cmd =
    let doc = "Generates HTML documentation" in
    let term =
      let+ common = common and+ file = doc_file_arg in
      setup_common common; doc_gen file
    in
    (term, Term.info "doc-gen" ~doc ~man:[ `Blocks common_docs ])
  in

  let init_config_cmd =
    let doc = "Generates a new config file." in
    let term =
      let+ common = common
      and+ config =
        required & pos 0 (some string) None & info ~doc:"The config to generate." ~docv:"CONFIG" []
      and+ force =
        value & flag
        & info ~doc:"Always write the config file, even if it already exists." [ "f"; "force" ]
      in
      setup_common common; init_config config force
    in
    (term, Term.info "init-config" ~doc ~man:[ `Blocks common_docs ])
  in
  let minify_cmd =
    let doc = "Minify a Lua file" in
    let term =
      let+ common = common and+ file = minify_file_arg in
      setup_common common; minify file
    in
    (term, Term.info "minify" ~doc ~man:[ `Blocks common_docs ])
  in

  let default_cmd =
    let doc = "Provides basic source code analysis of Lua projects." in
    let term =
      let+ common = common in
      setup_common common;
      `Help (`Pager, None)
    in
    (Term.(ret term), Term.info "illuaminate" ~doc ~exits:Term.default_exits)
  in

  Term.exit
  @@ Term.eval_choice default_cmd
       [ lint_cmd; fix_cmd; doc_gen_cmd; dump_global_cmd; init_config_cmd; minify_cmd ]
