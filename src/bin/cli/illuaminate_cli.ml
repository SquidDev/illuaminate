open IlluaminateCore
open IlluaminateLint
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module NMap = Map.Make (IlluaminateSemantics.Namespace)
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
  let pp_level out style h src =
    Fmt.pf out "%s: [%a] " (Logs.Src.name src) Fmt.(styled style string) h
  in
  let pp_header out (l, h, s) =
    match l with
    | Logs.App -> (
      match h with
      | None -> ()
      | Some h -> pp_level out `Cyan h s)
    | Logs.Error -> pp_level out `Red (Option.value ~default:"ERROR" h) s
    | Logs.Warning -> pp_level out `Yellow (Option.value ~default:"WARNING" h) s
    | Logs.Info -> pp_level out `Blue (Option.value ~default:"INFO" h) s
    | Logs.Debug -> pp_level out `Green (Option.value ~default:"DEBUG" h) s
  in

  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    Format.kfprintf k Format.err_formatter ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, header, src)
  in
  { Logs.report }

let lint paths =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, builder = Loader.load_from_many ~loader paths in
  let data = IlluaminateData.Builder.(empty |> builder |> build) in
  modules
  |> List.iter (function
       | { Loader.body = None; _ } -> ()
       | { path; config; body = Some parsed; _ } ->
           let tags, store = Config.get_linters config ~path () in
           Linters.all
           |> List.iter @@ fun l ->
              Driver.lint ~store ~data ~tags l parsed
              |> Driver.Notes.to_seq
              |> Seq.iter (Driver.Note.report_any errs));
  Error.display_of_files errs;
  if Error.has_problems errs then exit 1

let fix paths =
  let errs = Error.make () in
  let loader = Loader.create errs in
  let modules, builder = Loader.load_from_many ~loader paths in
  let data = IlluaminateData.Builder.(empty |> builder |> build) in
  let modules' =
    modules
    |> List.map (function
         | { Loader.body = None; _ } as f -> f
         | { path; config; body = Some parsed; _ } as f ->
             (* TODO: Have a separate linter list for fixers - so we can have things which are
                linted but not fixed? *)
             let tags, store = Config.get_linters config ~path () in
             let fixed, _ = Driver.lint_and_fix_all ~store ~data ~tags Linters.all parsed in
             { f with body = Some fixed })
  in
  let rewrite old_module new_module =
    match (old_module, new_module) with
    | ( { Loader.file = { name; path = Some path; _ }; body = Some oldm; _ },
        { Loader.body = Some newm; _ } )
      when oldm != newm -> (
      try write_file path (fun out -> File.emit out newm)
      with e -> Log.err (fun f -> f "Error fixing %s (%s).\n" name (Printexc.to_string e)))
    | _ -> ()
  in
  List.iter2 rewrite modules modules';
  Error.display_of_files errs;
  if Error.has_problems errs then exit 1

let mkdirs =
  let rec go xs path =
    let path_s = Fpath.(rem_empty_seg path |> to_string) in
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

    let parse_index ~options path =
      E.Html.load_file ~options path
      |> Result.fold ~ok:Fun.id ~error:(fun e -> Printf.eprintf "%s\n%!" e; exit 1)

    let gen_appended ~destination ~name ~contents extra =
      let output = Fpath.(destination / name |> to_string) in
      ( CCIO.with_out output @@ fun out ->
        output_string out contents;
        Option.iter
          (fun extra -> CCIO.with_in (Fpath.to_string extra) @@ fun i -> CCIO.copy_into i out)
          extra );
      (* Append an 8 byte cachebuster. There's no reason to only make it 8 bytes, but it doesn't
         need to be a full hash either. *)
      let hash = Digest.file output |> Digest.to_hex |> CCString.take 8 in
      Printf.sprintf "%s?v=%s" name hash
  end in
  let errs = Error.make () in
  let loader = Loader.create errs in
  (CCOption.get_lazy (fun () -> Sys.getcwd () |> Fpath.v) path
  |> Loader.load_from ~loader
  |> Option.iter @@ fun (config, _, builder) ->
     let data = IlluaminateData.Builder.(empty |> builder |> build) in
     let pages = IlluaminateData.get data Doc.Extract.public_pages () in
     let { Config.DocOptions.site_properties =
             { site_title; site_image; site_url; embed_head; embed_js; embed_css; source_link };
           index;
           destination;
           json_index
         } =
       Config.get_doc_options config
     in

     mkdirs destination;
     let site_image = Option.map (resolve_logo ~destination) site_image in
     let site_css =
       gen_appended ~destination ~name:"main.css" ~contents:E.Html.embedded_css embed_css
     in
     let site_js =
       gen_appended ~destination ~name:"main.js" ~contents:E.Html.embedded_js embed_js
     in
     let site_head =
       Option.map
         (fun f -> CCIO.with_in (Fpath.to_string f) CCIO.read_all |> CCString.trim)
         embed_head
     in
     let custom =
       let config =
         Config.get_store config |> IlluaminateConfig.Schema.get Doc.Extract.Config.key
       in
       config.module_kinds
     in

     let options resolve : E.Html.Options.t =
       E.Html.Options.make ?site_title ?site_image ?site_url ?site_head ~site_js ~site_css ~resolve
         ~source_link ~data ~custom ()
     in
     let module_options mod_kind =
       options @@ fun x ->
       let open Fpath in
       v x |> relativize ~root:(Fpath.v mod_kind) |> Option.fold ~none:("../" ^ x) ~some:to_string
     in
     ( Fun.flip NMap.iter pages @@ fun _ m ->
       Fun.flip StringMap.iter m @@ fun _ (modu : Doc.Syntax.page Doc.Syntax.documented) ->
       let { page_ref = { namespace = Namespace namespace; id; _ }; _ } = modu.descriptor in
       let options = module_options namespace in
       let path = Fpath.(destination / namespace / (id ^ ".html")) in

       mkdirs Fpath.(destination / namespace);
       E.Html.emit_page ~options ~pages modu
       |> emit_doc
       |> CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] (Fpath.to_string path) );

     let path = Fpath.(destination / "index.html") in
     Option.fold ~none:Html.Default.nil ~some:(parse_index ~options:(options Fun.id)) index
     |> E.Html.emit_index ~options:(options Fun.id) ~pages
     |> emit_doc
     |> CCIO.with_out ~flags:[ Open_creat; Open_trunc; Open_binary ] (Fpath.to_string path);

     if json_index then
       let path = Fpath.(destination / "index.json") in
       let pages =
         NMap.to_seq pages |> Seq.map snd |> Seq.flat_map StringMap.to_seq |> Seq.map snd
         |> List.of_seq
       in
       E.Summary.(everything ~source_link pages |> to_json)
       |> Yojson.Safe.to_file (Fpath.to_string path));
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
      StringSet.iter (Printf.printf " - %s\n") xs)
  in
  let unbound_names, defined_names =
    List.fold_left
      (fun (unbound, defined) program ->
        match program with
        | { Loader.body = None | Some (Markdown _); _ } -> (unbound, defined)
        | { body = Some (Lua parsed); _ } ->
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
    exit 1);
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

  let setup_common { verbose; colour } =
    let style_renderer = Option.map (fun x -> if x then `Ansi_tty else `None) colour in
    Fmt_tty.setup_std_outputs ?style_renderer ();

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
      let+ common = common and+ files = files_arg in
      setup_common common; lint files
    in
    Cmd.v (Cmd.info "lint" ~doc ~man:[ `Blocks common_docs ]) term
  in
  let fix_cmd =
    let doc = "Checks all files and fixes problems in them." in
    let term =
      let+ common = common and+ files = files_arg in
      setup_common common; fix files
    in
    Cmd.v (Cmd.info "fix" ~doc ~man:[ `Blocks common_docs ]) term
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
    Cmd.v (Cmd.info "dump-global" ~doc ~man:[ `Blocks common_docs ]) term
  in
  let doc_gen_cmd =
    let doc = "Generates HTML documentation" in
    let term =
      let+ common = common and+ file = doc_file_arg in
      setup_common common; doc_gen file
    in
    Cmd.v (Cmd.info "doc-gen" ~doc ~man:[ `Blocks common_docs ]) term
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
    Cmd.v (Cmd.info "init-config" ~doc ~man:[ `Blocks common_docs ]) term
  in
  let minify_cmd =
    let doc = "Minify a Lua file" in
    let term =
      let+ common = common and+ file = minify_file_arg in
      setup_common common; minify file
    in
    Cmd.v (Cmd.info "minify" ~doc ~man:[ `Blocks common_docs ]) term
  in

  let info =
    let doc = "Provides basic source code analysis of Lua projects." in
    Cmd.info "illuaminate" ~doc ~version:"illuaminate %%VERSION%%" ~exits:Cmd.Exit.defaults
  in

  Cmd.group info [ lint_cmd; fix_cmd; doc_gen_cmd; dump_global_cmd; init_config_cmd; minify_cmd ]
  |> Cmd.eval |> exit
