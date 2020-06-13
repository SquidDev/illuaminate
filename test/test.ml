open Omnomnom.Tests

(** A reporter which enables logging. *)
let logging : Omnomnom.Ingredients.reporter =
  let reporter () =
    let open Omnomnom.Formatting in
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
        | Logs.Debug -> (DullColor Green, "DEBG")
      in
      pp_with out style (Option.value ~default header)
    in
    Logs.format_reporter ~pp_header ()
  in

  ( module struct
    type options = { verbose : int }

    let options =
      let open Cmdliner in
      let open Cmdliner.Arg in
      let verbose =
        value & flag_all
        & info ~docs:Cmdliner.Manpage.s_common_options ~doc:"With verbose messages"
            [ "verbose"; "v" ]
      in

      Term.(const (fun v -> { verbose = List.length v }) $ verbose)

    let run verbose =
      reporter () |> Logs.set_reporter;
      let level =
        match verbose with
        | { verbose = 0 } -> Logs.Error
        | { verbose = 1 } -> Info
        | { verbose = _ } -> Debug
      in
      Logs.set_level ~all:true (Some level);
      None
  end )

let () =
  Omnomnom.run ~reporters:Omnomnom.Ingredients.[ console_reporter; OmnomnomJUnit.reporter; logging ]
  @@ group "illuaminate"
       [ Parser.tests;
         Lexer.tests;
         Config.tests;
         Lint.tests;
         Pattern.tests;
         Reprint.tests;
         Data.tests;
         Span.tests;
         Config_format.tests;
         group "Documentation"
           [ Doc_parser.tests;
             Doc_emit.Json_summary.tests;
             Doc_emit.Html_module.tests;
             Doc_emit.Dump_sexp.tests
           ];
         group "Language server"
           [ Lsp_code_action.tests;
             Lsp_declaration.tests;
             Lsp_definition.tests;
             Lsp_diagnostic.tests;
             Lsp_highlight.tests;
             Lsp_locate.tests;
             Lsp_reference.tests;
             Lsp_rename.tests;
             Lsp_workspace_symbol.tests;
             Lsp_leak.tests
           ]
       ]
