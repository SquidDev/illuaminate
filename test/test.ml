open Omnomnom.Tests

(** A reporter which enables logging. *)
let logging : Omnomnom.Ingredients.reporter =
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
      Logs.format_reporter () |> Logs.set_reporter;
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
  Omnomnom.run
    ~reporter:
      Omnomnom.Ingredients.(
        compose_reporters console_reporter OmnomnomJUnit.reporter |> compose_reporters logging)
  @@ group "illuaminate"
       [ Parser.tests;
         Lexer.tests;
         Config.tests;
         Lint.tests;
         Pattern.tests;
         Reprint.tests;
         Data.tests;
         group "Documentation" [ Doc_parser.tests; Doc_extract.tests ];
         group "Language server"
           [ Lsp_diagnostic.tests;
             Lsp_definition.tests;
             Lsp_declaration.tests;
             Lsp_reference.tests;
             Lsp_highlight.tests
           ]
       ]
