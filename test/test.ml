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

    let run = function
      | { verbose = 0 } -> None
      | { verbose = n } ->
          Logs.set_level ~all:true (Some (if n > 1 then Debug else Info));
          Logs.format_reporter () |> Logs.set_reporter;
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
         group "Documentation" [ Doc_parser.tests; Doc_extract.tests ]
       ]
