let () =
  let open Cmdliner in
  let open Term in
  let log_file =
    let open Arg in
    value
    & opt (some string) None
    & info [ "log" ] ~docv:"FILE" ~doc:"Enable logging to FILE (or - for stdout)."
  in

  let reporter : Logs.reporter =
    { report =
        (fun src level ~over k msgf ->
          let k out =
            Lsp.Logger.log ~section:(Logs.Src.name src)
              ~title:(Logs.level_to_string (Some level))
              "%s" out;
            over ();
            k ()
          in
          msgf (fun ?header:_ ?tags:_ -> Format.kasprintf k))
    }
  in
  let setup log_file =
    if Option.is_some log_file then (
      Logs.set_level ~all:true (Some Debug);
      Logs.set_reporter reporter );
    Lsp.Logger.with_log_file log_file Illuaminate_lsp_core.main
  in

  let cmd =
    ( const setup $ log_file,
      info "illuaminate-lsp" ~doc:"Start a Language Server Protocol sever for illuaminate" )
  in
  exit @@ eval cmd
