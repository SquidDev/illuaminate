let src = Logs.Src.create ~doc:"The main server loop" "illuaminate-lsp"

module Log = (val Logs.src_log src)

let to_title : Logs.level -> Lsp.Logger.Title.t = function
  | App | Info -> Info
  | Error -> Error
  | Warning -> Warning
  | Debug -> LocalDebug

let () =
  let open Cmdliner in
  let open Term in
  let log_file =
    let open Arg in
    value
    & opt (some string) None
    & info [ "log" ] ~docv:"FILE" ~doc:"Enable logging to FILE (or - for stdout)."
  in
  let verbose =
    let open Arg in
    value & flag_all
    & info ~docs:Cmdliner.Manpage.s_common_options
        ~doc:
          "Show log messages. One $(b,-v) shows errors, warnings and information messages. \
           Additional usages will also show debug messages."
        [ "verbose"; "v" ]
  in

  let reporter : Logs.reporter =
    { report =
        (fun src level ~over k msgf ->
          let k out =
            Lsp.Logger.log ~section:(Logs.Src.name src) ~title:(to_title level) "%s" out;
            over ();
            k ()
          in
          msgf (fun ?header:_ ?tags:_ -> Format.kasprintf k))
    }
  in
  let setup log_file verbose =
    if Option.is_some log_file then (
      let level =
        match verbose with
        | [] -> Logs.Warning
        | [ _ ] -> Logs.Info
        | _ :: _ :: _ -> Logs.Debug
      in

      Logs.set_level ~all:true (Some level);
      Logs.set_reporter reporter );

    let open Lsp in
    Logger.with_log_file log_file (fun () ->
        let open IlluaminateLsp in
        let server = server () in
        let wrap rpc : client_channel =
          { notify = Server.notification rpc; request = (fun x -> Server.request rpc x) }
        in
        Log.info (fun f -> f "Starting server");
        let handler =
          let add_store r = Fiber.map ~f:(Result.map (fun x -> (x, ()))) r in
          let on_notification rpc p = server.notify (wrap rpc) p in
          let on_request rpc req = server.request (wrap rpc) req |> add_store in
          Server.Handler.make ~on_notification ~on_request:{ on_request } ()
        in
        let scheduler = Scheduler.create () in
        let stream = Io.make stdin stdout |> Fiber_io.make scheduler in
        Server.make handler stream () |> Server.start |> Scheduler.run scheduler;
        Log.info (fun f -> f "Stopping server"))
  in

  let cmd =
    ( const setup $ log_file $ verbose,
      info "illuaminate-lsp" ~doc:"Start a Language Server Protocol sever for illuaminate" )
  in
  exit @@ eval cmd
