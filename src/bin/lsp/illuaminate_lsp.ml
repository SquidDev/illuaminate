let src = Logs.Src.create ~doc:"The main server loop" "illuaminate-lsp"

module Log = (val Logs.src_log src)

let to_title : Logs.level -> Lsp.Logger.Title.t = function
  | App | Info -> Info
  | Error -> Error
  | Warning -> Warning
  | Debug -> LocalDebug

let start () =
  let open Lsp_fiber in
  let open Fiber_unix in
  let scheduler = Scheduler.create () in
  let stream =
    let io = Lsp.Io.make stdin stdout in
    Lsp_fiber.Fiber_io.make scheduler io
  in
  let detached = Fiber_detached.create () in

  Log.info (fun f -> f "Starting server");
  let handler =
    let open IlluaminateLsp in
    let wrap (server : unit Server.t) : client_channel =
      { notify = Server.notification server; request = (fun x -> Server.request server x) }
    in

    let server = IlluaminateLsp.server () in
    let on_notification rpc p = server.notify (wrap rpc) p in
    let on_request rpc req =
      let open Fiber.O in
      let+ res = server.request (wrap rpc) req in
      ( Rpc.Reply.now
          (match res with
          | Ok r -> Ok r
          | Error e -> Error e),
        () )
    in
    Server.Handler.make ~on_notification ~on_request:{ on_request } ()
  in

  let server = Server.make handler stream () in
  Fiber.fork_and_join_unit
    (fun () ->
      let open Fiber.O in
      let* () = Server.start server in
      Fiber_detached.stop detached)
    (fun () -> Fiber_detached.run detached)
  |> Scheduler.run scheduler;
  Log.info (fun f -> f "Stopping server")

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
      Logs.set_reporter reporter);

    let open Lsp in
    Logger.with_log_file log_file (fun () -> start)
  in

  let cmd =
    ( const setup $ log_file $ verbose,
      info "illuaminate-lsp" ~doc:"Start a Language Server Protocol sever for illuaminate" )
  in
  exit @@ eval cmd
