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

    Lsp.Logger.with_log_file log_file (fun () ->
        let open IlluaminateLsp in
        let server = server () in
        let add_store r = Result.map (fun x -> ((), x)) r in
        let add_store' r = add_store r |> Fiber.return in
        let wrap rpc : client_channel =
          { notify = Lsp.Rpc.send_notification rpc;
            request = (fun r -> Ugly_hacks.send_request rpc r)
          }
        in
        Log.info (fun f -> f "Starting server");
        Lsp.Rpc.start ()
          { on_initialize = (fun rpc () p -> server.initialize (wrap rpc) p |> add_store');
            on_request = (fun rpc () cap req -> server.request (wrap rpc) cap req |> add_store');
            on_notification = (fun rpc () noti -> server.notify (wrap rpc) noti |> Fiber.return)
          }
          stdin stdout
        |> Fiber.run;
        Log.info (fun f -> f "Stopping server"))
  in

  let cmd =
    ( const setup $ log_file $ verbose,
      info "illuaminate-lsp" ~doc:"Start a Language Server Protocol sever for illuaminate" )
  in
  exit @@ eval cmd
