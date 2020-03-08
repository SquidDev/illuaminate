let src = Logs.Src.create ~doc:"The main server loop" "illuaminate-lsp"

module Log = (val Logs.src_log src)

let main () =
  Log.info (fun f -> f "Starting server");

  Lsp.Rpc.start (Store.create ())
    { on_initialize = On_init.handle;
      on_request = On_request.handle;
      on_notification = On_notification.handle
    }
    stdin stdout;
  Log.info (fun f -> f "Stopping server")
