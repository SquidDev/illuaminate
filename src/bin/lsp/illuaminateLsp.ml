open Lsp
open Lsp.Types

type client_channel = Store.client_channel =
  { notify : Server_notification.t -> unit;
    request : 'a. 'a Server_request.t -> unit
  }

type server_channel =
  { initialize : client_channel -> InitializeParams.t -> (InitializeResult.t, string) result;
    request :
      'res. client_channel -> ClientCapabilities.t -> 'res Client_request.t ->
      ('res, Jsonrpc.Response.Error.t) result;
    notify : client_channel -> Client_notification.t -> (unit, string) result
  }

let server () =
  let store = Store.create () in
  { initialize = (fun c param -> On_init.handle c store param);
    request = (fun c req -> On_request.handle c store req);
    notify = (fun c noti -> On_notification.handle c store noti)
  }
