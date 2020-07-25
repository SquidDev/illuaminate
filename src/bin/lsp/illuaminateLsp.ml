open Lsp

type client_channel = Store.client_channel =
  { notify : Server_notification.t -> unit Fiber.t;
    request : 'a. 'a Server_request.t -> ('a, Jsonrpc.Response.Error.t) result Fiber.t
  }

type server_channel =
  { request :
      'res. client_channel -> 'res Client_request.t ->
      ('res, Jsonrpc.Response.Error.t) result Fiber.t;
    notify : client_channel -> Client_notification.t -> unit Fiber.t
  }

let server () : server_channel =
  let store = Store.create () in
  { request = (fun c req -> On_request.handle c store req);
    notify = (fun c noti -> On_notification.handle c store noti)
  }
