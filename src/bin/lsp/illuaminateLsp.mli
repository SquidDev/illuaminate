(** The core library for Language server protocol support. *)

open Lsp

type client_channel =
  { notify : Server_notification.t -> unit Fiber.t;
    request : 'a. 'a Server_request.t -> ('a, Jsonrpc.Response.Error.t) result Fiber.t
  }

type server_channel =
  { request :
      'res.
      client_channel -> 'res Client_request.t -> ('res, Jsonrpc.Response.Error.t) result Fiber.t;
    notify : client_channel -> Client_notification.t -> unit Fiber.t
  }

(** Construct a new LSP server *)
val server : unit -> server_channel
