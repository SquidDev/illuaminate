(** The core library for Language server protocol support. *)

open Lsp
open Lsp.Types

type client_channel =
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

(** Construct a new LSP server *)
val server : unit -> server_channel
