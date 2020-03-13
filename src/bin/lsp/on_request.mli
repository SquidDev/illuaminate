open Lsp

(** Handle an LSP request. *)
val handle :
  Rpc.t ->
  Store.t ->
  Types.ClientCapabilities.t ->
  'a Client_request.t ->
  (Store.t * 'a, Jsonrpc.Response.Error.t) result
