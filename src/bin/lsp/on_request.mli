open Lsp

(** Handle an LSP request. *)
val handle :
  Store.client_channel ->
  Store.t ->
  'a Client_request.t ->
  ('a, Jsonrpc.Response.Error.t) result Fiber.t
