open Lsp

val handle :
  Rpc.t -> Store.t -> Lsp.Initialize.Params.t -> (Store.t * Initialize.Result.t, string) result
