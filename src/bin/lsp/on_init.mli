open Lsp.Types

val handle :
  Store.client_channel -> Store.t -> InitializeParams.t -> (InitializeResult.t, string) result
