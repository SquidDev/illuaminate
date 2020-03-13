open Lsp
open Lsp.Types

val handle : Rpc.t -> Store.t -> InitializeParams.t -> (Store.t * InitializeResult.t, string) result
