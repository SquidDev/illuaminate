open Lsp

val handle : Rpc.t -> Store.t -> Client_notification.t -> (Store.t, string) result
