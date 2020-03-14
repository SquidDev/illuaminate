open Lsp

val handle : Store.client_channel -> Store.t -> Client_notification.t -> (unit, string) result
