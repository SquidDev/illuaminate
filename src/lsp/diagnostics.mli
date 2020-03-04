(** Lint a program, producing a series of diagnostics. *)
val lint : Store.t -> Store.document -> Lsp.Protocol.PublishDiagnostics.diagnostic list
