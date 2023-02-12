open IlluaminateCore
open Lsp.Types

(** Prepare a renaming operation. *)
val check : IlluaminateData.t -> Position.t -> Span.filename -> Syntax.program -> Range.t option

(** Perform a renaming, either returning a list of edits, or a failure message. *)
val rename :
  IlluaminateData.t ->
  Position.t ->
  string ->
  Span.filename ->
  Syntax.program ->
  (TextEdit.t list, string) result
