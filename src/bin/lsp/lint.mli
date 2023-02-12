open IlluaminateCore
open Lsp.Types

(** All notes for a given program. *)
val notes : IlluaminateLint.Driver.Note.any array IlluaminateData.Programs.key

(** Lint a program and export any diagnostics. *)
val diagnostics : Store.t -> Store.document -> Diagnostic.t list

(** Get any code actions for a given range. *)
val code_actions : Store.t -> Span.filename -> Syntax.program -> Range.t -> CodeActionResult.t

(** Fix a program given a specific id, or fail. *)
val fix : Store.t -> Span.filename -> int -> (TextEdit.t, string) result
