(** The linter driver provides functions for running linters on one or more programs, as well as
    fixing any and all problems they detect. *)

open IlluaminateCore

(** A note at a specific node. *)
module NoteAt : sig
  type 'a t

  (** Get the note of a note_at. *)
  val note : 'a t -> 'a Linter.note

  (** Get the span of a note_at. *)
  val span : 'a t -> Span.t

  (** Report a note to an error sink. *)
  val report : Error.t -> 'a t -> unit
end

(** A note of any type. *)
type any_note = private Note : 'a NoteAt.t -> any_note

(** Report a note to an error sink. *)
val report_note : Error.t -> any_note -> unit

(** Gather all errors from a specific linter. *)
val need_lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.context ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  Syntax.program ->
  any_note list

(** Gather all errors from a specific linter. *)
val lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  Syntax.program ->
  any_note list


(** Fix one or more notes.

    Ideally all notes should come from a single linter. Otherwise we cannot guarantee all fixes can
    be applied safely. *)
val fix : Syntax.program -> any_note list -> Syntax.program

(** Run the linter, and attempt to fix all issues.

    Returns the updated program, and all notes (irregardless of whether fixed or not). *)
val lint_and_fix_all :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?id:IlluaminateData.Programs.Files.id ->
  ?tags:Error.Tag.filter ->
  Linter.t list ->
  Syntax.program ->
  Syntax.program * any_note list
