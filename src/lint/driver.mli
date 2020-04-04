(** The linter driver provides functions for running linters on one or more programs, as well as
    fixing any and all problems they detect. *)

open IlluaminateCore

(** A note at a specific node. *)
module NoteAt : sig
  type 'a t = private
    { note : 'a Linter.note;
      source : 'a;
      kind : 'a Witness.t
    }

  (** Get the span of a note_at. *)
  val span : 'a t -> Span.t

  (** Report a note to an error sink. *)
  val report : Error.t -> 'a t -> unit
end

(** A note of any type. *)
type any_note = private Note : 'a NoteAt.t -> any_note

(** Report a note to an error sink. *)
val report_note : Error.t -> any_note -> unit

module Notes : sig
  type t

  (** Get all notes within the collection. *)
  val to_seq : t -> any_note Seq.t

  (** Get the number of note within the collection. *)
  val size : t -> int

  (** Find all fixes at a specific location. *)
  val find : 'a Witness.t -> 'a -> t -> 'a Linter.note list
end

(** Gather all errors from a specific linter. *)
val need_lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.context ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  Syntax.program ->
  Notes.t

(** Gather all errors from a specific linter. *)
val lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  Syntax.program ->
  Notes.t

(** Fix one or more notes.

    Ideally all notes should come from a single linter. Otherwise we cannot guarantee all fixes can
    be applied safely. *)
val fix : Syntax.program -> Notes.t -> Syntax.program

(** Run the linter, and attempt to fix all issues.

    Returns the updated program, and all notes (irregardless of whether fixed or not). *)
val lint_and_fix_all :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?files:Span.filename * IlluaminateData.Programs.FileStore.t ->
  ?tags:Error.Tag.filter ->
  Linter.t list ->
  Syntax.program ->
  Syntax.program * Notes.t
