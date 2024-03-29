(** The linter driver provides functions for running linters on one or more programs, as well as
    fixing any and all problems they detect. *)

open IlluaminateCore

(** A note at a specific node. *)
module Note : sig
  (** A linter message which will be attached to a specific node. *)
  type 'a t = private
    { error : Illuaminate.Error.t;  (** The actual error. *)
      source : 'a;  (** The source node we're attached to. *)
      fix : 'a Linter.Fixer.t;
          (** A function which will be used to fix up this warning. Note, this function should not
              make any assumptions about the form of this node. *)
      kind : 'a Witness.t  (** The kind of source node. *)
    }

  (** Report a note to an error sink. *)
  val to_error : 'a t -> Illuaminate.Error.t

  (** A note of any type. *)
  type any = private Note : 'a t -> any [@@unboxed]

  (** Report a note to an error sink. *)
  val any_to_error : any -> Illuaminate.Error.t
end

module Notes : sig
  type t

  (** Get all notes within the collection. *)
  val to_seq : t -> Note.any Seq.t

  (** Get the number of note within the collection. *)
  val size : t -> int

  (** Find all fixes at a specific location. *)
  val find : 'a Witness.t -> 'a -> t -> 'a Note.t list
end

(** Gather all errors from a specific linter. *)
val need_lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.context ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  File.t ->
  Notes.t

(** Gather all errors from a specific linter. *)
val lint :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?tags:Error.Tag.filter ->
  Linter.t ->
  File.t ->
  Notes.t

(** Fix one or more notes.

    Ideally all notes should come from a single linter. Otherwise we cannot guarantee all fixes can
    be applied safely. *)
val fix : File.t -> Notes.t -> File.t

(** Run the linter, and attempt to fix all issues.

    Returns the updated program, and all notes (irregardless of whether fixed or not). *)
val lint_and_fix_all :
  store:IlluaminateConfig.Schema.store ->
  data:IlluaminateData.t ->
  ?files:Illuaminate.File_id.t * IlluaminateData.Programs.FileStore.t ->
  ?tags:Error.Tag.filter ->
  Linter.t list ->
  File.t ->
  File.t * Notes.t
