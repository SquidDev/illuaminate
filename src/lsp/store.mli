open IlluaminateSemantics
open IlluaminateCore

type document = private
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : Lsp.Text_document.t option;
        (** The file's contents, if it is open in an editor. *)
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
        (** The result of parsing the file, or nil if not available. *)
    mutable file : IlluaminateData.Programs.Files.id option;  (** The current file's ID *)
    context : IlluaminateData.Programs.Context.t
  }

type t

val data : t -> IlluaminateData.t

(** Create a new store. *)
val create : unit -> t

(** Create a new file and load it.*)
val create_file : t -> Lsp.Text_document.t -> document

(** Reload a file from an updated contents. *)
val update_file : t -> document -> unit
