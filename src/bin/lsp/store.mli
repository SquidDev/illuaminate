open IlluaminateCore

module Filename : sig
  val of_uri : Lsp.Uri.t -> Span.filename

  val to_uri : Span.filename -> Lsp.Uri.t

  val to_uri_json : Span.filename -> Yojson.Safe.t
end

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

(** Get a document if it is open. *)
val get_file : t -> Lsp.Protocol.documentUri -> document option

(** Create or open a file. *)
val open_file : t -> Lsp.Text_document.t -> document

(** Update the contents of a file. *)
val update_file : t -> document -> Lsp.Text_document.t -> unit
