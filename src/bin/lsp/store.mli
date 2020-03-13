open IlluaminateCore

module Filename : sig
  val of_uri : Lsp.Uri.t -> Span.filename

  val to_uri : Span.filename -> Lsp.Uri.t

  val to_uri_json : Span.filename -> Yojson.Safe.t

  val box : Lsp.Types.DocumentUri.t -> Lsp.Uri.t
end

module FileDigest : sig
  type t
end

module Workspace : sig
  type t
end

type contents =
  | Open of Lsp.Text_document.t  (** The current file's contents. *)
  | FromFile of FileDigest.t
      (** This file was loaded from the filesystem. We store a hash of its contents, to determine if
          the file has changed.

          We just use the built-in {!Digest} module - this uses MD5, so it's by no means "secure",
          but this only needs to serve as an efficient checksum. *)

type document = private
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : contents;  (** The file's contents. *)
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
        (** The result of parsing the file, or nil if not available. *)
    mutable file : IlluaminateData.Programs.Files.id option;  (** The current file's ID *)
    mutable workspace : Workspace.t option
  }

type t

val data : t -> IlluaminateData.t

(** Create a new store. *)
val create : unit -> t

(** Update the list of workspaces. *)
val set_workspace : t -> ?root:Lsp.Uri.t -> Lsp.Types.WorkspaceFolder.t list -> unit

(** Get a document if it is open. *)
val get_file : t -> Lsp.Uri.t -> document option

(** Create or open a file. *)
val open_file : t -> Lsp.Text_document.t -> document

(** Update the contents of a file. *)
val update_file : t -> document -> Lsp.Text_document.t -> unit
