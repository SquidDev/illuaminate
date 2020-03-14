open IlluaminateCore

type client_channel =
  { notify : Lsp.Server_notification.t -> unit;
    request : 'a. 'a Lsp.Server_request.t -> unit
  }

module Filename : sig
  val of_uri : Lsp.Uri.t -> Span.filename

  val to_uri : Span.filename -> Lsp.Uri.t

  val to_uri_json : Span.filename -> Yojson.Safe.t

  val box : Lsp.Types.DocumentUri.t -> Lsp.Uri.t
end

type document = private
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : Lsp.Text_document.t;  (** The file's contents. *)
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result
        (** The result of parsing the file, or nil if not available. *)
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
