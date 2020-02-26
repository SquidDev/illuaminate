open IlluaminateSemantics
open IlluaminateCore

module Table : Hashtbl.S with type key = Lsp.Uri.t

module UriSet : CCHashSet.S with type elt = Lsp.Uri.t

type document = private
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : Lsp.Text_document.t option;
        (** The file's contents, if it is open in an editor. *)
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
        (** The result of parsing the file, or nil if not available. *)
    mutable file : Data.Files.id option;  (** The current file's ID *)
    context : Data.context
  }

type t

(** Create a new store. *)
val create : unit -> t

(** Create a new file and load it.*)
val create_file : t -> Lsp.Text_document.t -> document
