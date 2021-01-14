open IlluaminateSemantics.Doc
open Syntax

(** An item in the index of documented terms. *)
type t =
  { name : string;
    full_name : string;
    summary : string option;
    source : string option;
    in_module : IlluaminateSemantics.Namespace.Ref.t;
    name_of : IlluaminateSemantics.Reference.name_of
  }

(** Convert a list of index items into a JSON index. *)
val to_json : t list -> Yojson.Safe.t

(** Convert a list of documented modules into a flat list of index terms. *)
val everything :
  source_link:(AbstractSyntax.source -> string option) -> page documented list -> t list
