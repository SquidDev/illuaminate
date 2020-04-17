open IlluaminateSemantics.Doc.Syntax

(** An item in the index of documented terms. *)
type t =
  { name : string;
    full_name : string;
    summary : string option;
    source : string option
  }

(** Convert a list of index items into a JSON index. *)
val to_json : t list -> Yojson.Safe.t

(** Convert a list of documented modules into a flat list of index terms. *)
val everything :
  source_link:(IlluaminateCore.Span.t -> string option) -> module_info documented list -> t list
