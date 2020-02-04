type t =
  | Unknown
  | InManual of string
  | Undocumented

(** Get a link to a section of the manual *)
val manual_section : string -> string

(** Convert a Lua reference to a url. *)
val to_url : t -> string option

(** Resolve a Lua (5.1) name and convert it into a section of the manual *)
val lookup_name : string -> t

(** Result a Lua (5.1) type and convert it into a section of the manual *)
val lookup_type : string -> t
