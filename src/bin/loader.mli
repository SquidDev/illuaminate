open IlluaminateCore

type file =
  { path : Fpath.t;
    file : Span.filename;
    file_id : Data.Files.id option;
    config : Config.t;
    parsed : Syntax.program option
  }

type t

val create : ?root:Fpath.t -> Error.t -> t

val get_config : loader:t -> Fpath.t -> Config.t option

(** Attempt to load files from a directory, skipping if there is a configuration error. *)
val load_from : loader:t -> Fpath.t -> (Config.t * file list * Data.Files.t) option

(** Attempt to load files from multiple directories. *)
val load_from_many : loader:t -> Fpath.t list -> file list * Data.Files.t
