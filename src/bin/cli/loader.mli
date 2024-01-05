open IlluaminateCore
module Config := IlluaminateConfigFormat
module Data := IlluaminateData

type file =
  { root : Fpath.t;
    path : Fpath.t;
    file : Illuaminate.File_id.t;
    config : Config.t;
    body : File.t option
  }

(** Attempt to load files from a directory, skipping if there is a configuration error. *)
val load_from :
  ?root:Fpath.t ->
  Fpath.t ->
  (Config.t * file list * (Data.Builder.t -> unit)) option * Illuaminate.Error.t list

(** Attempt to load files from multiple directories. *)
val load_from_many :
  ?root:Fpath.t -> Fpath.t list -> file list * (Data.Builder.t -> unit) * Illuaminate.Error.t list
