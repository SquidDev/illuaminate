(** Parses glob-like patterns, in the style of Git's {{:https://git-scm.com/docs/gitignore}
    gitignore patterns}:

    - If there's a directory separator at the beginning or middle of the pattern, it matches files
      relative to the current directory. Otherwise it will match here or on any child directory.

    - If it is at the end, it will match directories and any children of that directory.

    - [*] matches 0 or more non-[/] characters, [?] matches one non-[/] character.

    - [**] matches 0 or more directories. (so [**/foo] matches [foo], [bar/foo], [bar/baz/foo],
      etc...). *)

type t

(** Parse a glob, converting it to a pattern. *)
val parse : string -> t

(** Determine if this directory matches a given pattern. *)
val matches : string -> t -> bool

(** Additional utilities for working with paths. *)
module Paths : sig
  (** Convert a potentially file to an absolute one, if it is not already absolute. *)
  val to_absolute : ?cwd:string -> string -> string

  (** Convert one path so that it is relative to another. It is recommended that both paths are
      absolute, but that is not required.

      This only returns a value if path is a child of dir, otherwise the absolute path should be
      used. We do not introduce '..', as that is not correct in the presence of symlinks. *)
  val make_relative : path:string -> dir:string -> string option
end
