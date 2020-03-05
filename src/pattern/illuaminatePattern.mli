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
val matches : Fpath.t -> t -> bool

(** Locate all files matching a glob within a folder, calling the accepting function on them. *)
val iter : (Fpath.t -> unit) -> ?path:Fpath.t -> root:Fpath.t -> t -> unit

(** A version of {!iter}, which may accept multiple overlapping globs. *)
val iter_all : (Fpath.t -> unit) -> ?path:Fpath.t -> root:Fpath.t -> t list -> unit
