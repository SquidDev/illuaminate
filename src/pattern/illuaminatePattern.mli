(** Parses glob-like patterns, in the style of Git's gitignore patterns [1]:

    - If there's a directory separator at the beginning or middle of the pattern, it matches files
      relative to the current directory. Otherwise it will match here or on any child directory.

    - If it is at the end, it will match directories and any children of that directory.

    - "*" matches 0 or more non-"/" characters, "?" matches one non-"/" character.

    - "**" matches 0 or more directories. (so "**/foo" matches "foo", "bar/foo", "bar/baz/foo",
      etc...).

    [1]: https://git-scm.com/docs/gitignore *)

type t

(** Parse a glob, converting it to a pattern. *)
val parse : string -> t

(** Determine if this directory matches a given pattern. *)
val matches : string -> t -> bool
