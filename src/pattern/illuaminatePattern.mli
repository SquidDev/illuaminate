(** Parses glob-like patterns, in the style of Git's {{:https://git-scm.com/docs/gitignore}
    gitignore patterns}:

    - If there's a directory separator at the beginning or middle of the pattern, it matches files
      relative to the current directory. Otherwise it will match here or on any child directory.

    - If it is at the end, it will match directories and any children of that directory.

    - [*] matches 0 or more non-[/] characters, [?] matches one non-[/] character.

    - [**] matches 0 or more directories. (so [**/foo] matches [foo], [bar/foo], [bar/baz/foo],
      etc...). *)

(** A pattern which can be matched against, or used to direct a tree walk. *)
type t

val pp : Format.formatter -> t -> unit

(** Parse a glob, converting it to a pattern. *)
val parse : string -> t

(** Determine if this directory matches a given pattern. *)
val matches : Fpath.t -> t -> bool

(** Locate all files matching a glob within a folder, calling the accepting function on them. *)
val iter : (Fpath.t -> unit) -> ?path:Fpath.t -> root:Fpath.t -> t -> unit

(** A union of patterns. *)
module Union : sig
  type pattern := t

  (** A collection representing a union of multiple patterns. *)
  type t

  (** Construct a union from a list of patterns. *)
  val of_list : pattern list -> t

  (** Append two globs together. *)
  val union : t -> t -> t

  (** Determine if this directory matches any given pattern. *)
  val matches : Fpath.t -> t -> bool

  (** A version of {!IlluaminatePattern.iter} which works on a union of patterns instead. This is
      more optimal than iterating over each pattern individually. *)
  val iter : (Fpath.t -> unit) -> ?path:Fpath.t -> root:Fpath.t -> t -> unit
end
