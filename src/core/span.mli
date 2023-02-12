(** A file, with both a display name and absolute path *)
module Filename : sig
  type t = private
    { name : string;  (** A "display" name of the file, which can be shown to the user. *)
      path : Fpath.t option;  (** An absolute path this file may exist at. *)
      id : string;
          (** A unique identifier for this file. This may be the path, or some other piece of data.

              Generally the meaning/contents of this will vary depending on what program/library is
              constructing the spans, and thus should not be understood to have any specific
              meaning. *)
      hash : int  (** A cached hash of [id] *)
    }

  val pp : Format.formatter -> t -> unit

  include Hashtbl.HashedType with type t := t
  include Set.OrderedType with type t := t

  (** Construct a new filename from a unique id, with an optional on-disk file and display name. *)
  val mk : ?path:Fpath.t -> ?name:string -> string -> t
end

type filename = Filename.t = private
  { name : string;
    path : Fpath.t option;
    id : string;
    hash : int
  }

(** A builder, which tracks the start position of newlines, allowing you to map between the two. *)
module Lines : sig
  type t

  (** Lex using this line builder. This sets up the lexer with the appropriate positions and then
      passes the lines builder to the given lexing/parsing function. This object may then be used to
      construct positions, using {!of_pos2}. *)
  val using : filename -> Lexing.lexbuf -> (t -> 'a) -> 'a

  (** Wraps {!Lexing.new_line}, also tracking it within the context. *)
  val new_line : t -> unit
end

(** A span in a file or other source program *)
type t

(** Get the filename for this span. *)
val filename : t -> filename

(** Get the start line of this span.*)
val start_line : t -> int

(** Get the beginning of the starting line for this span. *)
val start_bol : t -> int

(** Get the beginning offset of this span. *)
val start_offset : (t, int) Lens.lens'

(** A lens over the starting column of this span. *)
val start_col : (t, int) Lens.lens'

(** A lens over the starting line and column of this span. *)
val start_pos : (t, int * int) Lens.lens'

(** Get the end line this span. *)
val finish_line : t -> int

(** A lens over the end column of this span. *)
val finish_col : (t, int) Lens.lens'

(** A lens over the end line and column of this span. *)
val finish_pos : (t, int * int) Lens.lens'

(** Get the end offset of this span. *)
val finish_offset : (t, int) Lens.lens'

val show : t -> string
val pp : Format.formatter -> t -> unit

(** Compare two spans given their filename and position in the file. *)
val compare : t -> t -> int

(** Make a source span from a pair of lexing positions *)
val of_pos2 : Lines.t -> Lexing.position -> Lexing.position -> t

(** Make a source span from a pair of two other spans *)
val of_span2 : t -> t -> t

(** Construct a new span which points to the first character. *)
val start : t -> t

(** Construct a new span which points to the last character. *)
val finish : t -> t

(** Some spanned value *)
type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
