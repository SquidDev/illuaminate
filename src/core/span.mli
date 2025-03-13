open Illuaminate

(** A builder, which tracks the start position of newlines, allowing you to map between the two. *)
module Lines : sig
  type t

  (** Lex using this line builder. This sets up the lexer with the appropriate positions and then
      passes the lines builder to the given lexing/parsing function. This object may then be used to
      construct positions, using {!of_pos2}. *)
  val using : File_id.t -> Lexing.lexbuf -> (t -> 'a) -> 'a

  val position_map : t -> Position_map.t

  (** Wraps {!Lexing.new_line}, also tracking it within the context. *)
  val new_line : t -> unit
end

(** A span in a file or other source program *)
type t

(** Get the filename for this span. *)
val filename : t -> File_id.t

(** Convert this span to an {! Illuaminate.Error.Position.t error position}. *)
val to_error_position : t -> Illuaminate.Error.Position.t

(** Create a span from an {! Illuaminate.Error.Position.t error position}. *)
val of_error_position : Illuaminate.Error.Position.t -> t

(** Get the start line of this span.*)
val start_line : t -> int

(** Get the beginning of the starting line for this span. *)
val start_bol : t -> int

(** Get the beginning offset of this span. *)
val start_offset : (t, int) Lens.lens'

(** A lens over the starting column of this span. *)
val start_col : t -> int

(** A lens over the starting line and column of this span. *)
val start_pos : t -> int * int

(** Get the end line this span. *)
val finish_line : t -> int

(** A lens over the end column of this span. *)
val finish_col : t -> int

(** A lens over the end line and column of this span. *)
val finish_pos : t -> int * int

(** Get the end offset of this span. *)
val finish_offset : (t, int) Lens.lens'

val show : t -> string
val pp : Format.formatter -> t -> unit

(** Compare two spans given their filename and position in the file. *)
val compare : t -> t -> int

(** Hash a span *)
val hash : t -> int

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
