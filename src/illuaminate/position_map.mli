(** A span in a file or other source program *)
type t

type line_map := t

(** A position inside the the file. *)
type pos = Pos of int [@@unboxed]

(** A one-indexed line number inside the file *)
type line = private int

(** A one-indexed column number inside the file. *)
type column = private int

(** A builder, which tracks the start position of newlines, allowing you to map between the two. *)
module Builder : sig
  type t

  (** Lex using this line builder. This sets up the lexer with the appropriate positions and then
      passes the lines builder to the given lexing/parsing function. This object may then be used to
      construct positions, using {!of_pos2}. *)
  val using : Lexing.lexbuf -> (t -> line_map -> 'a) -> 'a * line_map

  (** Wraps {!Lexing.new_line}, also tracking it within the context. *)
  val new_line : t -> unit
end

(** Get the start line of this span. *)
val position_of : t -> pos -> line * column

(** Get the start of the line for this position. *)
val position_bol : t -> pos -> pos

(** The maximum position in this map. *)
val max_position : t -> pos
