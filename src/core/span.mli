(** A file, with both a display name and absolute path *)
type filename =
  { name : string;
    path : string
  }

(** A span in a file or other source program *)
type t =
  { filename : filename;
    start_line : int;
    start_col : int;
    start_bol : int;
    finish_line : int;
    finish_col : int;
    finish_bol : int
  }

val show : t -> string

val pp : Format.formatter -> t -> unit

(** Compare two spans given their filename and position in the file. *)
val compare : t -> t -> int

(** Make a source span from a pair of lexing positions *)
val of_pos2 : filename -> Lexing.position -> Lexing.position -> t

(** Make a source span from a pair of two other spans *)
val of_span2 : t -> t -> t

(** Construct a new span which points to the last character. *)
val finish : t -> t

(** Some spanned value *)
type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
