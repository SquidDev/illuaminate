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

(** A span in a file or other source program *)
type t

(** Get the filename for this span. *)
val filename : t -> filename

(** A lens over the starting line of this span. *)
val start_line : (t, int) Lens.lens'

(** A lens over the starting column of this span. *)
val start_col : (t, int) Lens.lens'

(** Get the beginning of the starting line for this span. *)
val start_bol : t -> int

(** A lens over the last column of this span. *)
val finish_line : (t, int) Lens.lens'

(** A lens over the last line of this span. *)
val finish_col : (t, int) Lens.lens'

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
