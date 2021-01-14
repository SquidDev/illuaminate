(** Represents the contents of various files supported by illuaminate. This contains both Lua ones
    ({!Syntax.program}) and those with Markdown front-matter. *)

type t =
  | Markdown of
      { attributes : (string Span.spanned * string Span.spanned) list;
        contents : string Span.spanned
      }
      (** A markdown file (for instance a tutorial or long-form documentation). This is conceptually
          the same as one long documentation comment. *)
  | Lua of Syntax.program  (** A Lua program. *)

val ( = ) : t -> t -> bool

(** A lens to return the first token in the term, much like with {!First}. *)
val first : (t, Syntax.token) Lens.lens'

(** A lens to return the last token in the term, much like with {!Last}. *)
val last : (t, Syntax.token) Lens.lens'

(** Get the span of this term, excluding leading/trailing trivia. *)
val span : t -> Span.t

(** Emit this term, using {!Emit}. *)
val emit : Format.formatter -> t -> unit
