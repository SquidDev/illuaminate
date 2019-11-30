(** {!Config} provides a very basic way of declaring configuration options using combinators.

    Config files are built from s-expressions, and so the parsing combinators reflect the underlying
    structure of the file. *)

(** Represents a configured term.

    This effectively acts as a representation of the underlying configuration file, merged together
    using combinators to build nested data structures. *)
module Kind : sig
  type term

  type fields
end

(** An arbitrary parser for terms. This allows some combinators to span over fields and base terms. *)
type ('a, 'kind) parser

(** The main term parser. This consumes a list of sexprs and yields an arbitrary value. *)
type 'a t = ('a, Kind.term) parser

(** An axillary parser for key-value pairs. *)
type 'a fields = ('a, Kind.fields) parser

(* (\** Get the default value of this term. *\)
 * val default : ('a, 'kind) parser -> 'a *)

(** {3 Basic combinators} *)

(** An empty parser which does nothing. *)
val unit : (unit, 'kind) parser

(** A parser which always yields a value. *)
val const : 'a -> ('a, 'kind) parser

(** Functor-style for parsers, allowing you to apply a function over a parser's result. *)
val ( let+ ) : ('a, 'kind) parser -> ('a -> 'b) -> ('b, 'kind) parser

(** Applicative functor-style syntax for parsers, allowing you to combine parsers. *)
val ( and+ ) : ('a, 'kind) parser -> ('b, 'kind) parser -> ('a * 'b, 'kind) parser

(** {3 Value parsers} *)

(** Parse a boolean, only accepting the atoms "true" and "false" *)
val bool : bool t

(** Parse some arbitrary atom. *)
val atom : ty:string -> (string -> 'a option) -> 'a t

(** Parse some arbitrary atom, allowing returning custom error messages. *)
val atom_res : ty:string -> (string -> ('a, string) result) -> 'a t

(** Parse a string, either as a raw atom or a constant value. *)
val string : string t

(** Parse a float. *)
val float : float t

(** Parse an integer. *)
val int : int t

(** Parse all remaining items using this parser. *)
val many : 'a t -> 'a list t

(** Parse all remaining items using this parser.

    This requires there to be at least one item present - `some term` is equivalent to `term` then
    `many term`. *)
val some : 'a t -> 'a list t

(** Enter a list, and parse the body using this parser. *)
val in_list : 'a t -> 'a t

(** Parse a list of items. Equivalent to `in_list (many term)`. *)
val list : 'a t -> 'a list t

(** Parse any remaining values as fields. *)
val fields : 'a fields -> 'a t

(** {3 Field parsers} *)

(** Parse a required field. *)
val field : name:string -> 'a t -> 'a fields

(** Parse an optional field, with some default value. *)
val field_opt : name:string -> 'a t -> 'a option fields

(** Parse a field which may occur multiple times. *)
val field_repeated : name:string -> 'a t -> 'a list fields

(** {4 Running the parser} *)

type pos = Sexplib.Src_pos.Absolute.t =
  { row : int;
    col : int
  }

(** Parse this sexpr into a string. *)
val parse : Sexplib.Type_with_layout.Parsed.t_or_comment list -> 'a t -> ('a, pos * string) result

(** Read a single term from a buffer . *)
val parse_buf : Lexing.lexbuf -> 'a t -> ('a, pos * string) result
