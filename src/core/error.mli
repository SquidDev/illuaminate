(** An error reporting level *)
type level =
  | Critical  (** An error which cannot be recovered from *)
  | Error  (** A problem, which may cause problems during processing or runtime *)
  | Warning  (** A potential problem or bug. *)
  | Note  (** A minor issue. Effectively just a less major warning. *)

module Tag : sig
  (** A marker token for errors, used for filtering and classification *)
  type t

  (** Display a tag in a formatter. *)
  val pp : Format.formatter -> t -> unit

  (** Create a new error tag, and register it internally *)
  val make : level -> string -> t

  (** Find a tag with a given name *)
  val find : string -> t option

  (** Compare two tags. *)
  val compare : t -> t -> int

  (** A predicate which determines if a tag should be reported or not. *)
  type filter = t -> bool
end

(** An error reporting writer, into which errors are placed *)
type t

(** Make a new error reporter *)
val make : unit -> t

(** Report a new error at a specific position *)
val report : t -> Tag.t -> Span.t -> string -> unit

(** Determine if the error reporter has any problems in it. *)
val has_problems : t -> bool

(** Display any errors which occurred. *)
val display_of_channel : ?out:Format.formatter -> t -> unit

(** Display any errors which occurred, with a function which maps file names to strings *)
val display_of_string : ?out:Format.formatter -> (Span.filename -> string option) -> t -> unit

(** Provides a mechanism for formatting strings displayed by {!display_of_string} or
    {!display_of_channel}. *)
module Style : sig
  (** One of the 8 basic colors supported by ANSI escape sequences. *)
  type ansi_color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  (** A custom style to display text.

      While styles are based on those supported by ANSI escape sequences, they may be converted to
      other formats (such as HTML tags, etc...).

      This is a {!Format.stag}, and so can either be manually pushed and popped, or one can use the
      {!printf} helper function. *)
  type t =
    | Unstyled  (** Remove any styling from this string. *)
    | DullColor of ansi_color
        (** Tint the styled string's foreground with a "dull" color.

            When unit ANSI, this will be in the 30-37 range without the bold flag. *)
    | BrightColor of ansi_color
        (** Tint the styled string's foreground with a "bright" color.

            When unit ANSI, this will be in the 90-97 range with the bold flag. *)
    | Underlined  (** Underline this string. *)
    | Styled of t list  (** Apply multiple styles. *)

  type Format.stag += Style of t

  (** Print some formatting string with the given style applied.

      One must be careful to not partially apply this function, otherwise the formatting tag may be
      pushed once, but popped multiple times. *)
  val printf : t -> Format.formatter -> ('a, Format.formatter, unit) format -> 'a

  (** Set up a formatter to print ANSI escape codes. *)
  val setup_ansi : Format.formatter -> unit
end
