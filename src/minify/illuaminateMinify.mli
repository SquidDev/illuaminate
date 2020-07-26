(** Provides utilities for minifying a program. *)
open IlluaminateCore

(** A customised emitter which will wrap text rather than printing it verbatim. *)
module Emit : sig
  include Emit.S with type t := Format.formatter

  (** Set the margin of this formatter to 80, then print and flush a value. *)
  val with_wrapping : Format.formatter -> ('b, Format.formatter, unit) format -> 'b
end

(** Remove any superfluous trivia nodes from a program. *)
val remove_trivia : Syntax.program -> Syntax.program

(** Rename any variables within the program to a shorter variant. *)
val rename : IlluaminateData.context -> Syntax.program -> Syntax.program

(** Attempts to minify a program as much as possible. This applies [rename] and [remove_trivia] to
    minify as far as possible. *)
val minify : IlluaminateData.context -> Syntax.program -> Syntax.program
