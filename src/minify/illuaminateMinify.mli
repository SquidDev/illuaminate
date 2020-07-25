(** Provides utilities for minifying a program. *)
open IlluaminateCore

(** A customised emitter which makes minified output a little more readable. Currently this does
    nothing. *)
module Emit : sig
  type t

  val use : Format.formatter -> (t -> 'a) -> 'a

  include Emit.S with type t := t
end

(** Attempts to minify a program as much as possible. This removes any superfluous trivia, making
    the program as short as possible. *)
val minify : IlluaminateData.context -> Syntax.program -> Syntax.program
