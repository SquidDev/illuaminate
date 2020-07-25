(** Provides utilities for minifying a program. *)
open IlluaminateCore

(** A customised emitter which makes minified output a little more readable. Currently this does
    nothing. *)
module Emit : sig
  type t

  val use : Format.formatter -> (t -> 'a) -> 'a

  include Emit.S with type t := t
end

(** Remove any superfluous trivia nodes from a program. *)
val remove_trivia : Syntax.program -> Syntax.program

(** Rename any variables within the program to a shorter variant. *)
val rename : IlluaminateData.context -> Syntax.program -> Syntax.program

(** Attempts to minify a program as much as possible. This applies [rename] and [remove_trivia] to
    minify as far as possible. *)
val minify : IlluaminateData.context -> Syntax.program -> Syntax.program
