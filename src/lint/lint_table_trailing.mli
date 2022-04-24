(** Checks for trailing commas/semicolons on tables. *)

(** The separator to use between table fields. *)
module Separator : sig
  type t =
    | Comma
    | Semicolon

  val token : t -> IlluaminateCore.Token.t
  val show : t -> string
  val options : t IlluaminateConfig.Category.key
end

include Linter.S
