open Ast

type element_type =
  | Inline
  | Block

type t =
  | Element of element_type * string * attributes * t option
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

val of_doc : attributes block list -> t

val to_string : t -> string
