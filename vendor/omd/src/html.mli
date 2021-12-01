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

val inline :
  ref:([ `Text | `Code ] -> 'ref -> t -> t) -> (attributes, 'ref) inline -> t

val of_doc :
     ?highlight:(attributes -> string -> string -> t)
  -> ref:([ `Text | `Code ] -> 'ref -> t -> t)
  -> (attributes, 'ref) block list
  -> t

val to_plain_text : (_, _) inline -> string

val to_string : t -> string
