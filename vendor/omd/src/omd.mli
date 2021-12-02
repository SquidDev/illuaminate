(** A markdown parser in OCaml. *)

type attributes = (string * string) list

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

type admonition =
  | Note
  | Info
  | Tip
  | Caution
  | Warning

type ('attr, 'ref) link =
  { label : ('attr, 'ref) inline
  ; destination : string
  ; title : string option
  }

and ('attr, 'ref) inline =
  | Concat of 'attr * ('attr, 'ref) inline list
  | Text of 'attr * string
  | Emph of 'attr * ('attr, 'ref) inline
  | Strong of 'attr * ('attr, 'ref) inline
  | Code of 'attr * string
  | Hard_break of 'attr
  | Soft_break of 'attr
  | Link of 'attr * ('attr, 'ref) link
  | Image of 'attr * ('attr, 'ref) link
  | Html of 'attr * string
  | Ref of [ `Text | `Code ] * 'ref * ('attr, 'ref) inline
  | Colour of string

type ('attr, 'ref) def_elt =
  { term : ('attr, 'ref) inline
  ; defs : ('attr, 'ref) inline list
  }

type ('attr, 'ref) block =
  | Paragraph of 'attr * ('attr, 'ref) inline
  | List of 'attr * list_type * list_spacing * ('attr, 'ref) block list list
  | Blockquote of 'attr * ('attr, 'ref) block list
  | Thematic_break of 'attr
  | Heading of 'attr * int * ('attr, 'ref) inline
  | Code_block of 'attr * string * string
  | Html_block of 'attr * string
  | Definition_list of 'attr * ('attr, 'ref) def_elt list
  | Admonition of
      'attr * admonition * ('attr, 'ref) inline * ('attr, 'ref) block list

type 'ref doc = (attributes, 'ref) block list
(** A markdown document *)

val of_channel : in_channel -> string doc

val of_string : string -> string doc

val inline_to_html :
     ref:([ `Text | `Code ] -> 'ref -> string -> string)
  -> (attributes, 'ref) inline
  -> string

val to_html :
     ?highlight:(attributes -> string -> string -> string)
  -> ref:([ `Text | `Code ] -> 'ref -> string -> string)
  -> 'ref doc
  -> string

val to_sexp : ref:('ref -> string) -> 'ref doc -> string

val to_plain_text : (_, _) inline -> string

val headers :
     ?remove_links:bool
  -> ('attr, 'ref) block list
  -> ('attr * int * ('attr, 'ref) inline) list

val toc : ?start:int list -> ?depth:int -> 'ref doc -> 'ref doc
