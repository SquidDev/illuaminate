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

type table_alignment =
  | Default
  | Left
  | Right
  | Center

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
  | Ref_raw of 'ref * string
  | Ref_desc of 'ref * ('attr, 'ref) inline
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
  | Table of
      'attr
      * table_alignment list
      * ('attr, 'ref) inline list
      * ('attr, 'ref) inline list list

type 'ref doc = (attributes, 'ref) block list
(** A markdown document *)

val of_channel : in_channel -> string doc
val of_string : string -> string doc

val inline_to_html :
     ref:('ref -> [ `Raw of string | `Desc of string ] -> string)
  -> (attributes, 'ref) inline
  -> string

val to_html :
     ?highlight:(attributes -> string -> string -> string)
  -> ref:('ref -> [ `Raw of string | `Desc of string ] -> string)
  -> 'ref doc
  -> string

val to_sexp : ref:('ref -> string) -> 'ref doc -> string
val to_plain_text : (_, _) inline -> string

val headers :
     ?remove_links:bool
  -> ('attr list, 'ref) block list
  -> ('attr list * int * ('attr list, 'ref) inline) list

val toc : ?start:int list -> ?depth:int -> 'ref doc -> 'ref doc
