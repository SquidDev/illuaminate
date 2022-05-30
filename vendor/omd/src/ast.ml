type attributes = (string * string) list

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

type 'attr link_def =
  { label : string
  ; destination : string
  ; title : string option
  ; attributes : 'attr
  }

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

module type T = sig
  type ('attr, 'ref) t
end

module MakeBlock (I : T) = struct
  type ('attr, 'ref) def_elt =
    { term : ('attr, 'ref) I.t
    ; defs : ('attr, 'ref) I.t list
    }

  (* A value of type 'attr is present in all variants of this type. We use it to associate extra
     information to each node in the AST. In the common case, the attributes type defined above is
     used. We might eventually have an alternative function to parse blocks while keeping concrete
     information such as source location and we'll use it for that as well. *)
  type ('attr, 'ref) block =
    | Paragraph of 'attr * ('attr, 'ref) I.t
    | List of 'attr * list_type * list_spacing * ('attr, 'ref) block list list
    | Blockquote of 'attr * ('attr, 'ref) block list
    | Thematic_break of 'attr
    | Heading of 'attr * int * ('attr, 'ref) I.t
    | Code_block of 'attr * string * string
    | Html_block of 'attr * string
    | Definition_list of 'attr * ('attr, 'ref) def_elt list
    | Admonition of
        'attr * admonition * ('attr, 'ref) I.t * ('attr, 'ref) block list
    | Table of
        'attr
        * table_alignment list
        * ('attr, 'ref) I.t list
        * ('attr, 'ref) I.t list list
end

type ('attr, 'ref) link =
  { label : ('attr, 'ref) inline
  ; destination : string
  ; title : string option
  }

(* See comment on the block type above about the 'attr parameter *)
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

module StringT = struct
  type ('attr, 'ref) t = string
end

module InlineT = struct
  type ('attr, 'ref) t = ('attr, 'ref) inline
end

module Raw = MakeBlock (StringT)
module Inline = MakeBlock (InlineT)
include Inline

module MakeMapper (Src : T) (Dst : T) = struct
  module SrcBlock = MakeBlock (Src)
  module DstBlock = MakeBlock (Dst)

  let rec map (f : ('attr, 'ref) Src.t -> ('attr, 'ref) Dst.t) :
      ('attr, 'ref) SrcBlock.block -> ('attr, 'ref) DstBlock.block = function
    | SrcBlock.Paragraph (attr, x) -> DstBlock.Paragraph (attr, f x)
    | List (attr, ty, sp, bl) ->
        List (attr, ty, sp, List.map (List.map (map f)) bl)
    | Blockquote (attr, xs) -> Blockquote (attr, List.map (map f) xs)
    | Thematic_break attr -> Thematic_break attr
    | Heading (attr, level, text) -> Heading (attr, level, f text)
    | Definition_list (attr, l) ->
        let f { SrcBlock.term; defs } =
          { DstBlock.term = f term; defs = List.map f defs }
        in
        Definition_list (attr, List.map f l)
    | Code_block (attr, label, code) -> Code_block (attr, label, code)
    | Html_block (attr, x) -> Html_block (attr, x)
    | Admonition (attr, kind, title, body) ->
        Admonition (attr, kind, f title, List.map (map f) body)
    | Table (attr, aligns, hs, rows) ->
        Table (attr, aligns, List.map f hs, List.map (List.map f) rows)
end

module Mapper = MakeMapper (StringT) (InlineT)

let same_block_list_kind k1 k2 =
  match (k1, k2) with
  | Ordered (_, c1), Ordered (_, c2) | Bullet c1, Bullet c2 -> c1 = c2
  | _ -> false

let string_of_admonition : admonition -> string = function
  | Note -> "note"
  | Info -> "info"
  | Tip -> "tip"
  | Caution -> "caution"
  | Warning -> "warning"
