module Map = struct
  let rec link f : (Omd.attributes, 'a) Omd.link -> (Omd.attributes, 'b) Omd.link =
   fun { label; destination; title } -> { label = inline f label; destination; title }

  and inline f : (Omd.attributes, 'a) Omd.inline -> (Omd.attributes, 'b) Omd.inline = function
    | Concat (a, xs) -> Concat (a, List.map (inline f) xs)
    | Text (a, x) -> Text (a, x)
    | Emph (a, xs) -> Emph (a, inline f xs)
    | Strong (a, xs) -> Strong (a, inline f xs)
    | Code (a, x) -> Code (a, x)
    | Hard_break a -> Hard_break a
    | Soft_break a -> Soft_break a
    | Link (a, l) -> Link (a, link f l)
    | Image (a, l) -> Image (a, link f l)
    | Html (a, x) -> Html (a, x)
    | Ref (k, r, inl) -> Ref (k, f r, inline f inl)
    | Colour c -> Colour c

  let rec block f : (Omd.attributes, 'a) Omd.block -> (Omd.attributes, 'b) Omd.block = function
    | Paragraph (attr, x) -> Paragraph (attr, inline f x)
    | List (attr, ty, sp, bl) -> List (attr, ty, sp, List.map (List.map (block f)) bl)
    | Blockquote (attr, xs) -> Blockquote (attr, List.map (block f) xs)
    | Thematic_break attr -> Thematic_break attr
    | Heading (attr, level, text) -> Heading (attr, level, inline f text)
    | Definition_list (attr, l) ->
        let f { Omd.term; defs } = { Omd.term = inline f term; defs = List.map (inline f) defs } in
        Definition_list (attr, List.map f l)
    | Code_block (attr, label, code) -> Code_block (attr, label, code)
    | Html_block (attr, x) -> Html_block (attr, x)

  let doc f = List.map (block f)
end

module Iter = struct
  let rec link f : (Omd.attributes, 'r) Omd.link -> unit =
   fun { label; destination = _; title = _ } -> inline f label

  and inline f : (Omd.attributes, 'r) Omd.inline -> unit = function
    | Concat (_, xs) -> List.iter (inline f) xs
    | Emph (_, xs) | Strong (_, xs) -> inline f xs
    | Hard_break _ | Soft_break _ | Html _ | Colour _ | Text _ | Code _ -> ()
    | Link (_, l) -> link f l
    | Image (_, l) -> link f l
    | Ref (_, r, inl) -> f r; inline f inl

  let rec block f : (Omd.attributes, 'a) Omd.block -> unit = function
    | Paragraph (_, x) -> inline f x
    | List (_, _, _, bl) -> List.iter (List.iter (block f)) bl
    | Blockquote (_, xs) -> List.iter (block f) xs
    | Thematic_break _ | Code_block _ | Html_block _ -> ()
    | Heading (_, _, text) -> inline f text
    | Definition_list (_, l) ->
        let f { Omd.term; defs } =
          inline f term;
          List.iter (inline f) defs
        in
        List.iter f l

  let doc f = List.iter (block f)

  let rec code_blocks f : (Omd.attributes, 'a) Omd.block -> unit = function
    | Paragraph _ | Thematic_break _ | Html_block _ | Heading _ | Definition_list _ -> ()
    | List (_, _, _, bl) -> List.iter (List.iter (code_blocks f)) bl
    | Blockquote (_, xs) -> List.iter (code_blocks f) xs
    | Code_block (attrs, lang, code) -> f attrs lang code
end
