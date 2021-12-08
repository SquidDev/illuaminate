open Ast

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

let rec link ~ref { label; destination; title; _ } =
  let title =
    match title with
    | Some title -> [ Atom title ]
    | None -> []
  in
  List (Atom "link" :: inline ~ref label :: Atom destination :: title)

and inline ~ref = function
  | Concat (_, xs) -> List (Atom "concat" :: List.map (inline ~ref) xs)
  | Text (_, s) -> Atom s
  | Emph (_, il) -> List [ Atom "emph"; inline ~ref il ]
  | Strong (_, il) -> List [ Atom "strong"; inline ~ref il ]
  | Code _ -> Atom "code"
  | Hard_break _ -> Atom "hard-break"
  | Soft_break _ -> Atom "soft-break"
  | Link (_, def) -> List [ Atom "url"; link ~ref def ]
  | Html (_, s) -> List [ Atom "html"; Atom s ]
  | Image _ -> Atom "img"
  | Colour c -> List [ Atom "colour"; Atom c ]
  | Ref_raw (r, label) -> List [ Atom "ref[raw]"; Atom (ref r); atom label ]
  | Ref_desc (r, label) ->
      List [ Atom "ref[desc"; Atom (ref r); inline ~ref label ]

let rec block ~ref = function
  | Paragraph (_, x) -> List [ Atom "paragraph"; inline ~ref x ]
  | List (_, _, _, bls) ->
      List
        (Atom "list"
        :: List.map
             (fun xs -> List (Atom "list-item" :: List.map (block ~ref) xs))
             bls)
  | Blockquote (_, xs) -> List (Atom "blockquote" :: List.map (block ~ref) xs)
  | Thematic_break _ -> Atom "thematic-break"
  | Heading (_, level, text) ->
      List [ Atom "heading"; Atom (string_of_int level); inline ~ref text ]
  | Code_block (_, info, _) -> List [ Atom "code-block"; Atom info ]
  | Html_block (_, s) -> List [ Atom "html"; Atom s ]
  | Definition_list (_, l) ->
      List
        [ Atom "def-list"
        ; List
            (List.map
               (fun elt ->
                 List
                   [ inline ~ref elt.term
                   ; List (List.map (inline ~ref) elt.defs)
                   ])
               l)
        ]
  | Admonition (_, kind, title, body) ->
      List
        (Atom "admonition"
        :: Atom (string_of_admonition kind)
        :: inline ~ref title
        :: List.map (block ~ref) body)

let create ~ref ast = List (List.map (block ~ref) ast)

let needs_quotes s =
  let rec loop i =
    if i >= String.length s then
      false
    else
      match s.[i] with
      | ' '
      | '\t'
      | '\x00' .. '\x1F'
      | '\x7F' .. '\x9F' ->
          true
      | _ -> loop (succ i)
  in
  loop 0

let rec print ppf = function
  | Atom s when needs_quotes s -> Format.fprintf ppf "%S" s
  | Atom s -> Format.pp_print_string ppf s
  | List l ->
      Format.fprintf
        ppf
        "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
        l
