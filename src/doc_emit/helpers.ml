open IlluaminateSemantics
open Doc.AbstractSyntax
open Doc.Syntax

(** Try to extract a summary from a markdown document. This will take the first sentence or line of
    the document, or at most [[max_length]] characters. *)
let get_summary ?(max_length = 120) (desc : _ Omd.doc) : _ Omd.inline =
  let open Omd in
  let rec go space : (attributes, 'r) inline -> int * (attributes, 'r) inline = function
    | _ when space <= 0 -> (0, Text ([], ""))
    | Concat (a, xs) ->
        let space, xs = go_list space [] xs in
        (space, Concat (a, xs))
    | Text (a, t) -> (
        let sentence_end =
          [ '.'; '!'; '?' ]
          |> List.map (String.index_opt t)
          |> List.fold_left
               (fun exist idx ->
                 match (exist, idx) with
                 | Some exist, Some idx when idx < exist -> Some idx
                 | None, idx -> idx
                 | exist, _ -> exist)
               None
        in
        match sentence_end with
        | Some i -> (0, Text (a, CCString.take (i + 1) t))
        | None when String.length t >= space -> (0, Text (a, CCString.take space t ^ "..."))
        | None -> (space - String.length t, Text (a, t)))
    (* Basic formatting blocks *)
    | Emph (a, body) -> on_child (fun x -> Emph (a, x)) space body
    | Strong (a, body) -> on_child (fun x -> Strong (a, x)) space body
    | Link (a, ({ label; _ } as link)) ->
        on_child (fun label -> Link (a, { link with label })) space label
    | Ref_raw (_, text) as r -> (space - String.length text, r)
    | Ref_desc (ref, text) -> on_child (fun text -> Ref_desc (ref, text)) space text
    | Code (_, body) as c -> (space - String.length body, c)
    | Colour c -> (space - String.length c, Colour c)
    | Soft_break _ | Hard_break _ -> (space - 1, Text ([], " "))
    | Image _ | Html _ -> (space, Text ([], ""))
  and on_child factory space node =
    let space, node = go space node in
    (space, factory node)
  and go_list space ys = function
    | [] -> (space, List.rev ys)
    | _ when space <= 0 -> (space, List.rev ys)
    | x :: xs ->
        let space, x = go space x in
        go_list space (x :: ys) xs
  in
  let rec block : _ Omd.block list -> _ = function
    | (Paragraph (_, x) | Heading (_, _, x)) :: _ -> go max_length x |> snd
    | Html_block (_, html) :: description when String.starts_with ~prefix:"<!--" html ->
        block description
    | _ -> Text ([], "")
  in
  block desc

(** Get a link to a node. *)
let link ~source_link { definition; custom_source; _ } =
  let link =
    match custom_source with
    | None -> Span definition
    | Some pos -> Position pos
  in
  source_link link

let reference_link { Namespace.Ref.namespace = Namespace ns; id; _ } ref =
  let section = Reference.section_of_name ref in
  match section with
  | None -> Format.asprintf "%s/%s.html" ns id
  | Some sec -> Format.asprintf "%s/%s.html#%s" ns id sec
