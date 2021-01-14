open IlluaminateSemantics
open Doc.AbstractSyntax
open Doc.Syntax

(** Try to extract a summary from a markdown document. This will take the first sentence or line of
    the document, or at most [\[max_length\]] characters. *)
let get_summary ?(max_length = 120) (desc : Omd.t) =
  let open Omd_representation in
  let rec go space = function
    | [] -> Ok []
    | _ when space <= 0 -> Ok []
    | Text t :: xs -> (
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
        | Some i -> Ok [ Text (CCString.take (i + 1) t) ]
        | None when String.length t >= space -> Ok [ Text (CCString.take space t ^ "...") ]
        | None -> appending (Text t) (space - String.length t) xs )
    (* Basic formatting blocks *)
    | Emph body :: xs -> on_child (fun x -> Emph x) space body xs
    | Bold body :: xs -> on_child (fun x -> Bold x) space body xs
    | Url (href, body, title) :: xs -> on_child (fun body -> Url (href, body, title)) space body xs
    | (Ref (_, _, text, _) as x) :: xs -> appending x (space - String.length text) xs
    | NL :: xs -> appending NL (space - 1) xs
    | (Code (_, body) as c) :: xs -> appending c (space - String.length body) xs
    (* Paragraphs are a forced break *)
    | Paragraph body :: _ -> Ok [ Paragraph (Result.value (go space body) ~default:body) ]
    | (Html _ as node) :: xs -> appending node space xs
    (* Just abort here *)
    | _ -> Ok []
  and appending node space xs = Result.map (fun xs -> node :: xs) (go space xs)
  and on_child factory space body xs =
    match go space body with
    | Ok x -> Ok [ Emph x ]
    | Error space -> Result.map (fun x -> factory x :: x) (go space xs)
  in
  match go max_length desc with
  | Ok x -> x
  | _ -> desc

(** Get a link to a node. *)
let link ~source_link { definition; custom_source; _ } =
  let link =
    match custom_source with
    | None -> Span definition
    | Some pos -> Position pos
  in
  source_link link

let reference_link (Namespace.Namespace ns, mod_name) ref =
  let section = Reference.section_of_name ref in
  match section with
  | None -> Format.asprintf "%s/%s.html" ns mod_name
  | Some sec -> Format.asprintf "%s/%s.html#%s" ns mod_name sec
