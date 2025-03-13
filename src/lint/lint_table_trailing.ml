open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

module Separator = struct
  type t =
    | Comma
    | Semicolon

  let token : t -> Token.t = function
    | Comma -> Comma
    | Semicolon -> Semicolon

  let show = function
    | Comma -> "comma"
    | Semicolon -> "semicolon"

  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      field ~name:"table-separator"
        ~comment:"Whether tables entries should be separated by a comma (',') or semicolon (';')."
        ~default:Comma
        (Converter.enum ~ty:"separator"
           [ ("comma", Comma); (",", Comma); ("semicolon", Semicolon); (";", Semicolon) ])
    in
    Category.add term category
end

let tag = Error.Tag.make ~attr:[ Default ] ~level:Note "format:table-trailing"

let rec over_last f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: over_last f xs

let add_sep insert =
  Fixer.fix @@ function
  | Table ({ table_body; _ } as t) ->
      let fix = function
        | i, Some x -> (i, Some (Node.contents.over (fun _ -> insert) x))
        | item, None ->
            let last = Last.table_item.get item in
            ( Illuaminate.Lens.(
                (Last.table_item -| Node.trailing_trivia) ^= Illuaminate.IArray.empty)
                item,
              Some
                { Node.contents = insert;
                  span = Span.finish last.span;
                  leading_trivia = Illuaminate.IArray.empty;
                  trailing_trivia = last.trailing_trivia
                } )
      in
      Ok (Table { t with table_body = over_last fix table_body })
  | _ -> Error "Expected a table"

let del_sep =
  Fixer.fix @@ function
  | Table ({ table_body; _ } as t) ->
      let fix = function
        | i, None -> (i, None)
        | i, Some { Node.leading_trivia; trailing_trivia; _ }
          when Illuaminate.IArray.is_empty leading_trivia
               && Illuaminate.IArray.is_empty trailing_trivia -> (i, None)
        | i, Some { leading_trivia; trailing_trivia; _ } ->
            ( Illuaminate.Lens.(Last.table_item -| Node.trailing_trivia).over
                (fun x ->
                  let ( @^ ) = Node.join_trivia in
                  x @^ leading_trivia @^ trailing_trivia)
                i,
              None )
      in
      Ok (Table { t with table_body = over_last fix table_body })
  | _ -> Error "Expected a table"

let expr sep _ r = function
  | Table { table_open; table_body; table_close } -> (
      let multiline = Span.start_line table_open.span <> Span.start_line table_close.span in
      match CCList.last_opt table_body with
      | None -> () (* If it's an empty table, allow both. *)
      | Some (item, None) when multiline ->
          Separator.token sep |> Token.show
          |> r.r
               ~fix:(add_sep (Separator.token sep))
               ~span:(Spanned.table_item item) ~tag "Expected trailing %S on multiline table"
      | Some (_, Some tok) when not multiline ->
          Node.contents.get tok |> Token.show
          |> r.r ~fix:del_sep ~span:(Node.span tok) ~tag
               "Unexpected trailing %S on single line table"
      | _ -> ())
  | _ -> ()

let linter = make ~options:Separator.options ~tags:[ tag ] ~expr ()
