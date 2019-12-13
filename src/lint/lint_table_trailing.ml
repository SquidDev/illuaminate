open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

module Separator = struct
  type t =
    | Comma
    | Semicolon

  let read = function
    | "comma" | "," -> Ok Comma
    | "semicolon" | ";" -> Ok Semicolon
    | e -> Error (Printf.sprintf "Unknown separator %S (expected 'comma' or 'semicolon')." e)

  let show = function
    | Comma -> "comma"
    | Semicolon -> "semicolon"

  let token = function
    | Comma -> Token.Comma
    | Semicolon -> Token.Semicolon

  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      field ~name:"table-separator"
        ~comment:"Whether tables entries should be separated by a comma (',') or semicolon (';')."
        ~default:Comma
        (Converter.atom ~ty:"separator" read show)
    in
    Category.add term category
end

let linter =
  let tag = Error.Tag.make Error.Note "syntax:table-trailing" in
  let rec over_last f = function
    | [] -> []
    | [ x ] -> [ f x ]
    | x :: xs -> x :: over_last f xs
  in
  let add_sep insert =
    FixOne
      (function
      | Table ({ table_body; _ } as t) ->
          let fix = function
            | i, Some x -> (i, Some (Node.contents.over (fun _ -> insert) x))
            | i, None -> (
                let last = Last.table_item.get i in
                match last with
                | Node.SimpleNode _ -> (i, Some (Node.SimpleNode { contents = insert }))
                | Node.Node { trailing_trivia; span; _ } ->
                    ( Lens.(Last.table_item -| Node.trailing_trivia).over (fun _ -> []) i,
                      Some
                        (Node.Node
                           { contents = insert;
                             span = Span.finish span;
                             leading_trivia = [];
                             trailing_trivia
                           }) ) )
          in
          Ok (Table { t with table_body = over_last fix table_body })
      | _ -> Error "Expected a table")
  in
  let del_sep =
    FixOne
      (function
      | Table ({ table_body; _ } as t) ->
          let fix = function
            | i, (None | Some (Node.SimpleNode _)) -> (i, None)
            | i, Some (Node.Node { leading_trivia = []; trailing_trivia = []; _ }) -> (i, None)
            | i, Some (Node.Node { leading_trivia; trailing_trivia; _ }) ->
                ( Lens.(Last.table_item -| Node.trailing_trivia).over
                    (fun x ->
                      let ( @^ ) = Node.join_trivia in
                      x @^ leading_trivia @^ trailing_trivia)
                    i,
                  None )
          in
          Ok (Table { t with table_body = over_last fix table_body })
      | _ -> Error "Expected a table")
  in
  let expr sep _ = function
    | Table
        { table_open = Node { span = start; _ }; table_body; table_close = Node { span = fin; _ } }
      -> (
        let multiline = start.start_line <> fin.start_line in
        match CCList.last_opt table_body with
        | None -> [] (* If it's an empty table, allow both. *)
        | Some (item, None) when multiline ->
            [ Separator.token sep |> Token.show
              |> Printf.sprintf "Expected trailing %S on multiline table"
              |> note ~fix:(add_sep (Separator.token sep)) ~span:(Spanned.table_item item) ~tag
            ]
        | Some (_, Some tok) when not multiline ->
            [ Node.contents.get tok |> Token.show
              |> Printf.sprintf "Unexpected trailing %S on single line table"
              |> note ~fix:del_sep ~span:(Node.span tok) ~tag
            ]
        | _ -> [] )
    | _ -> []
  in
  make ~options:Separator.options ~tags:[ tag ] ~expr ()
