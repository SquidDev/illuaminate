open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make Error.Error "syntax:string-escape"

let expr () _ = function
  | String { lit_node; _ } ->
      let value = Node.contents.get lit_node in
      if
        String.length value > 0
        && (value.[0] = '\'' || value.[0] = '\"')
        && String.contains value '\\'
      then
        let open IlluaminateSemantics.Stringlib.Literal in
        let rec build out = function
          | [] -> out
          | Malformed (chr, span) :: xs ->
              build (note ~tag ~span "Unknown escape character '\\%c'." chr :: out) xs
          | _ :: xs -> build out xs
        in
        parse lit_node |> Option.get |> build []
      else []
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ()
