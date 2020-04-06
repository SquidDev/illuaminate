open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:string-escape"

let expr () _ r = function
  | String { lit_node; _ } ->
      let value = Node.contents.get lit_node in
      if
        String.length value > 0
        && (value.[0] = '\'' || value.[0] = '\"')
        && String.contains value '\\'
      then
        let open IlluaminateSemantics.Stringlib.Literal in
        let build = function
          | Malformed (chr, span) -> r.r ~tag ~span "Unknown escape character '\\%c'." chr
          | _ -> ()
        in
        parse lit_node |> Option.get |> List.iter build
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
