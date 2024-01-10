open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:string-escape"

let expr () _ r = function
  | String node -> (
      let value = Node.contents.get node in
      if
        String.length value > 0
        && (value.[0] = '\'' || value.[0] = '\"')
        && String.contains value '\\'
      then
        let module S = Illuaminate.Syntax.Literal.String in
        let check : S.spanned_component -> unit = function
          | { contents = Unknown_escape chr; start; length } ->
              let span = Node.span node in
              let span =
                let open Illuaminate.Lens in
                span
                |> (Span.start_offset ^= (Span.start_offset.get span + start))
                   % (Span.finish_offset ^= (Span.start_offset.get span + start + length - 1))
              in
              r.r ~tag ~span "Unknown escape character '\\%c'." chr
          | _ -> ()
        in
        match Node.contents.get node |> S.parse with
        | Error () | Ok (Long_string _) -> ()
        | Ok (Short_string fragments) -> List.iter check fragments)
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
