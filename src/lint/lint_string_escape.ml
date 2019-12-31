open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.Tag.make Error.Error "syntax:string-escape" in

  let expr () _ = function
    | String { lit_node; _ } ->
        let value = Node.contents.get lit_node and span = Node.span lit_node in
        if String.length value > 0 && (value.[0] = '\'' || value.[0] = '\"') then
          let lexbuf = Lexing.from_string value in
          match Lex_string_escape.string_of lexbuf with
          | None -> Printf.printf "Oh no"; []
          | Some es ->
              let msg (chr, start, finish) =
                let span =
                  { span with
                    start_col = span.start_col + start.Lexing.pos_cnum;
                    finish_col = span.start_col + finish.Lexing.pos_cnum - 1
                  }
                in
                note ~tag ~span (Printf.sprintf "Unknown escape character '\\%c'." chr)
              in
              List.map msg es
        else []
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ()
