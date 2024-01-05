open IlluaminateCore
open IlluaminateSemantics
open Doc.Comment
module Doc = Doc.Parser.Data
open Import

let display ~out ~contents comments =
  Doc.comments comments
  |> List.sort (fun a b -> Span.compare a.source b.source)
  |> List.iter @@ fun comment ->
     Doc_sexp.Comment.to_sexp comment |> CCSexp.pp out;
     Format.pp_print_newline out ();
     match comment.errors with
     | [] -> ()
     | _ :: _ as errors ->
         List.map Comment_error.to_error errors
         |> Illuaminate.Console_reporter.display_of_string ~out (fun _ -> Some contents)

let process_lua_worker ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = Illuaminate.File_id.mk name in
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
        (fun _ -> Some contents)
        [ IlluaminateParser.Error.to_error err ]
  | Ok parsed ->
      let data = make_data name (Lua parsed) in
      D.get data Doc.program name |> Option.get |> display ~out ~contents

let process_md_worker ~name input out =
  let lexbuf = Lexing.from_string input in
  let name = Illuaminate.File_id.mk name in
  let attributes, contents = IlluaminateParserMd.parse name lexbuf in
  let data = make_data name (Markdown { attributes; contents }) in
  D.get data Doc.file name |> Option.get |> display ~out ~contents:input

let process_lua ~name contents = Format.asprintf "%t" (process_lua_worker ~name contents)
let process_md ~name contents = Format.asprintf "%t" (process_md_worker ~name contents)

let tests =
  Omnomnom.Tests.group "Parsing"
    [ OmnomnomGolden.of_directory process_lua ~group:"Comment parsing" ~directory:"data/doc-parse"
        ~extension:".lua" ();
      OmnomnomGolden.of_directory process_md ~group:"Markdown parsing" ~directory:"data/doc-parse"
        ~extension:".md" ()
    ]
