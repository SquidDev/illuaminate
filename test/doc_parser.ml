open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
module D = Doc.Parser.Data
open Doc.Comment

let process ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = { Span.name; path = name } in
  match IlluaminateParser.parse name lexbuf with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out (fun _ -> Some contents) errs
  | Ok parsed -> (
      let context = { Data.root = Sys.getcwd () |> Fpath.v; config = Schema.(default empty) } in
      let data =
        Data.Files.create (Fun.const context) |> Data.Files.add parsed |> fst |> Data.of_files
      in
      let comments = Data.get parsed D.key data in
      D.comments comments
      |> List.sort (fun a b -> Span.compare a.source b.source)
      |> List.iter @@ fun comment ->
         Doc_sexp.Comment.to_sexp comment |> CCSexp.pp out;
         Format.pp_print_newline out ();
         match comment.errors with
         | [] -> ()
         | _ :: _ as errors ->
             List.iter (fun (tag, msg) -> Format.fprintf out "%s: %s" tag.Error.Tag.name msg) errors
      )

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  OmnomnomGolden.of_directory process ~group:"Comment parsing" ~directory:"data/doc-parse"
    ~extension:".lua" ()
