open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
open Doc.Comment
module Doc = Doc.Parser.Data
module D = IlluaminateData

let process ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = Span.Filename.mk name in
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out (fun _ -> Some contents) errs
  | Ok parsed -> (
      let context = { D.Programs.Context.root = None; config = Schema.(default empty) } in
      let data =
        let open D.Builder in
        empty
        |> D.Programs.FileStore.(create () |> builder)
        |> oracle D.Programs.Context.key (fun _ _ -> context)
        |> build
      in
      let comments = D.get data Doc.program parsed in
      Doc.comments comments
      |> List.sort (fun a b -> Span.compare a.source b.source)
      |> List.iter @@ fun comment ->
         Doc_sexp.Comment.to_sexp comment |> CCSexp.pp out;
         Format.pp_print_newline out ();
         match comment.errors with
         | [] -> ()
         | _ :: _ as errors ->
             let errs = Error.make () in
             List.iter (fun (tag, f, msg) -> Error.report errs tag f msg) errors;
             Error.display_of_string ~out (fun _ -> Some contents) errs)

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  OmnomnomGolden.of_directory process ~group:"Comment parsing" ~directory:"data/doc-parse"
    ~extension:".lua" ()
