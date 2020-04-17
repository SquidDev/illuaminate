open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
module Doc = Doc.Extract
module D = IlluaminateData

let process ~go ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = Span.Filename.mk name in
  let errs = Error.make () in
  ( match IlluaminateParser.program name lexbuf with
  | Error err -> IlluaminateParser.Error.report errs err.span err.value
  | Ok parsed ->
      let context =
        { D.Programs.Context.root = Sys.getcwd () |> Fpath.v |> Option.some;
          config = Schema.(singleton Doc.Config.key |> default)
        }
      in
      let files = D.Programs.FileStore.create () in
      D.Programs.FileStore.update files name (Some parsed);
      let data =
        let open D.Builder in
        empty
        |> D.Programs.FileStore.builder files
        |> oracle D.Programs.Context.key (fun _ _ -> context)
        |> build
      in
      let data = D.get data Doc.key parsed in
      Doc.errors data
      |> List.iter (fun (e : Error.Error.t) -> Error.report errs e.tag e.span e.message);
      Doc.get_module data |> Option.iter (fun m -> go out m) );

  Error.display_of_string ~out (fun _ -> Some contents) errs

let process ~go ~name contents = Format.asprintf "%t" (process ~go ~name contents)

let tests ~extension ~group go =
  OmnomnomGolden.of_directory (process ~go) ~group ~directory:"data/doc-extract"
    ~rename:(fun x -> Printf.sprintf "%s.%s" x extension)
    ~extension:".lua" ()

let source_link x = Span.show x |> Option.some

module Json_summary = struct
  let tests =
    tests ~extension:"json" ~group:"Json summary" @@ fun out m ->
    IlluaminateDocEmit.Summary.(everything ~source_link [ m ] |> to_json)
    |> Yojson.Safe.pretty_print out
end

module Html_module = struct
  let tests =
    tests ~extension:"html" ~group:"Html page" @@ fun out m ->
    Format.fprintf out "<!DOCTYPE html>@\n";
    IlluaminateDocEmit.Html_main.emit_module ~site_title:"My title" ~resolve:Fun.id ~source_link
      ~modules:[] m
    |> Html.Default.emit_pretty out
end