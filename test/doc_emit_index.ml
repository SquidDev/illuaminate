open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
module Doc = Doc.Extract
module D = IlluaminateData
open CCFun

let process ~name contents out =
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
      Doc.get_module data
      |> Option.iter (fun m ->
             IlluaminateDocEmit.Flat_index.(
               of_modules ~source_link:(Option.some % Span.show) [ m ] |> to_json)
             |> Yojson.Safe.pretty_print out) );

  Error.display_of_string ~out (fun _ -> Some contents) errs

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  OmnomnomGolden.of_directory process ~group:"Emit index" ~directory:"data/doc-extract"
    ~rename:(fun x -> x ^ ".json")
    ~extension:".lua" ()
