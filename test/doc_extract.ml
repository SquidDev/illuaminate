open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
module D = Doc.Extract
open CCFun

let process ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = { Span.name; path = name } in
  let errs = Error.make () in
  ( match IlluaminateParser.program name lexbuf with
  | Error err -> IlluaminateParser.Error.report errs err.span err.value
  | Ok parsed ->
      let context =
        { Data.root = Sys.getcwd () |> Fpath.v;
          config = Schema.(singleton D.Config.key |> default)
        }
      in
      let data =
        Data.Files.create (Fun.const context) |> Data.Files.add parsed |> fst |> Data.of_files
      in
      let data = Data.get parsed D.key data in
      D.errors data
      |> List.iter (fun (e : Error.Error.t) -> Error.report errs e.tag e.span e.message);
      D.get_module data
      |> Option.iter (fun m ->
             Doc_sexp.Syntax.(documented (Doc_sexp.one' % module_info) m) |> CCSexp.pp out) );

  Error.display_of_string ~out (fun _ -> Some contents) errs

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  OmnomnomGolden.of_directory process ~group:"Extraction" ~directory:"data/doc-extract"
    ~extension:".lua" ()
