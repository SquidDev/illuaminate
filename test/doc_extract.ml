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
      let files = D.Programs.Files.create () in
      D.Programs.Files.add parsed files |> ignore;
      let data =
        let open D.Builder in
        empty |> D.Programs.Files.builder files
        |> oracle D.Programs.Context.key (Fun.const context)
        |> build
      in
      let data = D.get data Doc.key parsed in
      Doc.errors data
      |> List.iter (fun (e : Error.Error.t) -> Error.report errs e.tag e.span e.message);
      Doc.get_module data
      |> Option.iter (fun m ->
             Doc_sexp.Syntax.(documented (Doc_sexp.one' % module_info) m) |> CCSexp.pp out) );

  Error.display_of_string ~out (fun _ -> Some contents) errs

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  OmnomnomGolden.of_directory process ~group:"Extraction" ~directory:"data/doc-extract"
    ~extension:".lua" ()
