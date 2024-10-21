open IlluaminateCore
open IlluaminateSemantics
open IlluaminateConfig
module Doc = Doc.Extract
module D = IlluaminateData
module NMap = Map.Make (Namespace)

let root = Fpath.(v (Sys.getcwd ()) / "data" / "doc-emit")
let temp = lazy (Filename.temp_dir "illuaminate" "doc-emit" |> Fpath.v)

let process ~parse ~go ~name contents =
  Format.asprintf "%t" @@ fun out ->
  let report errs =
    Illuaminate.Console_reporter.display_of_string ~out (fun _ -> Some contents) errs
  in
  let lexbuf = Lexing.from_string contents in
  let name = Illuaminate.File_id.mk ~path:Fpath.(root / name) name in
  match parse name lexbuf with
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
        build @@ fun b ->
        D.Programs.FileStore.builder files b;
        IlluaminateDocEmit.Html.Assets.add_find_asset (Lazy.force temp) b;
        oracle D.Programs.Context.key (fun _ _ -> context) b
      in
      let docs = D.get data Doc.file name |> Option.get in
      Doc.errors docs |> List.map Doc.Extract_error.to_error |> report;
      Doc.get_page docs |> Option.iter (fun m -> go out data m)
  | Error err -> report [ err ]

let process_lua ~go ~name contents =
  let parse name lexbuf =
    match IlluaminateParser.program name lexbuf with
    | Error err -> Error (IlluaminateParser.Error.to_error err)
    | Ok parsed -> Ok (IlluaminateCore.File.Lua parsed)
  in
  process ~parse ~go ~name contents

let process_md ~go ~name contents =
  let parse name lexbuf =
    let attributes, contents = IlluaminateParserMd.parse name lexbuf in
    Ok (IlluaminateCore.File.Markdown { attributes; contents })
  in
  process ~parse ~go ~name contents

let tests ~extension ~group go =
  Omnomnom.Tests.group group
    [ OmnomnomGolden.of_directory (process_lua ~go) ~group:"Lua" ~directory:"data/doc-emit"
        ~rename:(fun x -> Printf.sprintf "%s.%s" x extension)
        ~extension:".lua" ();
      OmnomnomGolden.of_directory (process_md ~go) ~group:"Markdown" ~directory:"data/doc-emit"
        ~rename:(fun x -> Printf.sprintf "%s.%s" x extension)
        ~extension:".md" ()
    ]

let source_link : IlluaminateSemantics.Doc.AbstractSyntax.source -> string option = function
  | Span x -> Span.show x |> Option.some
  | Position { path; start_line; _ } -> Printf.sprintf "%s#L%d" path start_line |> Option.some

module Json_summary = struct
  let tests =
    tests ~extension:"json" ~group:"JSON summary" @@ fun out _ m ->
    IlluaminateDocEmit.Summary.(everything ~source_link [ m ] |> to_json)
    |> Yojson.Safe.pretty_print out
end

module Html_module = struct
  let tests =
    tests ~extension:"html" ~group:"HTML page" @@ fun out data m ->
    let time = Unix.time () |> Unix.gmtime in
    let date =
      Printf.sprintf "%04d-%02d-%02d" (time.tm_year + 1900) (time.tm_mon + 1) time.tm_mday
    in
    Format.fprintf out "<!DOCTYPE html>@\n";
    let module H = IlluaminateDocEmit.Html in
    let options =
      H.Options.make ~site_title:"My title" ~site_css:"main.css" ~site_js:"main.js" ~resolve:Fun.id
        ~data ~source_link ()
    in
    H.emit_page ~options ~pages:NMap.empty m
    |> Format.asprintf "%a" Illuaminate.Html.emit_pretty
    |> (fun x -> CCString.replace ~sub:date ~by:"xxxx-xx-xx" x)
    |> Format.pp_print_string out
end

module Dump_sexp = struct
  let tests =
    tests ~extension:"sexp" ~group:"Dump" @@ fun out _ m ->
    let open Illuaminate.Lens in
    Doc_sexp.Syntax.(documented (Doc_sexp.one' % module_info) m) |> CCSexp.pp out
end
