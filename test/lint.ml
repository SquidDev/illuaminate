open Omnomnom.Tests
open IlluaminateCore
open IlluaminateLint
open IlluaminateConfig
open IlluaminateSemantics
open Lens
module D = IlluaminateData

let only =
  let field =
    Term.field ~name:"only" ~comment:"Only use these linters" ~default:[]
      Term.Converter.(list string)
  in
  Category.add field Linter.category

let schema =
  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    (Schema.singleton only) Linters.all
  |> Schema.union (Schema.singleton Doc.Extract.Config.key)

let parse_schema : Node.trivial Span.spanned -> _ = function
  | { value = LineComment c | BlockComment (_, c); _ } ->
      c
      |> CCString.drop_while (fun c -> c == '-')
      |> String.trim
      |> CCString.chop_prefix ~pre:"config:"
      |> Option.map @@ fun c ->
         let buf = Lexing.from_string c in
         Schema.to_parser schema |> Parser.fields
         |> Parser.parse_buf (Span.Filename.mk "=config") buf
         |> Result.fold ~ok:Fun.id ~error:(fun (_, x) -> failwith x)
  | { value = Whitespace _; _ } -> None

let parse_schema program =
  program ^. (Syntax.First.program -| Node.leading_trivia)
  |> CCList.find_map parse_schema
  |> CCOpt.get_lazy @@ fun () ->
     program ^. (Syntax.Last.program -| Node.trailing_trivia)
     |> CCList.find_map parse_schema
     |> CCOpt.get_lazy @@ fun () -> Schema.default schema

let files ?out () =
  lazy
    (let module FileStore = D.Programs.FileStore in
    let dir = Fpath.(v (Sys.getcwd ()) / "data" / "lint" / "extra") in
    let files = FileStore.create () in
    let errs = Error.make () in
    Sys.readdir (Fpath.to_string dir)
    |> Array.iter (fun d ->
           let file = Fpath.(dir / d) in
           let name = Span.Filename.mk ~path:file d in
           let program =
             CCIO.with_in (Fpath.to_string file)
               (IlluaminateParser.program name % Lexing.from_channel)
           in
           Result.iter_error
             (fun (err : _ Span.spanned) -> IlluaminateParser.Error.report errs err.span err.value)
             program;
           Result.to_option program
           |> Option.map (fun x -> File.Lua x)
           |> FileStore.update files name);
    Error.display_of_files ?out ~with_summary:false errs;
    files)

let process ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = Span.Filename.mk name in
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out (fun _ -> Some contents) errs
  | Ok parsed ->
      let store = parse_schema parsed in
      let only = Schema.get only store in
      let only =
        List.map
          (fun x ->
            match Error.Tag.find x with
            | None -> failwith (Printf.sprintf "Unknown tag %S" x)
            | Some t -> t)
          only
      in
      let context = { D.Programs.Context.root = None; config = store } in
      let data =
        let open D.Builder in
        empty
        |> D.Programs.FileStore.lazy_builder (files ~out ())
        |> oracle D.Programs.Context.key (fun _ _ -> context)
        |> build
      in
      let linters =
        Linters.all
        |> List.filter (fun (Linter.Linter l) ->
               match only with
               | [] -> true
               | _ -> List.exists (fun x -> List.mem x l.tags) only)
      in
      let program, notes = Driver.lint_and_fix_all ~store ~data linters (Lua parsed) in
      let errs = Error.make () in
      Driver.Notes.to_seq notes |> Seq.iter (Driver.Note.report_any errs);
      Error.display_of_string ~out (fun _ -> Some contents) errs;
      let new_contents = Format.asprintf "%a" File.emit program in
      if contents <> new_contents then Helpers.diff out contents new_contents

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let leak =
  let open Leak in
  let name = Span.Filename.mk "=stdin" in
  let store = Schema.default schema in
  let context = { D.Programs.Context.root = None; config = store } in
  let data =
    let open D.Builder in
    empty
    |> D.Programs.FileStore.lazy_builder (files ())
    |> oracle D.Programs.Context.key (fun _ _ -> context)
    |> build
  in

  OmnomnomAlcotest.mk_alcotest_case "Memory leaks" `Slow @@ fun () ->
  Leak.run_unit ~n:1000
    (action ~name:"Parse file" (fun () ->
         let contents = "print('hello world')" in
         match Lexing.from_string contents |> IlluaminateParser.program name with
         | Error err ->
             let errs = Error.make () in
             IlluaminateParser.Error.report errs err.span err.value;
             Alcotest.failf "%t" (fun out ->
                 Error.display_of_string ~out (fun _ -> Some contents) errs)
         | Ok parsed -> parsed)
    >-> action ~name:"Lint and fix" (fun prog ->
            match Driver.lint_and_fix_all ~store ~data Linters.all (Lua prog) with
            | Lua x, _ -> x
            | _ -> assert false)
    >-> action ~name:"Lint fixed program" (fun prog ->
            List.iter (fun l -> Driver.lint ~store ~data l (Lua prog) |> ignore) Linters.all))

let tests =
  group "Linting"
    [ OmnomnomGolden.of_directory process ~group:"Basic lints" ~directory:"data/lint"
        ~extension:".lua" ();
      leak
    ]
