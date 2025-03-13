open IlluaminateCore
open IlluaminateLint
open IlluaminateConfig
open IlluaminateSemantics
open Illuaminate.Lens
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

let parse_schema : Node.Trivia.t -> _ = function
  | { kind = LineComment | BlockComment; contents; _ } ->
      contents
      |> CCString.drop_while (fun c -> c == '-' || c == '[')
      |> String.trim
      |> CCString.chop_prefix ~pre:"config:"
      |> Option.map @@ fun c ->
         let buf = Lexing.from_string c in
         Schema.to_parser schema |> Parser.fields
         |> Parser.parse_buf (Illuaminate.File_id.mk "=config") buf
         |> Result.fold ~ok:Fun.id ~error:(fun (_, x) -> failwith x)
  | { kind = Whitespace; _ } -> None

let parse_schema program =
  Syntax.First.program program ^. Node.leading_trivia
  |> Illuaminate.IArray.find_map parse_schema
  |> CCOption.or_lazy ~else_:(fun () ->
         Syntax.Last.program program ^. Node.leading_trivia
         |> Illuaminate.IArray.find_map parse_schema)
  |> CCOption.get_lazy @@ fun () -> Schema.default schema

let files ~report extra =
  lazy
    (let module FileStore = D.Programs.FileStore in
    let files = FileStore.create () in
    let dir = Fpath.(v (Sys.getcwd ()) / "extra") in
    Sys.readdir (Fpath.to_string dir)
    |> Array.iter (fun d ->
           let file = Fpath.(dir / d) in
           let name = Illuaminate.File_id.mk ~path:file d in
           let program =
             CCIO.with_in (Fpath.to_string file)
               (IlluaminateParser.program name % Lexing.from_channel)
           in
           Result.iter_error (fun err -> report (IlluaminateParser.Error.to_error err)) program;
           Result.to_option program
           |> Option.map (fun x -> File.Lua x)
           |> FileStore.update files name);
    List.iter (fun (name, program) -> FileStore.update files name (Some program)) extra;
    files)

let process ?(name = "input.lua") contents =
  let lexbuf = Lexing.from_string contents in
  let name = Illuaminate.File_id.mk name in
  let errors = ref [] in
  let report x = errors := x :: !errors in
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      report (IlluaminateParser.Error.to_error err);
      (!errors, None)
  | Ok parsed ->
      let store = parse_schema parsed in
      let linters =
        match Schema.get only store with
        | [ ":all" ] -> Linters.all
        | tags -> (
            let tags =
              List.map
                (fun x ->
                  match Error.Tag.find x with
                  | None -> failwith (Printf.sprintf "Unknown tag %S" x)
                  | Some t -> t)
                tags
            in
            Linters.all
            |> List.filter @@ fun (Linter.Linter l) ->
               match l.tags with
               | [] -> true
               | _ -> List.exists (fun x -> List.mem x l.tags) tags)
      in
      let context = { D.Programs.Context.root = None; config = store } in
      let data =
        let open D.Builder in
        build @@ fun b ->
        D.Programs.FileStore.lazy_builder (files ~report [ (name, Lua parsed) ]) b;
        oracle D.Programs.Context.key (fun _ _ -> context) b
      in
      let program, notes = Driver.lint_and_fix_all ~store ~data linters (Lua parsed) in
      let errs =
        Seq.append (List.to_seq !errors)
          (Driver.Notes.to_seq notes |> Seq.map Driver.Note.any_to_error)
        |> List.of_seq
        |> List.sort Illuaminate.Error.compare_by_position
      in

      let new_contents = Format.asprintf "%a" File.emit program in
      ( errs,
        if contents <> new_contents then Some (Diff.diff ~old:contents ~new_:new_contents) else None
      )
