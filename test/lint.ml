open Omnomnom.Tests
open IlluaminateCore
open IlluaminateLint
open IlluaminateConfig
open Lens

let only =
  let field =
    Term.field ~name:"only" ~comment:"Only use these linters" ~default:[]
      Term.Converter.(list string)
  in
  Category.add field Linter.category

let process ~name contents out =
  let lexbuf = Lexing.from_string contents in
  let name = { Span.name; path = name } in
  match IlluaminateParser.parse name lexbuf with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out (fun _ -> Some contents) errs
  | Ok parsed ->
      let store =
        let term =
          List.fold_left
            (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
            (Schema.singleton only) Linters.all
          |> Schema.to_term
        in
        match parsed ^. Syntax.First.program with
        | Node { leading_trivia = { value = LineComment c | BlockComment (_, c); _ } :: _; _ } -> (
            let c = String.trim c in
            match CCString.chop_prefix ~pre:"config:" c with
            | Some c ->
                let buf = Lexing.from_string c in
                Term.to_parser term |> Parser.fields
                |> Parser.parse_buf { Span.path = "?"; name = "?" } buf
                |> Result.fold ~ok:Fun.id ~error:(fun (_, x) -> failwith x)
            | None -> Term.default term )
        | _ -> Term.default term
      in
      let only = Schema.get only store in
      let only =
        List.map
          (fun x ->
            match Error.Tag.find x with
            | None -> failwith (Printf.sprintf "Unknown tag %S" x)
            | Some t -> t)
          only
      in
      let data = Data.create () in
      let linters =
        Linters.all
        |> List.filter (fun (Linter.Linter l) ->
               match only with
               | [] -> true
               | _ -> List.exists (fun x -> List.mem x l.tags) only)
      in
      let program, notes = Driver.lint_and_fix_all ~store ~data linters parsed in
      let errs = Error.make () in
      List.iter (Driver.report_note errs) notes;
      Error.display_of_string ~out (fun _ -> Some contents) errs;
      let new_contents = Format.asprintf "%a" Emit.program program in
      if contents <> new_contents then Helpers.diff out contents new_contents

let process ~name contents = Format.asprintf "%t" (process ~name contents)

let tests =
  group "Linting"
    [ OmnomnomGolden.of_directory process ~group:"Basic lints" ~directory:"data/lint"
        ~extension:".lua" ()
    ]
