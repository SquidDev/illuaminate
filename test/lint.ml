open IlluaminateLint
open IlluaminateConfig
open IlluaminateSemantics
open Import

let schema =
  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    Schema.empty Linters.all
  |> Schema.union (Schema.singleton Doc.Extract.Config.key)

let files ?(extra = []) () =
  lazy
    (let module FileStore = D.Programs.FileStore in
    let files = FileStore.create () in
    List.iter (fun (name, program) -> FileStore.update files name (Some program)) extra;
    files)

let _leak =
  let open Leak in
  let name = Illuaminate.File_id.mk "=stdin" in
  let store = Schema.default schema in
  let context = { D.Programs.Context.root = None; config = store } in
  let data =
    let open D.Builder in
    build @@ fun b ->
    D.Programs.FileStore.lazy_builder (files ()) b;
    oracle D.Programs.Context.key (fun _ _ -> context) b
  in

  OmnomnomAlcotest.mk_alcotest_case "Memory leaks" `Slow @@ fun () ->
  Leak.run_unit ~n:1000
    (action ~name:"Parse file" (fun () ->
         let contents = "print('hello world')" in
         match Lexing.from_string contents |> IlluaminateParser.program name with
         | Error err ->
             Alcotest.failf "%t" (fun out ->
                 Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
                   (fun _ -> Some contents)
                   [ IlluaminateParser.Error.to_error err ])
         | Ok parsed -> parsed)
    >-> action ~name:"Lint and fix" (fun prog ->
            match Driver.lint_and_fix_all ~store ~data Linters.all (Lua prog) with
            | Lua x, _ -> x
            | _ -> assert false)
    >-> action ~name:"Lint fixed program" (fun prog ->
            List.iter (fun l -> Driver.lint ~store ~data l (Lua prog) |> ignore) Linters.all))

let tests = Omnomnom.Tests.group "Linting" []
