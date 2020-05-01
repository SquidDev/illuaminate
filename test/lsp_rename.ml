open Lsp_test
open Lsp.Types

let prepare =
  Omnomnom.Tests.group "Prepare"
    [ test ~name:"Allows renaming variables" (fun t ->
          let uri = open_file t "vars.lua" in
          request t (TextDocumentPrepareRename { textDocument = { uri }; position = pos 0 6 })
          |> Check.ok_response
          |> Testable.(check (option range)) "Received a single position" (Some (range 0 6 0 7));
          ());
      test ~name:"Does not allow renaming globals" ~workspace:"rename" (fun t ->
          let uri = open_file t "odd_vars.lua" in
          request t (TextDocumentPrepareRename { textDocument = { uri }; position = pos 0 9 })
          |> Check.ok_response
          |> Testable.(check (option range)) "Received no positions" None;
          ());
      test ~name:"Does not allow renaming self" ~workspace:"rename" (fun t ->
          let uri = open_file t "odd_vars.lua" in
          request t (TextDocumentPrepareRename { textDocument = { uri }; position = pos 1 9 })
          |> Check.ok_response
          |> Testable.(check (option range)) "Received no positions" None;
          ());
      test ~name:"Does not allow renaming args" ~workspace:"rename" (fun t ->
          let uri = open_file t "odd_vars.lua" in
          request t (TextDocumentPrepareRename { textDocument = { uri }; position = pos 5 12 })
          |> Check.ok_response
          |> Testable.(check (option range)) "Received no positions" None;
          ())
    ]

let read_message t =
  get_notification t (function
    | ShowMessage m -> Some m.message
    | _ -> None)

let tests =
  Omnomnom.Tests.group "Renaming"
    [ prepare;
      test ~name:"Allows renaming variables" (fun t ->
          let uri = open_file t "vars.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 0 6; newName = "y" })
          |> Check.ok_response |> apply_change t;

          contents t uri
          |> Alcotest.(check string) "vars.lua renamed all usages" (read_file t "vars.renamed");
          ());
      test ~name:"Fails on invalid identifiers" (fun t ->
          let uri = open_file t "vars.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 0 6; newName = "123" })
          |> Check.ok_response
          |> Testable.(check workspace_edit) "No changes given" (WorkspaceEdit.create ());

          read_message t
          |> Testable.(check string) "Gives an error message" "\"123\" is not a valid identifier.";
          ());
      test ~name:"Fails on renaming globals" ~workspace:"rename" (fun t ->
          let uri = open_file t "odd_vars.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 0 9; newName = "abc" })
          |> Check.ok_response
          |> Testable.(check workspace_edit) "No changes given" (WorkspaceEdit.create ());

          read_message t
          |> Testable.(check string) "Gives an error message" "Global \"x\" cannot be renamed.";
          ());
      test ~name:"Ensures usages are not shadowed" ~workspace:"rename" (fun t ->
          let uri = open_file t "shadow.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 0 6; newName = "y" })
          |> Check.ok_response
          |> Testable.(check workspace_edit) "No changes given" (WorkspaceEdit.create ());

          read_message t
          |> Testable.(check string)
               "Gives an error message"
               "Usage of x on line 3 would be shadowed by the definition of y on line 2.";
          ());
      test ~name:"Ensures other definitions are not shadowed" ~workspace:"rename" (fun t ->
          let uri = open_file t "shadow.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 1 6; newName = "x" })
          |> Check.ok_response
          |> Testable.(check workspace_edit) "No changes given" (WorkspaceEdit.create ());

          read_message t
          |> Testable.(check string)
               "Gives an error message" "Renaming y would shadow usage of x on line 3.";
          ());
      test ~name:"Correctly checks scope of shadowed variables" ~workspace:"rename" (fun t ->
          let uri = open_file t "shadow_usage_ok.lua" in
          request t
            (TextDocumentRename { textDocument = { uri }; position = pos 5 8; newName = "x" })
          |> Check.ok_response |> apply_change t;

          contents t uri
          |> Alcotest.(check string) "Renamed all usages" (read_file t "shadow_usage_ok.renamed");
          ())
    ]
