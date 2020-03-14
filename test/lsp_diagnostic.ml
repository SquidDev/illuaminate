open Lsp_test
open Lsp.Types

let diagnostics ?uri t =
  get_notification t @@ function
  | PublishDiagnostics t -> (
    match uri with
    | Some uri when t.uri <> uri -> None
    | _ -> Some t )
  | _ -> None

let tests =
  Omnomnom.Tests.group "Diagnostics"
    [ test ~name:"Well formed files have no diagnostic" (fun t _ _ ->
          let uri = open_file t "basic.lua" in
          let diags = diagnostics ~uri t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "Has no diagnostics" [] diags.diagnostics;
          ());
      test ~name:"Parse errors show a diagnostic" (fun t client server ->
          (* [print("This is a basic file")] *)
          let uri = open_file t "basic.lua" in
          let _ = diagnostics ~uri t in

          (* [print "This is a basic file")] *)
          server.notify client
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 5 0 6) ~text:" " () ]
               })
          |> Check.ok_s;

          let diags = diagnostics ~uri t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "Has a parse error"
            [ Diagnostic.create ~range:(range 0 28 0 29) ~code:(Left "parse:syntax-error")
                ~source:"illuaminate" ~severity:Error
                ~message:"Unexpected `)`: Unexpected token after function call" ()
            ]
            diags.diagnostics;

          (* [print "This is a basic file" ] *)
          server.notify client
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 28 0 29) ~text:"" () ]
               })
          |> Check.ok_s;

          let diags = diagnostics ~uri t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "Has no parse errors after an edit" [] diags.diagnostics;
          ());
      test ~name:"Linter errors show a diagnostic" (fun t client server ->
          (* [print("This is a basic file")] *)
          let uri = open_file t "basic.lua" in
          let _ = diagnostics ~uri t in

          (* [print "This is a basic file")] *)
          server.notify client
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 0 0 0)
                       ~text:"local x = 0\n" ()
                   ]
               })
          |> Check.ok_s;

          let diags = diagnostics ~uri t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "Has an unused variable error"
            [ Diagnostic.create ~range:(range 0 6 0 7) ~code:(Left "var:unused")
                ~source:"illuaminate" ~severity:Warning ~message:"Unused variable \"x\"." ()
            ]
            diags.diagnostics;
          ());
      test ~name:"Picks up configuration" ~workspace:"linters" (fun t _ _ ->
          let allow = open_file t "allow_unused.lua" in
          let d = diagnostics ~uri:allow t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "allow_unused's diagnostics are empty" [] d.diagnostics;

          let check = open_file t "check_unused.lua" in
          let d = diagnostics ~uri:check t in
          Alcotest.(check (list (Check.json Diagnostic.yojson_of_t)))
            "check_unused's warns on unused variables"
            [ Diagnostic.create ~range:(range 0 6 0 7) ~code:(Left "var:unused")
                ~source:"illuaminate" ~severity:Warning ~message:"Unused variable \"x\"." ()
            ]
            d.diagnostics;

          ())
    ]
