open Lsp_test
open Lsp.Types

let diagnostics ?uri t =
  get_notification t @@ function
  | PublishDiagnostics t -> (
    match uri with
    | Some uri when t.uri <> uri -> None
    | _ -> Some t.diagnostics )
  | _ -> None

let tests =
  Omnomnom.Tests.group "Diagnostics"
    [ test ~name:"Well formed files have no diagnostic" (fun t ->
          let uri = open_file t "basic.lua" in
          diagnostics ~uri t |> Testable.(check (list diagnostic)) "Has no diagnostics" [];
          ());
      test ~name:"Parse errors show a diagnostic" (fun t ->
          (* [print("This is a basic file")] *)
          let uri = open_file t "basic.lua" in
          let _ = diagnostics ~uri t in

          (* [print "This is a basic file")] *)
          notify t
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 5 0 6) ~text:" " () ]
               })
          |> Check.ok_s;

          diagnostics ~uri t
          |> Testable.(check (list diagnostic))
               "Has a parse error"
               [ Diagnostic.create ~range:(range 0 28 0 29) ~code:(Left "parse:syntax-error")
                   ~source:"illuaminate" ~severity:Error
                   ~message:"Unexpected `)`: Unexpected token after function call" ()
               ];

          (* [print "This is a basic file" ] *)
          notify t
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 28 0 29) ~text:"" () ]
               })
          |> Check.ok_s;

          diagnostics ~uri t
          |> Testable.(check (list diagnostic)) "Has no parse errors after an edit" [];
          ());
      test ~name:"Linter errors show a diagnostic" (fun t ->
          (* [print("This is a basic file")] *)
          let uri = open_file t "basic.lua" in
          let _ = diagnostics ~uri t in

          (* [print "This is a basic file")] *)
          notify t
            (TextDocumentDidChange
               { textDocument = VersionedTextDocumentIdentifier.create ~uri ();
                 contentChanges =
                   [ TextDocumentContentChangeEvent.create ~range:(range 0 0 0 0)
                       ~text:"local x = 0\n" ()
                   ]
               })
          |> Check.ok_s;

          diagnostics ~uri t
          |> Testable.(check (list diagnostic))
               "Has an unused variable error"
               [ Diagnostic.create ~range:(range 0 6 0 7) ~code:(Left "var:unused")
                   ~source:"illuaminate" ~severity:Warning ~message:"Unused variable \"x\"." ()
               ];
          ());
      test ~name:"Picks up configuration" ~workspace:"linters" (fun t ->
          let allow = open_file t "allow_unused.lua" in
          diagnostics ~uri:allow t
          |> Testable.(check (list diagnostic)) "allow_unused's diagnostics are empty" [];

          let check = open_file t "check_unused.lua" in
          diagnostics ~uri:check t
          |> Testable.(check (list diagnostic))
               "check_unused's warns on unused variables"
               [ Diagnostic.create ~range:(range 0 6 0 7) ~code:(Left "var:unused")
                   ~source:"illuaminate" ~severity:Warning ~message:"Unused variable \"x\"." ()
               ];

          ())
    ]
