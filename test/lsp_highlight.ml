open Lsp_test
open Lsp.Types

let tests =
  Omnomnom.Tests.group "References"
    [ test ~name:"Highlight all usages of a local" (fun t ->
          let uri = open_file t "vars.lua" in
          request t (TextDocumentHighlight { textDocument = { uri }; position = pos 0 6 })
          |> Check.ok_response
          |> Testable.(check (option (list document_highlight)))
               "Received locations from reference"
               (Some
                  [ DocumentHighlight.create ~kind:Write ~range:(range 0 6 0 7) ();
                    DocumentHighlight.create ~kind:Write ~range:(range 1 0 1 1) ();
                    DocumentHighlight.create ~kind:Write ~range:(range 2 0 2 1) ();
                    DocumentHighlight.create ~kind:Read ~range:(range 4 6 4 7) ();
                    DocumentHighlight.create ~kind:Read ~range:(range 4 9 4 10) ()
                  ]);
          ())
    ]
