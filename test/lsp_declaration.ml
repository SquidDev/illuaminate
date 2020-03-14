open Lsp_test
open Lsp.Types

let tests =
  Omnomnom.Tests.group "Declarations"
    [ test ~name:"Find the declaration of a local" (fun t ->
          let uri = open_file t "vars.lua" in
          request t (TextDocumentDeclaration { textDocument = { uri }; position = pos 0 6 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from declaration"
               (Some (`Location [ Location.create ~uri ~range:(range 0 6 0 7) ]));
          ());
      test ~name:"Find the declaration of an argument" (fun t ->
          let uri = open_file t "vars.lua" in

          request t (TextDocumentDeclaration { textDocument = { uri }; position = pos 5 8 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from usage"
               (Some (`Location [ Location.create ~uri ~range:(range 4 21 4 22) ]));

          request t (TextDocumentDeclaration { textDocument = { uri }; position = pos 4 21 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from declaration"
               (Some (`Location [ Location.create ~uri ~range:(range 4 21 4 22) ]));

          ())
    ]
