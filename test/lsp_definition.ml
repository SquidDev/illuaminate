open Lsp_test
open Lsp.Types

let tests =
  Omnomnom.Tests.group "Definitions"
    [ test ~name:"Find the definition of a local" (fun t ->
          let uri = open_file t "vars.lua" in
          request t (TextDocumentDefinition { textDocument = { uri }; position = pos 0 6 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from definition"
               (Some
                  (`Location
                    [ Location.create ~uri ~range:(range 2 0 2 1);
                      Location.create ~uri ~range:(range 1 0 1 1);
                      Location.create ~uri ~range:(range 0 6 0 7)
                    ]));
          ());
      test ~name:"Find the definition of an argument" (fun t ->
          let uri = open_file t "vars.lua" in

          request t (TextDocumentDefinition { textDocument = { uri }; position = pos 5 8 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from usage"
               (Some (`Location [ Location.create ~uri ~range:(range 4 21 4 22) ]));

          request t (TextDocumentDefinition { textDocument = { uri }; position = pos 4 21 })
          |> Check.ok_response
          |> Testable.(check (option (locations location location_link)))
               "Received locations from definition"
               (Some (`Location [ Location.create ~uri ~range:(range 4 21 4 22) ]));

          ())
    ]
