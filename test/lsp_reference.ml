open Lsp_test
open Lsp.Types

let tests =
  let reference ~uri ~position =
    ReferenceParams.create ~textDocument:{ uri } ~position ~context:{ includeDeclaration = false }
  in
  Omnomnom.Tests.group "References"
    [ test ~name:"Find the reference of a local" (fun t ->
          let uri = open_file t "vars.lua" in
          request t (TextDocumentReferences (reference ~uri ~position:(pos 0 6)))
          |> Check.ok_response
          |> Testable.(check (option (list location)))
               "Received locations from reference"
               (Some
                  [ Location.create ~uri ~range:(range 4 6 4 7);
                    Location.create ~uri ~range:(range 4 9 4 10)
                  ]);
          ());
      test ~name:"Find the reference of an argument" (fun t ->
          let uri = open_file t "vars.lua" in

          request t (TextDocumentReferences (reference ~uri ~position:(pos 5 8)))
          |> Check.ok_response
          |> Testable.(check (option (list location)))
               "Received locations from usage"
               (Some [ Location.create ~uri ~range:(range 5 8 5 9) ]);

          request t (TextDocumentReferences (reference ~uri ~position:(pos 4 21)))
          |> Check.ok_response
          |> Testable.(check (option (list location)))
               "Received locations from reference"
               (Some [ Location.create ~uri ~range:(range 5 8 5 9) ]);

          ())
    ]
