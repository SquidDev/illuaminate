open Lsp_test
open Lsp.Types
open CCFun
open Leak

let test ~name ?n ?threshold ?workspace ~init actions =
  test ~name ~speed:`Slow ?workspace @@ fun t -> run ?n ?threshold ~init (actions t)

let tests =
  Omnomnom.Tests.group "Memory leaks"
    [ test ~name:"Open/Close" ~init:() (fun t ->
          action ~name:"Open" (fun () ->
              let uri = open_file t "basic.lua" in
              drain t; uri)
          >-> action ~name:"Close" (fun uri -> close_file t uri));
      test ~name:"Basic Editing" ~init:() (fun t ->
          let uri = open_file t "basic.lua" in
          let textDocument = VersionedTextDocumentIdentifier.create ~uri () in
          action ~name:"Insert character" (fun () ->
              notify t
                (TextDocumentDidChange
                   { textDocument;
                     contentChanges =
                       [ TextDocumentContentChangeEvent.create ~range:(range 0 0 0 0) ~text:"a" () ]
                   });
              drain t)
          >-> action ~name:"Remove character" (fun () ->
                  notify t
                    (TextDocumentDidChange
                       { textDocument;
                         contentChanges =
                           [ TextDocumentContentChangeEvent.create ~range:(range 0 0 0 1) ~text:""
                               ()
                           ]
                       });
                  drain t))
    ]
