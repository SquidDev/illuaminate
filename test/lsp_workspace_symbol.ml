open Lsp.Types
open Lsp_test

let compare_names x y =
  match String.compare x.SymbolInformation.name y.SymbolInformation.name with
  | 0 -> Option.compare String.compare x.containerName y.containerName
  | i -> i

let location =
  let uri = Lsp.Uri.of_path "/" |> Lsp.Uri.to_string in
  Location.create ~uri ~range:(range 0 0 0 0)

let a_module =
  [ SymbolInformation.create ~name:"a_module" ~kind:Module ~location ();
    SymbolInformation.create ~name:"Foo" ~kind:Class ~containerName:"a_module" ~location ();
    SymbolInformation.create ~name:"a" ~kind:Method ~containerName:"a_module.Foo" ~location ();
    SymbolInformation.create ~name:"b" ~kind:Method ~containerName:"a_module.Foo" ~location ();
    SymbolInformation.create ~name:"c" ~kind:Number ~containerName:"a_module.Foo" ~location ();
    SymbolInformation.create ~name:"a" ~kind:Function ~containerName:"a_module" ~deprecated:true
      ~location ();
    SymbolInformation.create ~name:"b" ~kind:Number ~containerName:"a_module" ~location ();
    SymbolInformation.create ~name:"c" ~kind:Variable ~containerName:"a_module" ~location ()
  ]

let tests =
  Omnomnom.Tests.group "Workspace Symbol"
    [ test ~name:"Lists all symbols" ~workspace:"symbols" (fun t ->
          request t (WorkspaceSymbol { query = "" })
          |> Check.ok_response
          |> Option.map (List.sort compare_names)
          |> Testable.(check (option (slist (symbol_information ~location:pass ()) compare_names)))
               "Lists all symbols" (Some a_module);
          ());
      test ~name:"Filters the module list" ~workspace:"symbols" (fun t ->
          request t (WorkspaceSymbol { query = "Fo" })
          |> Check.ok_response
          |> Option.map (List.sort compare_names)
          |> Testable.(check (option (slist (symbol_information ~location:pass ()) compare_names)))
               "Lists all symbols"
               (Some
                  [ SymbolInformation.create ~name:"Foo" ~kind:Class ~containerName:"a_module"
                      ~location ()
                  ]);
          ())
    ]
