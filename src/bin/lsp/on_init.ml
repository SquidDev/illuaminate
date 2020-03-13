open Lsp

let init_info : Initialize.Result.t =
  { capabilities =
      { textDocumentSync =
          { change = IncrementalSync;
            openClose = true;
            willSave = false;
            willSaveWaitUntil = false;
            save = None
          };
        hoverProvider = true;
        completionProvider = None;
        signatureHelpProvider = None;
        definitionProvider = true;
        typeDefinitionProvider = false;
        referencesProvider = true;
        documentHighlightProvider = true;
        documentSymbolProvider = false;
        workspaceSymbolProvider = false;
        codeActionProvider = Bool true;
        codeLensProvider = None;
        documentFormattingProvider = false;
        documentRangeFormattingProvider = false;
        documentOnTypeFormattingProvider = None;
        renameProvider = false;
        documentLinkProvider = None;
        executeCommandProvider = Some { commands = [ "illuaminate/fix" ] };
        typeCoverageProvider = false;
        foldingRangeProvider = Bool false
      };
    serverInfo = Some { name = "illuaminate-lsp"; version = Some "%%VERSION%%" }
  }

let handle rpc store
    { Initialize.Params.rootPath; rootUri; workspaceFolders; capabilities = { workspace; _ }; _ } =
  if workspace.workspaceFolders then
    Ugly_hacks.send_request rpc
      (ClientRegisterCapability
         { registrations =
             [ { id = ""; method_ = "workspace/didChangeWorkspaceFolders"; registerOptions = None }
             ]
         });

  let root =
    match rootUri with
    | Some u -> Some u
    | None -> Option.map Uri.of_path rootPath
  in
  Store.set_workspace store ?root workspaceFolders;
  Ok (store, init_info)
