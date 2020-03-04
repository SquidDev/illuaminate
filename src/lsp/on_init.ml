let init_info : Lsp.Initialize.Result.t =
  { capabilities =
      { textDocumentSync =
          { change = IncrementalSync;
            openClose = true;
            willSave = false;
            willSaveWaitUntil = false;
            save = None
          };
        hoverProvider = false;
        completionProvider = None;
        signatureHelpProvider = None;
        definitionProvider = true;
        typeDefinitionProvider = false;
        referencesProvider = true;
        documentHighlightProvider = true;
        documentSymbolProvider = false;
        workspaceSymbolProvider = false;
        codeActionProvider = Bool false;
        codeLensProvider = None;
        documentFormattingProvider = false;
        documentRangeFormattingProvider = false;
        documentOnTypeFormattingProvider = None;
        renameProvider = false;
        documentLinkProvider = None;
        executeCommandProvider = None;
        typeCoverageProvider = false;
        foldingRangeProvider = Bool false
      };
    serverInfo = Some { name = "illuaminate-lsp"; version = Some "%%VERSION%%" }
  }

let handle (_ : Lsp.Rpc.t) store (_ : Lsp.Initialize.Params.t) = Ok (store, init_info)
