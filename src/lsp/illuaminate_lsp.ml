open IlluaminateSemantics
open IlluaminateCore

let { Lsp.Logger.log } = Lsp.Logger.for_section "illuaminate-lsp"

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
        definitionProvider = false;
        typeDefinitionProvider = false;
        referencesProvider = false;
        documentHighlightProvider = false;
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

let run f =
  try f ()
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    log ~title:"error" "Error making function call: %s\n%s" e bt;
    Error e

let on_notification rps store cap : Lsp.Client_notification.t -> (Store.t, string) result =
  let open Lsp in
  function
  | TextDocumentDidOpen { textDocument = { uri; version; text; languageId } } ->
      ( if languageId = "lua" then
        let _ = Store.create_file store (Lsp.Text_document.make ~version uri text) in
        (* TODO: send_diagnostics rpc doc *)
        () );
      Ok store
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
      Document_store.get store uri >>= fun prev_doc ->
      let doc =
        let f doc change = Document.update_text ~version change doc in
        List.fold_left ~f ~init:prev_doc contentChanges
      in
      Document_store.put store doc; send_diagnostics rpc doc; Ok store
  | _ -> Error "NYI"

let main () =
  Lsp.Rpc.start (Store.create ())
    { on_initialize = (fun rpc state params -> Ok (state, init_info));
      on_request = (fun rpc state cap req -> Error "Nope");
      on_notification = (fun rpc state noti -> Error "Nope")
    }
    stdin stdout;
  log ~title:"info" "Exiting"

let () =
  let open Cmdliner in
  let open Term in
  let log_file =
    let open Arg in
    value
    & opt (some string) None
    & info [ "log" ] ~docv:"FILE" ~doc:"Enable logging to FILE (or - for stdout)."
  in

  let cmd =
    ( const (fun log_file -> Lsp.Logger.with_log_file log_file main) $ log_file,
      info "illuaminate-lsp" ~doc:"Start a Language Server Protocol sever for illuaminate" )
  in
  exit @@ eval cmd
