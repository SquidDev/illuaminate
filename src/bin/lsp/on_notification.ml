open Lsp

let src = Logs.Src.create ~doc:"Notification handler" __MODULE__

module Log = (val Logs.src_log src)

let send_diagnostics rpc store ?version doc =
  let diagnostics = Lint.diagnostics store doc in
  Rpc.send_notification rpc
    (PublishDiagnostics { uri = Uri.to_string doc.uri; version; diagnostics })

let worker rpc store : Client_notification.t -> (Store.t, string) result = function
  | TextDocumentDidOpen { textDocument = { uri; version; text; languageId } } ->
      if languageId = "lua" then (
        let doc =
          Store.open_file store (Text_document.make ~version (Store.Filename.box uri) text)
        in
        send_diagnostics rpc store ~version doc;
        () );
      Ok store
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
      let uri = Store.Filename.box uri in
      ( match Store.get_file store uri with
      | Some ({ contents = Open contents; _ } as doc) ->
          List.fold_left
            (fun doc change -> Text_document.apply_content_change ?version change doc)
            contents contentChanges
          |> Store.update_file store doc;
          send_diagnostics rpc store ?version doc
      | _ -> Log.err (fun f -> f "Cannot update file %a (absent or missing contents)" Uri.pp uri) );
      Ok store
  (* TODO: Implement. *)
  | TextDocumentDidClose _ -> Ok store
  | ChangeWorkspaceFolders _ -> Ok store
  | ChangeConfiguration _ -> Ok store
  (* Whole bunch of notifications we can ignore. *)
  | WillSaveTextDocument _ | DidSaveTextDocument _ | Initialized | Exit | Unknown_notification _ ->
      Ok store

let handle rpc store noti =
  try worker rpc store noti
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling notification: %s\n%s" e bt);
    Error e
