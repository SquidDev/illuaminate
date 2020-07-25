open Lsp
module D = IlluaminateData

let src = Logs.Src.create ~doc:"Notification handler" __MODULE__

module Log = (val Logs.src_log src)

let send_diagnostics client store ?version doc =
  let diagnostics = Lint.diagnostics store doc in
  client.Store.notify (PublishDiagnostics { uri = Uri.to_string doc.uri; version; diagnostics })

let worker client store : Client_notification.t -> unit Fiber.t = function
  | TextDocumentDidOpen ({ textDocument = { version; languageId; _ } } as d) ->
      if languageId = "lua" then
        let doc = Store.open_file store (Text_document.make d) in
        send_diagnostics client store ~version doc
      else Fiber.return ()
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } -> (
      let uri = Store.Filename.box uri in
      match Store.get_file store uri with
      | Some doc ->
          List.fold_left
            (fun doc change -> Text_document.apply_content_change ?version change doc)
            doc.contents contentChanges
          |> Store.update_file store doc;
          send_diagnostics client store ?version doc
      | _ ->
          Log.err (fun f -> f "Cannot update file %a (not currently open)" Uri.pp uri);
          Fiber.return () )
  | TextDocumentDidClose { textDocument = { uri; _ } } ->
      Store.close_file store (Store.Filename.box uri);
      Fiber.return ()
  (* TODO: Implement. *)
  | ChangeWorkspaceFolders { event = { added; removed } } ->
      Store.update_workspace store ~add:added ~remove:removed ();
      Fiber.return ()
  (* Whole bunch of notifications we can ignore. *)
  | ChangeConfiguration _ | WillSaveTextDocument _ | DidSaveTextDocument _ | Initialized | Exit
  | Unknown_notification { method_ = "$/illuaminate/dump"; _ } ->
      Format.asprintf "%a" (D.pp_store ~all:true) (Store.data store) |> Printf.printf "%s\n";
      Fiber.return ()
  | Unknown_notification _ -> Fiber.return ()

let handle client store noti =
  try worker client store noti
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling notification: %s\n%s" e bt);
    Fiber.return ()
