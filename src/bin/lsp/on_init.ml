open Lsp
open Lsp.Types

let init_info : InitializeResult.t =
  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~change:Incremental ~openClose:true ~willSave:false
             ~willSaveWaitUntil:false ()))
      ~hoverProvider:(`Bool true) ~definitionProvider:(`Bool true) ~referencesProvider:(`Bool true)
      ~declarationProvider:(`Bool true) ~documentHighlightProvider:(`Bool true)
      ~codeActionProvider:(`Bool true)
      ~renameProvider:(`RenameOptions (RenameOptions.create ~prepareProvider:true ()))
      ~workspace:
        (ServerCapabilities.create_workspace
           ~workspaceFolders:
             (WorkspaceFoldersServerCapabilities.create ~supported:true
                ~changeNotifications:(`Bool true) ())
           ())
      ~executeCommandProvider:(ExecuteCommandOptions.create ~commands:[ "illuaminate/fix" ] ())
      ()
  in
  InitializeResult.create
    ~serverInfo:{ name = "illuaminate-lsp"; version = Some "%%VERSION%%" }
    ~capabilities ()

let handle (_ : Store.client_channel) store
    { InitializeParams.rootPath; rootUri; workspaceFolders; _ } =
  let root =
    match rootUri with
    | Some u -> Some (Store.Filename.box u)
    | None -> Option.join rootPath |> Option.map Uri.of_path
  in
  Store.update_workspace store ?root
    ~add:(Option.join workspaceFolders |> Option.value ~default:[])
    ~remove:[] ();
  Ok init_info
