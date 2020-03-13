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
      ~workspace:
        { workspaceFolders =
            Some
              (WorkspaceFoldersServerCapabilities.create ~supported:true
                 ~changeNotifications:(`Bool true) ())
        }
      ~executeCommandProvider:(ExecuteCommandOptions.create ~commands:[ "illuaminate/fix" ] ())
      ()
  in
  InitializeResult.create
    ~serverInfo:{ name = "illuaminate-lsp"; version = Some "%%VERSION%%" }
    ~capabilities ()

let handle (_ : Rpc.t) store { InitializeParams.rootPath; rootUri; workspaceFolders; _ } =
  let root =
    match rootUri with
    | Some u -> Some (Store.Filename.box u)
    | None -> Option.join rootPath |> Option.map Uri.of_path
  in
  Store.set_workspace store ?root (Option.join workspaceFolders |> Option.value ~default:[]);
  Ok (store, init_info)
