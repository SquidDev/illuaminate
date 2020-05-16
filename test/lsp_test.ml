open Lsp
open Lsp.Types
include IlluaminateLsp

let pp_json out x = Format.pp_print_string out (Yojson.Safe.pretty_to_string x)

let json_pp pp out x = pp_json out (pp x)

module Check = struct
  let ok ~pp : ('a, 'e) Result.t -> 'a = function
    | Ok k -> k
    | Error e -> Alcotest.failf "Unexpected error:\n %a" pp e

  let ok_json ~pp e = ok ~pp:(json_pp pp) e

  let json_err { Jsonrpc.Response.Error.message; data; _ } =
    let bnds : (string * Yojson.Safe.t) list = [] in
    let bnds =
      match data with
      | None -> bnds
      | Some v -> ("data", v) :: bnds
    in
    `Assoc (("message", `String message) :: bnds)

  let ok_response k = ok_json ~pp:json_err k

  let ok_s e = ok ~pp:Format.pp_print_string e
end

module Testable = struct
  include Alcotest

  let mk (type a) ~eq ~pp : a testable =
    ( module struct
      type t = a

      let pp = pp

      let equal = eq
    end )

  let yojson : Yojson.Safe.t testable = mk ~pp:pp_json ~eq:( = )

  let json pp : 'a Alcotest.testable = mk ~pp:(json_pp pp) ~eq:( = )

  let locations location link : Locations.t testable =
    let pp out = function
      | `Location l -> pp (list location) out l
      | `LocationLink l -> pp (list link) out l
    in
    let eq l r =
      match (l, r) with
      | `Location l, `Location r -> equal (list location) l r
      | `LocationLink l, `LocationLink r -> equal (list link) l r
      | (`Location _ | `LocationLink _), _ -> false
    in
    mk ~pp ~eq

  let diagnostic = json Diagnostic.yojson_of_t

  let location = json Location.yojson_of_t

  let location_link = json LocationLink.yojson_of_t

  let document_highlight = json DocumentHighlight.yojson_of_t

  let command ?(title = string) ?(command = string) ?arguments () : Command.t testable =
    let arguments = Option.value ~default:yojson arguments |> list |> option in
    mk
      ~pp:(fun out ({ title = t; command = c; arguments = a } : Command.t) ->
        Format.pp_open_vbox out 2;
        Format.fprintf out "Title: %a@;" (pp title) t;
        Format.fprintf out "Command: %a@;" (pp command) c;
        Format.fprintf out "Arguments: %a" (pp arguments) a;
        Format.pp_close_box out ())
      ~eq:(fun (x : Command.t) (y : Command.t) ->
        equal title x.title y.title && equal command x.command y.command
        && equal arguments x.arguments y.arguments)

  let def_command = command ()

  let workspace_edit = json WorkspaceEdit.yojson_of_t

  let code_action_kind = json CodeActionKind.yojson_of_t

  let position = json Position.yojson_of_t

  let range = json Range.yojson_of_t

  let code_action ?(title = string) ?(diagnostic = diagnostic) ?(command = def_command) () =
    mk
      ~pp:
        (fun out
             ({ title = t; kind; diagnostics = d; isPreferred; edit; command = c } : CodeAction.t) ->
        Format.pp_open_vbox out 2;
        Format.fprintf out "Title: %a@;" (pp title) t;
        Format.fprintf out "Kind: %a@;" (Fmt.option (pp code_action_kind)) kind;
        Format.fprintf out "Diagnostics: %a@;" Fmt.(option (list (pp diagnostic))) d;
        Format.fprintf out "Is Preferred: %a@;" Fmt.(option bool) isPreferred;
        Format.fprintf out "Edit: %a@;" Fmt.(option (pp workspace_edit)) edit;
        Format.fprintf out "Command: %a" (pp (option command)) c;
        Format.pp_close_box out ())
      ~eq:(fun (l : CodeAction.t) (r : CodeAction.t) ->
        equal title l.title r.title
        && equal (option code_action_kind) l.kind r.kind
        && equal (option (list diagnostic)) l.diagnostics r.diagnostics
        && equal (option bool) l.isPreferred r.isPreferred
        && equal (option workspace_edit) l.edit r.edit
        && equal (option command) l.command r.command)

  let code_action_result command code_action : CodeActionResult.t testable =
    mk
      ~pp:(fun out x ->
        match x with
        | `Command x -> pp command out x
        | `CodeAction x -> pp code_action out x)
      ~eq:(fun x y ->
        match (x, y) with
        | `Command x, `Command y -> equal command x y
        | `CodeAction x, `CodeAction y -> equal code_action x y
        | `Command _, `CodeAction _ | `CodeAction _, `Command _ -> false)
    |> list |> option

  let symbol_information ?(location = location) () : SymbolInformation.t testable =
    mk
      ~pp:(fun out { SymbolInformation.name; kind; location = loc; deprecated; containerName } ->
        Format.pp_open_vbox out 2;
        Format.fprintf out "Name: %s (%a)@;" name (json_pp SymbolKind.yojson_of_t) kind;
        Option.iter (Format.fprintf out "Deprecated: %b@;") deprecated;
        Option.iter (Format.fprintf out "Container: %s@;") containerName;
        Format.fprintf out "Location: %a" (pp location) loc;
        Format.pp_close_box out ())
      ~eq:(fun (x : SymbolInformation.t) (y : SymbolInformation.t) ->
        equal location x.location y.location
        && x.name = y.name && x.kind = y.kind && x.deprecated = y.deprecated
        && x.containerName = y.containerName)
end

type message =
  | Notification of Lsp.Server_notification.t
  | Request' : 'a Lsp.Server_request.t -> message

let capabilities = ClientCapabilities.create ()

type t =
  { outgoing : message Queue.t;
    workspace : Fpath.t;
    files : (DocumentUri.t, string) Hashtbl.t;
    client : client_channel;
    server : server_channel
  }

let test ~name ?workspace run =
  OmnomnomAlcotest.of_alcotest_case
    ( name,
      `Quick,
      fun () ->
        let workspace =
          Option.value ~default:"default" workspace
          |> Fpath.v
          |> Fpath.(append (v (Sys.getcwd ()) / "data" / "lsp"))
        in
        let rootUri = Fpath.to_string workspace |> Uri.of_path |> Uri.to_string in
        let server = IlluaminateLsp.server () in
        let outgoing = Queue.create () in
        let client : client_channel =
          { request = (fun r -> Queue.add (Request' r) outgoing);
            notify = (fun n -> Queue.add (Notification n) outgoing)
          }
        in
        InitializeParams.create ~capabilities ~rootUri ()
        |> server.initialize client |> Check.ok_s |> ignore;
        run { outgoing; workspace; server; client; files = Hashtbl.create 2 } )

type some_request = Request : 'a Lsp.Server_request.t -> some_request

let rec get_request t f =
  match Queue.take_opt t.outgoing with
  | None -> Alcotest.fail "No matching request"
  | Some (Notification _) -> get_request t f
  | Some (Request' req) -> (
    match f (Request req) with
    | Some x -> x
    | None -> get_request t f )

let rec get_notification t f =
  match Queue.take_opt t.outgoing with
  | None -> Alcotest.fail "No matching notification"
  | Some (Request' _) -> get_notification t f
  | Some (Notification noti) -> (
    match f noti with
    | Some x -> x
    | None -> get_notification t f )

let read_file { workspace; _ } f =
  let path = Fpath.(append workspace (v f) |> to_string) in
  CCIO.(with_in path read_all)

let resolve_file { workspace; _ } f =
  Fpath.(append workspace (v f) |> to_string) |> Uri.of_path |> Uri.to_string

let open_file ({ client; server; files; _ } as t) f =
  let text = read_file t f and uri = resolve_file t f in
  Hashtbl.replace files uri text;
  server.notify client
    (TextDocumentDidOpen { textDocument = { uri; languageId = "lua"; version = 0; text } })
  |> Check.ok_s;
  uri

let contents { files; _ } filename = Hashtbl.find files filename

let apply_change { files; _ } { WorkspaceEdit.changes; documentChanges; _ } =
  let apply_edit uri edits =
    let content = Hashtbl.find files uri in
    let doc = TextDocumentItem.create ~uri ~languageId:"_" ~version:0 ~text:content in
    let doc = DidOpenTextDocumentParams.create ~textDocument:doc |> Text_document.make in
    List.fold_left
      (fun c { TextEdit.range; newText } ->
        Logs.info (fun f -> f "Applying edit at %a: %s" (json_pp Range.yojson_of_t) range newText);
        Text_document.apply_content_change
          (TextDocumentContentChangeEvent.create ~range ~text:newText ())
          c)
      doc edits
    |> Text_document.text |> Hashtbl.replace files uri
  in
  let apply_doc_edit ({ textDocument = { uri; _ }; edits } : TextDocumentEdit.t) =
    apply_edit uri edits
  in
  let apply_change = function
    | `TextDocumentEdit x -> apply_doc_edit x
    | `CreateFile _ | `RenameFile _ | `DeleteFile _ ->
        Alcotest.fail "File operations not yet supported"
  in
  Option.iter (List.iter (fun (f, e) -> apply_edit f e)) changes;
  Option.iter (List.iter apply_change) documentChanges

let apply_edits t =
  get_request t @@ function
  | Request (WorkspaceApplyEdit { edit; _ }) -> apply_change t edit; Some ()
  | _ -> Alcotest.fail "Expecting workspace edit"

let range l1 c1 l2 c2 =
  Range.create
    ~start:(Position.create ~line:l1 ~character:c1)
    ~end_:(Position.create ~line:l2 ~character:c2)

let pos l c = Position.create ~line:l ~character:c

let request { client; server; _ } r = server.request client capabilities r

let notify { client; server; _ } r = server.notify client r
