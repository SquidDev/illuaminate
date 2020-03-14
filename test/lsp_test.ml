open Stdlib
open Lsp
open Lsp.Types
include IlluaminateLsp

module Check = struct
  let ok ~pp : ('a, 'e) Result.t -> 'a = function
    | Ok k -> k
    | Error e -> Alcotest.failf "Unexpected error:\n %a" pp e

  let pp_json out x = Format.pp_print_string out (Yojson.Safe.pretty_to_string x)

  let ok_json ~pp e = ok ~pp:(fun out x -> pp_json out (pp x)) e

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

  let json (type a) pp : a Alcotest.testable =
    ( module struct
      type t = a

      let pp out x = Check.pp_json out (pp x)

      let equal = ( = )
    end )

  let locations location link : Locations.t Alcotest.testable =
    ( module struct
      type t = Locations.t

      let pp out = function
        | `Location l -> pp (list location) out l
        | `LocationLink l -> pp (list link) out l

      let equal l r =
        match (l, r) with
        | `Location l, `Location r -> equal (list location) l r
        | `LocationLink l, `LocationLink r -> equal (list link) l r
        | (`Location _ | `LocationLink _), _ -> false
    end )

  let diagnostic = json Diagnostic.yojson_of_t

  let location = json Location.yojson_of_t

  let location_link = json LocationLink.yojson_of_t

  let document_highlight = json DocumentHighlight.yojson_of_t
end

type message =
  | Notification of Lsp.Server_notification.t
  | Request' : 'a Lsp.Server_request.t -> message

let capabilities = ClientCapabilities.create ()

type t =
  { outgoing : message Queue.t;
    workspace : Fpath.t;
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
        run { outgoing; workspace; server; client } )

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

let open_file { workspace; client; server; _ } f =
  let path = Fpath.(append workspace (v f) |> to_string) in
  let uri = Uri.of_path path |> Uri.to_string in
  let text = CCIO.(with_in path read_all) in

  server.notify client
    (TextDocumentDidOpen { textDocument = { uri; languageId = "lua"; version = 0; text } })
  |> Check.ok_s;
  uri

let range l1 c1 l2 c2 =
  Range.create
    ~start:(Position.create ~line:l1 ~character:c1)
    ~end_:(Position.create ~line:l2 ~character:c2)

let pos l c = Position.create ~line:l ~character:c

let request { client; server; _ } r = server.request client capabilities r

let notify { client; server; _ } r = server.notify client r
