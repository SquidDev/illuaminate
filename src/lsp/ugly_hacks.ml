open Lsp

let { Logger.log } = Logger.for_section "lsp"

(** {!Rpc.send}, but public. Thanks, I hate it. *)
let send (_ : Rpc.t) json =
  log ~title:"debug" "send: %a" (fun () -> Yojson.Safe.pretty_to_string ~std:false) json;
  let data = Yojson.Safe.to_string json in
  let content_length = String.length data in
  let header = Header.create ~content_length in
  Header.write header stdout; output_string stdout data; flush stdout

let next_id = ref 0

let send_request (type t) rpc (request : t Server_request.t) =
  let id = Lsp.Import.Either.Right !next_id in
  incr next_id;
  let r = Server_request.to_jsonrpc_request request ~id in
  let r =
    match request with
    | WorkspaceApplyEdit { label; edit = [ edit ]; _ } ->
        let bnds = [ ("edit", Protocol.WorkspaceEdit.yojson_of_t edit) ] in
        let bnds =
          match label with
          | None -> bnds
          | Some v -> ("label", `String v) :: bnds
        in
        { r with params = Some (`Assoc bnds) }
    | WorkspaceApplyEdit { edit = _; _ } -> assert false
    | _ -> r
  in
  Jsonrpc.Request.yojson_of_t r |> send rpc
