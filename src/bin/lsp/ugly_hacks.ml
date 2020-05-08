open Lsp

let { Logger.log } = Logger.for_section "lsp"

(** {!Rpc.IO.send}, but public. Thanks, I hate it. *)
let send (packet : Rpc.Io.packet) =
  let json =
    match packet with
    | Request r -> Jsonrpc.Request.yojson_of_t r
    | Response r -> Jsonrpc.Response.yojson_of_t r
  in
  log ~title:Logger.Title.LocalDebug "send: %a"
    (fun () -> Yojson.Safe.pretty_to_string ~std:false)
    json;
  let data = Yojson.Safe.to_string json in
  let content_length = String.length data in
  let header = Header.create ~content_length in
  Header.write header stdout; output_string stdout data; flush stdout

let next_id = ref 0

let send_request request =
  let id = Lsp.Import.Either.Right !next_id in
  incr next_id;
  send (Request (Server_request.to_jsonrpc_request request ~id))
