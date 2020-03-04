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

let send_request rpc request =
  let id = Lsp.Import.Either.Right !next_id in
  incr next_id;
  Server_request.to_jsonrpc_request request ~id |> Jsonrpc.Request.yojson_of_t |> send rpc
