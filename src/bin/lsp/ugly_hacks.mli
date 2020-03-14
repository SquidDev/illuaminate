(** Terrible hacks to work around missing features in ocaml-lsp. *)

(** Send a server request. *)
val send_request : Lsp.Rpc.t -> 'a Lsp.Server_request.t -> unit
