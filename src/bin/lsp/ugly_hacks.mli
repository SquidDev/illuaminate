(** Terrible hacks to work around missing features in ocaml-lsp. *)

(** Send a server request. *)
val send_request : 'a Lsp.Server_request.t -> unit
