include module type of IlluaminateLsp

open Lsp

module Check : sig
  val ok : pp:(Format.formatter -> 'e -> unit) -> ('a, 'e) result -> 'a

  val ok_json : pp:('a -> Yojson.Safe.t) -> ('b, 'a) result -> 'b

  val ok_s : ('b, string) result -> 'b

  val json : ('a -> Yojson.Safe.t) -> 'a Alcotest.testable
end

type t

val test :
  name:string ->
  ?workspace:string ->
  (t -> client_channel -> server_channel -> unit) ->
  Omnomnom.Tests.test Omnomnom.Tests.tree

type some_request = Request : 'a Server_request.t -> some_request

(** Assert the server sent a request matching a predicate, and return its result. *)
val get_request : t -> (some_request -> 'a option) -> 'a

(** Assert the server sent a notification matching a predicate, and return its result. *)
val get_notification : t -> (Server_notification.t -> 'a option) -> 'a

(** Open a file relative to the current workspace. *)
val open_file : t -> string -> Types.DocumentUri.t

val range : int -> int -> int -> int -> Types.Range.t
