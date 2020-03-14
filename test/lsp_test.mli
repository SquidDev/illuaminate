include module type of IlluaminateLsp

open Lsp.Types
open Lsp

module Check : sig
  val ok : pp:(Format.formatter -> 'e -> unit) -> ('a, 'e) result -> 'a

  val ok_json : pp:('a -> Yojson.Safe.t) -> ('b, 'a) result -> 'b

  val ok_s : ('b, string) result -> 'b

  val ok_response : ('b, Jsonrpc.Response.Error.t) result -> 'b
end

module Testable : sig
  include module type of Alcotest

  val json : ('a -> Yojson.Safe.t) -> 'a Alcotest.testable

  val locations :
    Location.t Alcotest.testable ->
    LocationLink.t Alcotest.testable ->
    Locations.t Alcotest.testable

  val diagnostic : Diagnostic.t testable

  val location : Location.t testable

  val location_link : LocationLink.t testable

  val document_highlight : DocumentHighlight.t testable
end

type t

val test :
  name:string -> ?workspace:string -> (t -> unit) -> Omnomnom.Tests.test Omnomnom.Tests.tree

type some_request = Request : 'a Server_request.t -> some_request

(** Assert the server sent a request matching a predicate, and return its result. *)
val get_request : t -> (some_request -> 'a option) -> 'a

(** Assert the server sent a notification matching a predicate, and return its result. *)
val get_notification : t -> (Server_notification.t -> 'a option) -> 'a

(** Open a file relative to the current workspace. *)
val open_file : t -> string -> DocumentUri.t

val range : int -> int -> int -> int -> Range.t

val pos : int -> int -> Position.t

val request : t -> 'a Client_request.t -> ('a, Jsonrpc.Response.Error.t) result

val notify : t -> Client_notification.t -> (unit, string) result
