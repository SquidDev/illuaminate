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

  type 'a t := 'a testable

  val yojson : Yojson.Safe.t t

  val json : ('a -> Yojson.Safe.t) -> 'a t

  val locations : Location.t t -> LocationLink.t t -> Locations.t t

  val diagnostic : Diagnostic.t t

  val location : Location.t t

  val location_link : LocationLink.t t

  val document_highlight : DocumentHighlight.t t

  val position : Position.t t

  val range : Range.t t

  val workspace_edit : WorkspaceEdit.t t

  val command :
    ?title:string t -> ?command:string t -> ?arguments:Yojson.Safe.t t -> unit -> Command.t testable

  val code_action :
    ?title:string t -> ?diagnostic:Diagnostic.t t -> ?command:Command.t t -> unit -> CodeAction.t t

  val code_action_result : Command.t t -> CodeAction.t t -> CodeActionResult.t testable
end

type t

val test : name:string -> ?workspace:string -> (t -> unit) -> Omnomnom.Tests.tests

type some_request = Request : 'a Server_request.t -> some_request

(** Assert the server sent a request matching a predicate, and return its result. *)
val get_request : t -> (some_request -> 'a option) -> 'a

(** Assert the server sent a notification matching a predicate, and return its result. *)
val get_notification : t -> (Server_notification.t -> 'a option) -> 'a

(** Read the contents of a file. *)
val read_file : t -> string -> string

(** Open a file relative to the current workspace. *)
val open_file : t -> string -> DocumentUri.t

(** Get the contents of a file. *)
val contents : t -> DocumentUri.t -> string

(** Apply a {!WorkspaceEdit} to the current documents. *)
val apply_change : t -> WorkspaceEdit.t -> unit

(** Wait for a request and apply it. *)
val apply_edits : t -> unit

val range : int -> int -> int -> int -> Range.t

val pos : int -> int -> Position.t

val request : t -> 'a Client_request.t -> ('a, Jsonrpc.Response.Error.t) result

val notify : t -> Client_notification.t -> (unit, string) result
