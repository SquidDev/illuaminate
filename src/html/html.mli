(** Functions for building and emitting HTML nodes. This is designed to be used in tandem with
    Reason's JSX syntax and the {!Jsx} pre-processor. *)

(** A HTML syntax tree and functions for working with them.

    This functor is parameterised by the type of event handler this tree uses. The {!Default} module
    uses an empty variant type, however other modules (such as our website) use a js_of_ocaml event
    handler. *)
module Make (X : sig
  type event_handler
end) : sig
  type event_handler = X.event_handler

  (** The type of HTML nodes. *)
  type node = private
    | Element of
        { tag : string;
          attributes : (string * string) list;
          events : (string * event_handler) list;
          children : node list
        }
    | Text of string
    | Raw of string
    | Nil
    | Many of node list

  (** Create an element with a specific tag, attributes, event handlers and children. Indented to be
      used from Reason via the JSX processor. *)
  val create_node :
    tag:string ->
    ?attributes:(string * string option) list ->
    ?events:(string * event_handler) list ->
    ?children:node list ->
    unit ->
    node

  (** An empty node. *)
  val nil : node

  (** A node representing a string. This will be escaped when emitted. *)
  val str : string -> node

  (** A node representing raw HTML contents. *)
  val raw : string -> node

  (** Merge multiple nodes into one. Easier than flattening lists. *)
  val many : node list -> node

  (** Emit a node. *)
  val emit : Format.formatter -> node -> unit

  (** Emit a node, with additional whitespace. *)
  val emit_pretty : Format.formatter -> node -> unit

  (** Emit a document, including the DOCTYPE header. *)
  val emit_doc : Format.formatter -> node -> unit
end

(** The default HTML tree. *)
module Default : module type of Make (struct
  type event_handler = |
end)
