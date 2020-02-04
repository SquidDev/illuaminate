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
    ?attributes:(string * string) list ->
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

  (** Emit a document, including the DOCTYPE header. *)
  val emit_doc : Format.formatter -> node -> unit
end

module Default : module type of Make (struct
  type event_handler = |
end)
