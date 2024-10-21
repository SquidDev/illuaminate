(** Functions for building and emitting HTML nodes. This is designed to be used in tandem with
    Reason's JSX syntax and the {!Jsx} pre-processor. *)

(** {1 Nodes} *)

(** The type of HTML nodes. *)
type 'ev node = private
  | Element of
      { tag : string;
        attributes : (string * string) list;
        events : (string * 'ev) list;
        children : 'ev node list
      }
  | Text of string
  | Raw of string
  | Nil
  | Many of 'ev node list

(** Create an element with a specific tag, attributes, event handlers and children. Indented to be
    used from Reason via the JSX processor. *)
val create_node :
  tag:string ->
  ?attributes:(string * string option) list ->
  ?events:(string * 'ev) list ->
  ?children:'ev node list ->
  unit ->
  'ev node

(** An empty node. *)
val nil : 'ev node

(** A node representing a string. This will be escaped when emitted. *)
val str : string -> 'ev node

(** A node representing raw HTML contents. *)
val raw : string -> 'ev node

(** Merge multiple nodes into one. Easier than flattening lists. *)
val many : 'ev node list -> 'ev node

(** {1 Basic nodes} *)

type no_events = |

(** A basic {! node} that has no event handlers. *)
type node_ = no_events node

(** {1 Emitting basic nodes} *)

(** Emit a node. *)
val emit : Output_sink.t -> node_ -> unit

(** Emit a document, including the DOCTYPE header. *)
val emit_doc : Output_sink.t -> node_ -> unit

(** Emit a node, with additional whitespace. *)
val emit_pretty : Format.formatter -> node_ -> unit

(** {1 Utility functions for emitting HTML} *)

(** Emit a list of attributes. This is assumed to appear immediately after the tag, and so should be
    printed using [printf "%s%a" tag attributes] *)
val emit_attrs : Output_sink.t -> (string * string) list -> unit
