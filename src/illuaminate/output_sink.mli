(** A generic type that can be used for outputting strings.

    Like {!Format.formatter}, this provides a generic interface for outputting strings. However,
    this sacrifices the flexibility of {!Format} to provide a simpler and more efficient interface.
*)

type t
type 'a pp = t -> 'a -> unit

(** {1 Constructing a sink} *)

(** Construct a printer from an output stream, then apply some action with it.

    Writes will be automatically buffered to the supplied output channel, and flushed when
    sufficient bytes are written, or when the inner function completes. *)
val with_output_stream : out_channel -> (t -> unit) -> unit

(** Construct a printer from a buffer. *)
val of_buffer : Buffer.t -> t

(** Construct a printer from a formatter. *)
val of_formatter : Format.formatter -> t

(** [with_to_str fn] returns the result of printing with [fn] as a string. *)
val with_to_str : (t -> unit) -> string

(** {1 Outputting strings} *)

(** Write a string to this printer. *)
val write : t -> string -> unit

(** Write a substring to this printer. *)
val write_substring : t -> string -> int -> int -> unit

(** Write a formatted string to this printer. *)
val printf : t -> ('a, unit, string, unit) format4 -> 'a
