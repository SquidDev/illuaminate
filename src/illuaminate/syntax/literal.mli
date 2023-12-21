(** Functions for parsing and displaying Lua literals. *)

(** Functions for parsing and displaying Lua numbers. *)
module Number : sig
  (** A Lua number, either an integer or a string. *)
  type t =
    | Int of Int64.t
    | Float of float

  (** [parse num] attempts to parse a number from string [x]. *)
  val parse : string -> (t, unit) result
end

(** Functions for parsing and displaying Lua strings. *)
module String : sig
  (** A component in a string, such as a span of characters or an escape sequence. *)
  type component =
    | Quote of char  (** A ['] or ["\""] quote.*)
    | Segment  (** A span of normal characters string, without any quotes or escape sequences. *)
    | Escape of char
        (** A single character escape sequence, such as [\n]. The character corresponds to the value
            in the string. *)
    | Decimal_escape of char  (** A decimal escape sequence. ([\65]). *)
    | Hex_escape of char  (** A hexadecimal escape sequence. ([\x41]). *)
    | Unicode_escape of Uchar.t  (** A unicode escape sequence. ([\u0041])*)
    | Unknown_escape of char  (** An unknown escape sequence. *)
    | Zap  (** A "zap" escape sequence, which consumes all whitespace. ([\z])*)

  (** A component in a string (see {!string_component}) along with its position. *)
  type spanned_component =
    { contents : component;  (** The underlying component. *)
      start : int;  (** The start of this span. *)
      length : int  (** The length of this span. *)
    }

  (** A Lua string. *)
  type t =
    | Short_string of spanned_component list  (** A short string. *)
    | Long_string of string  (** A long string. *)

  (** Parse a string literal into the constituent components. *)
  val parse : string -> (t, unit) result

  (** Parse a string literal. *)
  val parse_value : string -> (string, unit) result
end
