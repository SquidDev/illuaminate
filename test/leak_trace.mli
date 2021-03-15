type allocation = private
  { n_samples : int;
    size : int;
    callstack : Printexc.raw_backtrace
  }

val pause : unit -> unit

val resume : unit -> unit

val run : (unit -> 'a) -> 'a * allocation list

val format_bt : ?n:int -> Printexc.raw_backtrace -> string
