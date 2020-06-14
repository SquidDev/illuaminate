(** A list of actions which can be run in a loop. *)
type ('a, 'b) action

(** A single action. This consumes some state and produces a new piece of state. *)
val action : name:string -> ('a -> 'b) -> ('a, 'b) action

(** Compose two action sequences together. *)
val ( >-> ) : ('a, 'b) action -> ('b, 'c) action -> ('a, 'c) action

(** Run our actions in a loop [n] times, with some initial state. If we use leak than [threshold]
    bytes of memory, then error. *)
val run : ?error:('a -> unit) -> ?n:int -> ?threshold:int -> init:'a -> ('a, 'a) action -> unit

(** The same as {!run}, but with no external state. *)
val run_unit : ?error:(unit -> unit) -> ?n:int -> ?threshold:int -> (unit, unit) action -> unit
