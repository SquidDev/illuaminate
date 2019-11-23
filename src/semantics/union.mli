(** An optimised mutable structure for handling objects which will later be unified. *)

(** A point holding a specific value which may be later unified. *)
type 'a t

(** Construct a new point with the given value. *)
val make : 'a -> 'a t

(** Get the value of this point and all equivalent ones. *)
val get : 'a t -> 'a

(** Set the value of this point and all equivalent ones. *)
val set : 'a t -> 'a -> unit

(** Unify two points.

    If the points are not already equivalent, then the given function will be applied to compute the
    new value of the equivalency class from the two previous ones. *)
val union_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> unit

(** Perform a right-biased union of two equivalency classes. *)
val union : 'a t -> 'a t -> unit
