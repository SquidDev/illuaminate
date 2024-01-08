(** Immutable arrays.

    This just provides a wrapper for the built-in {!Array} module, removing any functions which
    modify the array. *)

type 'a t

(** The empty array. *)
val empty : 'a t

(** Pretty print an array. *)
val pp : 'a Fmt.t -> 'a t Fmt.t

(** {1 Creation} *)

(** Create an immutable array from an array.

    The original array should not be modified after calling this function. *)
external of_array : 'a array -> 'a t = "%identity"

(** Create an immutable array from a list. *)
val of_list : 'a list -> 'a t

(** Create an immutable array from a reversed list. *)
val of_rev_list : 'a list -> 'a t

(** {1 Operations} *)

(** Return the length (number of elements) of the given array. *)
external length : 'a t -> int = "%array_length"

(** [get a n] returns the element number [n] of array [a]. *)
external get : 'a t -> int -> 'a = "%array_safe_get"

(** Set an element in this array, returning the new version of it. *)
val set : 'a t -> int -> 'a -> 'a t

(** Perform a left fold over this array. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Iterate over each item in the array. *)
val iter : ('a -> unit) -> 'a t -> unit
