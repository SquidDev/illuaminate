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

(** Create an immutable array with a single element. *)
val singleton : 'a -> 'a t

(** Append two lists together. *)
val append : 'a t -> 'a t -> 'a t

(** {1 Reading immutable arrays} *)

(** Determine if the array is empty. *)
val is_empty : 'a t -> bool

(** Return the length (number of elements) of the given array. *)
external length : 'a t -> int = "%array_length"

(** [get a n] returns the element number [n] of array [a]. *)
external get : 'a t -> int -> 'a = "%array_safe_get"

(** Get the first element in this array. *)
val first : 'a t -> 'a

(** Get the last element in this array. *)
val last : 'a t -> 'a

(** {1 Operating on immutable arrays} *)

(** Perform a left fold over this array. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Filter an immutable array. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Find the first item in an array matching a predicate, transforming it to an alternative type. *)
val find_map : ('a -> 'b option) -> 'a t -> 'b option

(** Iterate over each item in the array. *)
val iter : ('a -> unit) -> 'a t -> unit

(** {1 Updating immutable arrays} *)

(** Set an element in this array, returning the new version of it. *)
val set : 'a t -> int -> 'a -> 'a t

(** Prepend an item onto this array. *)
val push_first : 'a -> 'a t -> 'a t

(** Prepend an item onto this array. *)
val push_last : 'a t -> 'a -> 'a t
