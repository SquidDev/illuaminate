(** A hash table, where each key is wrapped in a container. The container may be modified and
    queried directly, rather than going through another call to {!S.set}. *)

(** A container for keys. This is similar to the signature given to
    {!Ephemeron.GenHashTable.MakeSeeded}, though with less mutation.

    One may use the built-in {!StrongContainer} and {!WeakContainer} implementations, or some custom
    one (say, if you want some elements of your key to be weak). *)
module type KeyContainer = sig
  (** The key of this container. *)
  type t

  (** The actual container. *)
  type 'a container

  (** Same as {!Hashtbl.HashType.hash} *)
  val hash : t -> int

  (** Equality predicate used to compare a key with the one in a container. Can return
      {!Ephemeron.GenHashTable.EDead} if the keys in the container are dead *)

  val equal : 'a container -> t -> Ephemeron.GenHashTable.equal

  (** Create a container from an initial key and piece of data. *)
  val create : t -> 'a -> 'a container

  (** Get the key, if alive. *)
  val get_key : 'a container -> t option

  (** Get the value, if alive. *)
  val get_data : 'a container -> 'a option

  (** Set the data of this cell. *)
  val set_data : 'a container -> 'a -> unit

  (** Check if this key is alive. *)
  val check_key : 'a container -> bool

  (** (Optionally) clear this reference cell. *)
  val unset : 'a container -> unit
end

module StrongContainer (M : Hashtbl.HashedType) : KeyContainer with type t = M.t

module WeakContainer (M : Hashtbl.HashedType) : KeyContainer with type t = M.t

module type S = sig
  type key

  type 'a container

  type 'a t

  (** Construct a new hash table. *)
  val create : int -> 'a t

  (** Find a key within the map. *)
  val find : 'a t -> key -> 'a container option

  (** Add or insert a key. *)
  val insert : 'a t -> key -> 'a -> 'a container
end

(** Construct a new open-hash table. *)
module Make (M : KeyContainer) : S with type key = M.t and type 'a container = 'a M.container

val strong :
  ?hash:('a -> int) -> eq:('a -> 'a -> bool) -> unit -> (module KeyContainer with type t = 'a)

val weak :
  ?hash:('a -> int) -> eq:('a -> 'a -> bool) -> unit -> (module KeyContainer with type t = 'a)
