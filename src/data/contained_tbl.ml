module type KeyContainer = sig
  type t

  type 'a container

  val hash : t -> int

  val equal : 'a container -> t -> Ephemeron.GenHashTable.equal

  val create : t -> 'a -> 'a container

  val get_key : 'a container -> t option

  val get_data : 'a container -> 'a option

  val set_data : 'a container -> 'a -> unit

  val check_key : 'a container -> bool

  val unset : 'a container -> unit
end

module StrongContainer (M : Hashtbl.HashedType) : KeyContainer with type t = M.t = struct
  type t = M.t

  type 'a container =
    { key : t;
      mutable value : 'a
    }

  let hash = M.hash

  let equal { key; _ } key2 : Ephemeron.GenHashTable.equal =
    if M.equal key key2 then ETrue else EFalse

  let create key value = { key; value }

  let get_key { key; _ } = Some key

  let get_data { value; _ } = Some value

  let set_data c v = c.value <- v

  let check_key _ = true

  let unset _ = ()
end

module WeakContainer (M : Hashtbl.HashedType) : KeyContainer with type t = M.t = struct
  open Ephemeron.K1

  type 'a container = (M.t, 'a) t

  type t = M.t

  let create k d =
    let c = create () in
    set_data c d; set_key c k; c

  let hash = M.hash

  let equal c k : Ephemeron.GenHashTable.equal =
    match get_key c with
    | None -> EDead
    | Some k' -> if M.equal k k' then ETrue else EFalse

  let get_data = get_data

  let get_key = get_key

  let set_data c d = unset_data c; set_data c d

  let check_key = check_key

  let unset c = unset_data c; unset_key c
end

module type S = sig
  type key

  type 'a container

  type 'a t

  val create : int -> 'a t

  val find : 'a t -> key -> 'a container option

  val insert : 'a t -> key -> 'a -> 'a container

  val pp :
    key:(Format.formatter -> key -> unit) ->
    value:(Format.formatter -> 'a -> unit) ->
    all:bool ->
    Format.formatter ->
    'a t ->
    unit
end

module Make (H : KeyContainer) = struct
  type key = H.t

  type 'a container = 'a H.container

  type 'a bucketlist =
    | Empty
    | Cons of int (* hash of the key *) * 'a container * 'a bucketlist

  type 'a t =
    { mutable size : int;  (** Number of entries *)
      mutable data : 'a bucketlist array;  (** The buckets *)
      initial_size : int  (** Initial array size *)
    }

  let rec power_2_above x n =
    if x >= n then x else if x * 2 > Sys.max_array_length then x else power_2_above (x * 2) n

  let create initial_size =
    let s = power_2_above 16 initial_size in
    { initial_size = s; size = 0; data = Array.make s Empty }

  let key_index h hkey = hkey land (Array.length h.data - 1)

  let clean h =
    (* Perform a GC just before cleaning our store. This ensures we don't grow too often.

       This is incredibly ugly, but thankfully we don't need to do it very often. Perhaps a better
       solution would be to overwrite dead cells, rather than skipping them. *)
    Gc.full_major ();
    let rec do_bucket = function
      | Empty -> Empty
      | Cons (_, c, rest) when not (H.check_key c) ->
          h.size <- h.size - 1;
          do_bucket rest
      | Cons (hkey, c, rest) -> Cons (hkey, c, do_bucket rest)
    in
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      d.(i) <- do_bucket d.(i)
    done

  let resize h =
    let odata = h.data in
    let osize = Array.length odata in
    let nsize = osize * 2 in
    clean h;
    if nsize < Sys.max_array_length && h.size >= osize lsr 1 then (
      let ndata = Array.make nsize Empty in
      h.data <- ndata;
      (* so that key_index sees the new bucket count *)
      let rec insert_bucket = function
        | Empty -> ()
        | Cons (hkey, data, rest) ->
            insert_bucket rest;
            (* preserve original order of elements *)
            let nidx = key_index h hkey in
            ndata.(nidx) <- Cons (hkey, data, ndata.(nidx))
      in
      for i = 0 to osize - 1 do
        insert_bucket odata.(i)
      done )

  let rec find_rec_opt key hkey = function
    | Empty -> None
    | Cons (hk, c, rest) when hkey = hk -> (
      match H.equal c key with
      | ETrue -> Some c
      | EFalse | EDead -> find_rec_opt key hkey rest )
    | Cons (_, _, rest) -> find_rec_opt key hkey rest

  let find h key =
    let hkey = H.hash key in
    find_rec_opt key hkey h.data.(key_index h hkey)

  let insert h key info =
    let hkey = H.hash key in
    let i = key_index h hkey in
    let l = h.data.(i) in
    let rec replace_bucket = function
      | Empty ->
          let c = H.create key info in
          h.data.(i) <- Cons (hkey, c, l);
          h.size <- h.size + 1;
          if h.size > Array.length h.data lsl 1 then resize h;
          c
      | Cons (hk, c, next) when hkey = hk -> (
        match H.equal c key with
        | ETrue -> H.set_data c info; c
        | EFalse | EDead -> replace_bucket next )
      | Cons (_, _, next) -> replace_bucket next
    in
    replace_bucket l

  let pp ~key ~value ~all out { data; _ } =
    let rec print_one = function
      | Empty -> ()
      | Cons (_, c, next) ->
          ( match (H.get_key c, H.get_data c) with
          | Some k, Some v -> Format.fprintf out "%a => %a@," key k value v
          | Some k, None -> if all then Format.fprintf out "%a => _@," key k
          | None, Some v -> if all then Format.fprintf out "_ => %a@," value v
          | None, None -> if all then Format.fprintf out "_ => _@," );
          print_one next
    in
    Format.fprintf out "@[<v 2>{[%d]@," (Array.length data);
    Array.iter print_one data;
    Format.fprintf out "@] }"
end

let strong (type k) ?(hash = Hashtbl.hash) ~eq () =
  ( module StrongContainer (struct
    type t = k

    let equal = eq

    let hash = hash
  end) : KeyContainer
    with type t = k )

let weak (type k) ?(hash = Hashtbl.hash) ~eq () =
  ( module WeakContainer (struct
    type t = k

    let equal = eq

    let hash = hash
  end) : KeyContainer
    with type t = k )
