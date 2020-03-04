module IntMap = Map.Make (Int)

type 'a containers = ..

type values = ..

type oracles = ..

let src = Logs.Src.create ~doc:"A basic system for incremental computation" __MODULE__

module Log = (val Logs.src_log src)

module type Key = sig
  type key

  type value

  type store

  val id : int

  val name : string

  module Container : Contained_tbl.KeyContainer with type t = key

  val eq : value -> value -> bool

  val factory : (store -> key -> value) option

  type 'a containers += Container of 'a Container.container

  type values += Value of value

  type oracles += Oracle of (key -> value)
end

module BoxedKey = struct
  type t = Mk : (module Key with type key = 'k) * 'k -> t

  type 'a container =
    { key : (module Key);
      container : 'a containers
    }

  let name { key = (module Key); _ } = Key.name

  let hash (Mk ((module Key), k)) = Key.Container.hash k

  let equal { container; _ } (Mk ((module Key), k)) =
    match container with
    | Key.Container c -> Key.Container.equal c k
    | _ -> EFalse

  let create (Mk ((module Key), k)) value =
    { key = (module Key); container = Key.Container (Key.Container.create k value) }

  let get_key { key = (module Key); container } =
    match container with
    | Key.Container c -> Key.Container.get_key c |> Option.map (fun k -> Mk ((module Key), k))
    | _ -> failwith "Key mismatch!"

  let get_data { key = (module Key); container } =
    match container with
    | Key.Container c -> Key.Container.get_data c
    | _ -> failwith "Key mismatch!"

  let unset { key = (module Key); container } =
    match container with
    | Key.Container c -> Key.Container.unset c
    | _ -> failwith "Key mismatch!"

  let set_data { key = (module Key); container } value =
    match container with
    | Key.Container c -> Key.Container.set_data c value
    | _ -> failwith "Key mismatch!"

  let check_key { key = (module Key); container } =
    match container with
    | Key.Container c -> Key.Container.check_key c
    | _ -> failwith "Key mismatch!"
end

module KeyTbl = Contained_tbl.Make (BoxedKey)

(** An id which represents some key. This is used in place of a {!boxed_key}, as it does not contain
    any garbage. *)
type version = int

type trace =
  | Depends of result KeyTbl.container list
  | Oracle

and result =
  { changed_at : version;
    built_at : version;
    contents : values;
    trace : trace
  }

type t =
  { mutable version : int;
    results : result KeyTbl.t;
    oracles : oracles IntMap.t
  }

type context =
  { store : t;
    tracing : bool;
    mutable active : bool;
    mutable trace : result KeyTbl.container list
  }

module Key = struct
  type ('k, 'v) t = (module Key with type key = 'k and type value = 'v and type store = context)

  let name (type k v) ((module Key) : (k, v) t) = Key.name

  (** Extract the value from {!values}. Raises {!Invalid_argument} if there is a type mismatch. *)
  let value (type k v) ((module Key) : (k, v) t) v : v =
    match v with
    | Key.Value v -> v
    | _ -> Printf.sprintf "Mismatch for key %S" Key.name |> invalid_arg

  type ('k, 'v, 'f) factory =
    name:string ->
    ?container_k:(module Contained_tbl.KeyContainer with type t = 'k) ->
    ?eq_v:('v -> 'v -> bool) ->
    'f ->
    ('k, 'v) t

  let next_id = ref 0

  let factory (type k v) : (k, v, (context -> k -> v) option) factory =
   fun ~name ?container_k ?eq_v factory ->
    let id = !next_id in
    incr next_id;
    let container =
      match container_k with
      | Some k -> k
      | None ->
          ( module Contained_tbl.StrongContainer (struct
            type t = k

            let equal = ( == )

            let hash = Hashtbl.hash
          end) )
    in
    let eq = Option.value ~default:( == ) eq_v in
    ( module struct
      type key = k

      type value = v

      type nonrec store = context

      let id = id

      let name = name

      module Container = (val container)

      let eq = eq

      let factory = factory

      type 'a containers += Container of 'a Container.container

      type values += Value of value

      type oracles += Oracle of (key -> value)
    end )

  let key ~name ?container_k ?eq_v f = factory ~name ?container_k ?eq_v (Some f)

  let oracle ~name ?container_k ?eq_v () = factory ~name ?container_k ?eq_v None
end

module Builder = struct
  type t = oracles IntMap.t

  let empty = IntMap.empty

  let oracle (type k v) ((module Key) : (k, v) Key.t) oracle oracles =
    if IntMap.mem Key.id oracles then invalid_arg ("Duplicate oracle for " ^ Key.name);
    IntMap.add Key.id (Key.Oracle oracle) oracles

  let build oracles = { version = 0; oracles; results = KeyTbl.create 32 }
end

let refresh x = x.version <- x.version + 1

(** Build a single term and return the result. This does not do dependency checking, nor does it
    update the store. *)
let build_result store (Mk ((module K), key) : BoxedKey.t) previous : result =
  let start = Sys.time () in
  Log.info (fun f -> f "Building %s" K.name);
  let version = store.version in
  let contents, trace =
    match K.factory with
    | Some f ->
        (* The use of magic here is terribly ugly, but it's not clear how else to convert from the
           abstract store to the concrete one without introducing another open union. There's only
           one possible type here, so our use of magic shouldn't ever be incorrect. *)
        let t = { tracing = true; trace = []; active = true; store } in
        let res = f (Obj.magic t) key in
        t.active <- false;
        (res, Depends t.trace)
    | None ->
        let oracle =
          match IntMap.find_opt K.id store.oracles with
          | None -> Printf.sprintf "No oracle for %S" K.name |> invalid_arg
          | Some (K.Oracle o) -> o
          | Some _ -> Printf.sprintf "Incorrect oracle for %S" K.name |> failwith
        in
        (oracle key, Oracle)
  in
  let delta = Sys.time () -. start in
  let log kind = Log.debug (fun f -> f "Finished %s in %.2f (%s)" K.name delta kind) in
  let result = { changed_at = version; built_at = version; contents = K.Value contents; trace } in
  match previous with
  | Some { contents = K.Value contents2; changed_at; _ } when K.eq contents contents2 ->
      (* If our value hasn't'changed, then don't update the changed_at timestamp. *)
      log "no change"; { result with changed_at }
  | Some _ -> log "changed"; result
  | None -> log "new"; result

(** Determine if this step can be skipped (namely, all its dependencies haven't changed). *)
let skip build : result -> bool = function
  | { trace = Oracle; _ } -> false
  | { built_at; trace = Depends trace; _ } ->
      let skip container =
        match BoxedKey.get_key container with
        | None ->
            Log.debug (fun f -> f "Not skipping: %s is unavailable" (BoxedKey.name container));
            false
        | Some key ->
            let _, result = build key (Some container) in
            if result.changed_at <= built_at then true
            else (
              Log.debug (fun f ->
                  f "Skipping: %s was built more recently (%d > %d)" (BoxedKey.name container)
                    result.changed_at built_at);
              false )
      in
      List.for_all skip trace

(** Get a result, rebuilding it if required. *)
let rec rebuild ({ version; results; _ } as store) k container =
  match container with
  | Some container -> (
    match BoxedKey.get_data container with
    | Some ({ built_at; _ } as result) when built_at = version ->
        (* If we rebuilt it this version, then don't check dependencies. *)
        (container, result)
    | Some result when skip (rebuild store) result ->
        (* If all our dependencies are up-to-date, then continue. *)
        Log.debug (fun f ->
            let (Mk ((module K), _)) = k in
            f "Skipping %s" K.name);
        (container, result)
    | previous ->
        (* Rebuild and update the key. *)
        let result = build_result store k previous in
        BoxedKey.set_data container result;
        (container, result) )
  | None ->
      (* Update and insert. We require that the key isn't in the table *)
      assert (KeyTbl.find results k |> Option.is_none);

      let result = build_result store k None in
      let container = KeyTbl.insert results k result in
      (container, result)

let rebuild store key = KeyTbl.find store.results key |> rebuild store key

let get (type k v) store (key : (k, v) Key.t) (k : k) : v =
  let module K = (val key) in
  let _, result = rebuild store (BoxedKey.Mk ((module K), k)) in
  Key.value key result.contents

let need (type k v) ({ store; _ } as context) (key : (k, v) Key.t) (k : k) : v =
  if not context.active then failwith "Calling need after computing the result.";
  let module K = (val key) in
  let container, result = rebuild store (BoxedKey.Mk ((module K), k)) in
  if context.tracing then context.trace <- container :: context.trace;
  Key.value key result.contents

let compute f store =
  let t = { tracing = false; trace = []; active = true; store } in
  let res = f t in
  t.active <- false;
  res
