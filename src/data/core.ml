module IntMap = Map.Make (Int)

type 'a containers = ..

type values = ..

type providers = ..

type ('store, 'k, 'v) provider =
  | Tracing of ('store -> 'k -> 'v)
  | Oracle of ('k -> 'v option -> 'v)

let src = Logs.Src.create ~doc:"A basic system for incremental computation" __MODULE__

module Log = (val Logs.src_log src)

module type Key = sig
  type key

  type value

  type store

  val id : int

  val name : string

  module Container : Contained_tbl.KeyContainer with type t = key

  val pp : Format.formatter -> key -> unit

  val eq : value -> value -> bool

  val factory : (store, key, value) provider option

  type 'a containers += Container of 'a Container.container

  type values += Value of value

  type providers += Provider of (store, key, value) provider
end

module BoxedKey = struct
  type t = Mk : (module Key with type key = 'k) * 'k -> t

  type 'a container =
    { key : (module Key);
      container : 'a containers
    }

  let hash (Mk ((module Key), k)) = Key.Container.hash k

  let pp out (Mk ((module Key), k)) = Format.fprintf out "%s(%a)" Key.name Key.pp k

  let pp_container out { key = (module Key); container } =
    match container with
    | Key.Container c -> (
        let key = Key.Container.get_key c in
        match key with
        | Some key -> Format.fprintf out "%s(%a)" Key.name Key.pp key
        | None -> Format.fprintf out "%s(missing)" Key.name )
    | _ -> Format.fprintf out "%s(unknown container)" Key.name

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
    checked_at : version;
    contents : values;
    trace : trace
  }

type t =
  { mutable version : int;
    results : result KeyTbl.t;
    providers : providers IntMap.t
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
    ?pp:(Format.formatter -> 'k -> unit) ->
    ?container_k:(module Contained_tbl.KeyContainer with type t = 'k) ->
    ?eq_v:('v -> 'v -> bool) ->
    'f ->
    ('k, 'v) t

  let next_id = ref 0

  let default_pp out _ = Format.pp_print_string out "?"

  let factory (type k v) : (k, v, (context, k, v) provider option) factory =
   fun ~name ?pp ?container_k ?eq_v factory ->
    let id = !next_id in
    incr next_id;
    let container =
      match container_k with
      | Some k -> k
      | None -> Contained_tbl.strong ~eq:( == ) ()
    in
    let eq = Option.value ~default:( == ) eq_v in
    let pp = Option.value ~default:default_pp pp in
    ( module struct
      type key = k

      type value = v

      type nonrec store = context

      let id = id

      let name = name

      module Container = (val container)

      let eq = eq

      let pp = pp

      let factory = factory

      type 'a containers += Container of 'a Container.container

      type values += Value of value

      type providers += Provider of (store, key, value) provider
    end )

  let key ~name ?pp ?container_k ?eq_v f = factory ~name ?pp ?container_k ?eq_v (Some (Tracing f))

  let oracle ~name ?pp ?container_k ?eq_v f = factory ~name ?pp ?container_k ?eq_v (Some (Oracle f))

  let deferred ~name ?pp ?container_k ?eq_v () = factory ~name ?pp ?container_k ?eq_v None
end

module Builder = struct
  type t = providers IntMap.t

  let empty = IntMap.empty

  let add_deferred (type k v) ((module Key) : (k, v) Key.t) provider providers =
    if Option.is_some Key.factory then
      invalid_arg (Printf.sprintf "Builder.add_deferred : %s is not a deferred key" Key.name);
    if IntMap.mem Key.id providers then
      invalid_arg (Printf.sprintf "Builder.add_deferred : Multiple factories for %s" Key.name);
    IntMap.add Key.id (Key.Provider provider) providers

  let key key f providers = add_deferred key (Tracing f) providers

  let oracle key f providers = add_deferred key (Oracle f) providers

  let build providers = { version = 0; providers; results = KeyTbl.create 32 }
end

let refresh x = x.version <- x.version + 1

let with_context ~store ~tracing f =
  let t = { tracing; trace = []; active = true; store } in
  let res = f t in
  t.active <- false;
  (res, t.trace)

(** Build a single term and return the result. This does not do dependency checking, nor does it
    update the store. *)
let build_result store (Mk ((module K), key) as bkey : BoxedKey.t) previous : result =
  let start = Sys.time () in
  Log.debug (fun f -> f "Building %a" BoxedKey.pp bkey);
  let version = store.version in
  let contents, trace =
    let provider =
      match K.factory with
      | Some f -> f
      | None -> (
        match IntMap.find_opt K.id store.providers with
        | None -> Printf.sprintf "No oracle for %S" K.name |> invalid_arg
        | Some (K.Provider f) -> f
        | Some _ -> Printf.sprintf "Incorrect oracle for %S" K.name |> failwith )
    in
    match provider with
    | Tracing f ->
        (* The use of magic here is terribly ugly, but it's not clear how else to convert from the
           abstract store to the concrete one without introducing another open union. There's only
           one possible type here, so our use of magic shouldn't ever be incorrect. *)
        let res, trace = with_context ~store ~tracing:true (fun t -> f (Obj.magic t) key) in
        (res, Depends trace)
    | Oracle f ->
        let previous =
          match previous with
          | Some ({ contents = K.Value c; _ } : result) -> Some c
          | _ -> None
        in
        (f key previous, Oracle)
  in
  let delta = Sys.time () -. start in
  let log kind = Log.info (fun f -> f "Finished %a in %.2f (%s)" BoxedKey.pp bkey delta kind) in
  let result =
    { checked_at = version;
      changed_at = version;
      built_at = version;
      contents = K.Value contents;
      trace
    }
  in
  match previous with
  | Some { contents = K.Value contents2; changed_at; _ } when K.eq contents contents2 ->
      (* If our value hasn't'changed, then don't update the changed_at timestamp. *)
      log "no change"; { result with changed_at }
  | Some _ -> log "changed"; result
  | None -> log "new"; result

(** Determine if this step can be skipped (namely, all its dependencies haven't changed). *)
let skip source build : result -> bool = function
  | { trace = Oracle; _ } -> false
  | { built_at; trace = Depends trace; _ } ->
      let skip container =
        match BoxedKey.get_key container with
        | None -> false
        | Some key ->
            let _, result = build key (Some container) in
            if result.changed_at <= built_at then true
            else (
              Log.debug (fun f ->
                  f "Rebuilding: %a - %a was built more recently (%d > %d)" BoxedKey.pp source
                    BoxedKey.pp_container container result.changed_at built_at);
              false )
      in
      List.for_all skip trace

(** Get a result, rebuilding it if required. *)
let rec rebuild ({ version; results; _ } as store) k container =
  match container with
  | Some container -> (
    match BoxedKey.get_data container with
    | Some ({ checked_at; _ } as result) when checked_at = version ->
        (* If we already checked this version, then don't check dependencies. *)
        (container, result)
    | Some result when skip k (rebuild store) result ->
        (* If all our dependencies are up-to-date, then continue. *)
        Log.debug (fun f -> f "Skipping %a" BoxedKey.pp k);
        (container, { result with checked_at = version })
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

let compute f store = with_context ~store ~tracing:false f |> fst
