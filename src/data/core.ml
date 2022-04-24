module IntMap = Map.Make (Int)

type 'a containers = ..
type values = ..
type providers = ..

let src = Logs.Src.create ~doc:"A basic system for incremental computation" __MODULE__

module Log = (val Logs.src_log src)

module KeyInfo = struct
  type 'a reason =
    | Absent
    | DependencyChange of 'a
    | Recompute of 'a

  type change =
    | NoChange
    | RecomputeChange
    | RecomputeSame

  type 'a result =
    { value : 'a;
      changed : change
    }
end

type ('s, 'k, 'v) provider = 's -> 'k -> 'v KeyInfo.reason -> 'v KeyInfo.result

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
        | None -> Format.fprintf out "%s(missing)" Key.name)
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

type trace = Depends of result KeyTbl.container list [@@unboxed]

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
  include KeyInfo

  type ('k, 'v) t = (module Key with type key = 'k and type value = 'v and type store = context)

  let name (type k v) ((module Key) : (k, v) t) = Key.name

  let pp (type k v) ((module Key) : (k, v) t) out (k : k) =
    Format.fprintf out "%s(%a)" Key.name Key.pp k

  (** Extract the value from {!values}. Raises {!Invalid_argument} if there is a type mismatch. *)
  let value (type k v) ((module Key) : (k, v) t) v : v =
    match v with
    | Key.Value v -> v
    | _ -> Printf.sprintf "Mismatch for key %S" Key.name |> invalid_arg

  type ('k, 'v, 'f) factory =
    name:string ->
    ?pp:(Format.formatter -> 'k -> unit) ->
    ?container:(module Contained_tbl.KeyContainer with type t = 'k) ->
    'f ->
    ('k, 'v) t

  let next_id = ref 0
  let default_pp out _ = Format.pp_print_string out "?"
  let fail_eq (_ : 'a) (_ : 'a) : bool = failwith "Cannot compare non-deferred keys"

  let factory (type k v) ~eq : (k, v, (context, k, v) provider option) factory =
   fun ~name ?pp ?container factory ->
    let id = !next_id in
    incr next_id;
    let container =
      match container with
      | Some k -> k
      | None -> Contained_tbl.strong ~eq:( == ) ()
    in
    let pp = Option.value ~default:default_pp pp in
    (module struct
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
    end)

  let builtin ~name ?pp ?container f = factory ~name ~eq:fail_eq ?pp ?container (Some f)

  let mk_oracle ~eq f _ key = function
    | Absent -> { value = f key None; changed = RecomputeChange }
    | Recompute old | DependencyChange old ->
        let value = f key (Some old) in
        { value; changed = (if eq old value then RecomputeSame else RecomputeChange) }

  let oracle ?(eq = ( == )) ~name ?pp ?container f =
    factory ~name ~eq:fail_eq ?pp ?container (Some (mk_oracle ~eq f))

  let mk_key ~eq f store key = function
    | Absent -> { value = f store key; changed = RecomputeChange }
    | Recompute value -> { value; changed = NoChange }
    | DependencyChange old ->
        let value = f store key in
        { value; changed = (if eq old value then RecomputeSame else RecomputeChange) }

  let key ?(eq = ( == )) ~name ?pp ?container f =
    factory ~name ~eq:fail_eq ?pp ?container (Some (mk_key ~eq f))

  let deferred ?(eq = ( == )) ~name ?pp ?container () = factory ~name ~eq ?pp ?container None
end

module Builder = struct
  type t = providers IntMap.t

  let empty = IntMap.empty

  let builtin (type k v) ((module Key) : (k, v) Key.t) provider providers =
    if Option.is_some Key.factory then
      invalid_arg (Printf.sprintf "Builder.add_deferred : %s is not a deferred key" Key.name);
    if IntMap.mem Key.id providers then
      invalid_arg (Printf.sprintf "Builder.add_deferred : Multiple factories for %s" Key.name);
    IntMap.add Key.id (Key.Provider provider) providers

  let get_eq (type k v) ((module K) : (k, v) Key.t) = K.eq
  let key key f providers = builtin key (Key.mk_key ~eq:(get_eq key) f) providers
  let oracle key f providers = builtin key (Key.mk_oracle ~eq:(get_eq key) f) providers
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
let build_result (type k v) store (k : (k, v) Key.t) (key : k) ~has_change ~previous : result =
  let module K = (val k) in
  let start = Sys.time () in
  Log.debug (fun f ->
      f "Building %a (has_change=%b, previous=%b)" (Key.pp k) key has_change
        (Option.is_some previous));
  let version = store.version in
  let contents, trace =
    let provider =
      match K.factory with
      | Some f -> f
      | None -> (
        match IntMap.find_opt K.id store.providers with
        | None -> Printf.sprintf "No provider for %S" K.name |> invalid_arg
        | Some (K.Provider f) -> f
        | Some _ -> Printf.sprintf "Incorrect provider for %S" K.name |> failwith)
    in
    let previous =
      match previous with
      | Some (previous : result) ->
          let c = Key.value k previous.contents in
          if has_change then KeyInfo.DependencyChange c else KeyInfo.Recompute c
      | _ -> KeyInfo.Absent
    in
    let res, trace = with_context ~store ~tracing:true (fun t -> provider t key previous) in
    (res, Depends trace)
  in

  let delta = Sys.time () -. start in
  let result =
    { checked_at = version;
      changed_at = version;
      built_at = version;
      contents = K.Value contents.value;
      trace
    }
  in
  let log kind = Log.info (fun f -> f "Finished %a in %.2f (%s)" (Key.pp k) key delta kind) in
  match previous with
  | Some ({ changed_at; _ } as old) -> (
    match contents.changed with
    | NoChange ->
        log "did nothing";
        (match trace with
        | Depends [] -> ()
        | Depends (_ :: _) -> failwith "Key returned NoChange, but has dependencies!");
        { old with contents = K.Value contents.value; checked_at = version }
    | RecomputeChange -> log "changed"; result
    | RecomputeSame -> log "same"; { result with changed_at })
  | _ ->
      (match contents.changed with
      | NoChange -> failwith "Key returned NoChange for a new key!"
      | _ -> ());
      log "new"; result

let build_result store (Mk (k, key) : BoxedKey.t) ~has_change ~previous : result =
  (* The use of magic here is terribly ugly, but it's not clear how else to convert from the
     abstract store to the concrete one without introducing another open union. There's only one
     possible type here, so our use of magic shouldn't ever be incorrect. *)
  build_result store (Obj.magic k) key ~has_change ~previous

(** Determine if any of this task's dependencies have changed. *)
let has_change source build : result -> bool = function
  | { built_at; trace = Depends trace; _ } ->
      let has container =
        match BoxedKey.get_key container with
        | None ->
            Log.debug (fun f ->
                f "Rebuilding: %a - %a is missing from the cache" BoxedKey.pp source
                  BoxedKey.pp_container container);

            true
        | Some key ->
            let _, result = build key container in
            if result.changed_at <= built_at then false
            else (
              Log.debug (fun f ->
                  f "Rebuilding: %a - %a was built more recently (%d > %d)" BoxedKey.pp source
                    BoxedKey.pp_container container result.changed_at built_at);
              true)
      in
      List.exists has trace

(** Get a result, rebuilding it if required. *)
let rebuild ({ version; results; _ } as store) =
  let rec go k container =
    match BoxedKey.get_data container with
    | Some ({ checked_at; _ } as result) when checked_at = version ->
        (* If we already checked this version, then don't check dependencies. *)
        (container, result)
    | previous ->
        (* Rebuild and update the key. *)
        let has_change = Option.fold ~none:true ~some:(has_change k go) previous in
        let result = build_result store k ~has_change ~previous in
        BoxedKey.set_data container result;
        (container, result)
  in
  fun key ->
    match KeyTbl.find results key with
    | Some container -> go key container
    | None ->
        let result = build_result store key ~has_change:true ~previous:None in
        let container = KeyTbl.insert results key result in
        (container, result)

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

let pp_store ~all out { results; _ } =
  KeyTbl.pp ~all ~key:BoxedKey.pp ~value:(fun out _ -> Format.pp_print_string out "?") out results
