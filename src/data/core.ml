let src = Logs.Src.create ~doc:"A basic system for incremental computation" __MODULE__

module Log = (val Logs.src_log src)

(** The module type of user-defined keys. *)
module type KEY = sig
  type t

  include Hashtbl.HashedType with type t := t

  val pp : Format.formatter -> t -> unit
end

(** A monotonic counter, representing the current version of the store. *)
type version = int

type 'a rule_id = Rule_id : int -> ('k * 'v) rule_id [@@unboxed]

module Build_types = struct
  (** The reason a build rule was triggered. *)
  type 'a reason =
    | Absent
    | DependencyChange of 'a
    | Recompute of 'a

  (** How this value changed. *)
  type change =
    | NoChange
    | RecomputeChange
    | RecomputeSame

  (** The result of building a rule. *)
  type 'a build_result =
    { value : 'a;
      changed : change
    }
end

open Build_types

(** The contents of our store.

    [P] is only ever instantiated with one concrete instance. However, we do this ugly song and
    dance so we can actually have the cycle of *)
module Store_factory (C : sig
  type context
end) =
struct
  type ('k, 'v) provider = C.context -> 'k -> 'v reason -> 'v build_result

  (** A "rule", describing a type of object to be built and how to build it. *)
  type ('k, 'v) rule =
    { id : ('k * 'v) rule_id;
      name : string;
      pp : Format.formatter -> 'k -> unit;
      eq : 'k -> 'k -> bool;
      hash : 'k -> int;
      build : ('k, 'v) provider;
      value_eq : ('v -> 'v -> bool) option
    }

  (** The result of building a rule. *)
  type 'v value =
    { changed_at : version;
      built_at : version;
      checked_at : version;
      contents : 'v;
      trace : trace_entry list
    }

  (** An instantiated build rule, with its {!rule}, {!key} and {!value}. This is used in trace
      entries to allow direct access to the value (and thus faster trace verification). *)
  and ('k, 'v) kv_entry =
    { rule : ('k, 'v) rule;
      key : 'k;
      mutable value : 'v value
    }

  (** A {!t} with its type removed. *)
  and trace_entry = Trace_entry : ('k, 'v) kv_entry -> trace_entry [@@unboxed]

  module Rule = struct
    type 'a t = 'a rule_id

    let hash (type a) (Rule_id x : a t) = x

    let equal (type a b) (Rule_id x : a t) (Rule_id y : b t) : (a, b) Het_map.Eq.t =
      if Int.equal x y then Obj.magic Het_map.Eq.Eq else Ineq
  end

  module Provider = struct
    type 'a t = P : ('k, 'v) provider -> ('k * 'v) t [@@unboxed]
  end

  module Provider_tbl = Het_map.Make (Rule) (Provider)

  module Store_key = struct
    type 'a t = K : ('k, 'v) rule * 'k -> ('k * 'v) t

    let hash (type a) (K ({ id; hash; _ }, value) : a t) =
      let (Rule_id id) = id in
      (id * 31) + hash value

    let equal (type a b) (K (lk, lv) : a t) (K (rk, rv) : b t) : (a, b) Het_map.Eq.t =
      match Het_map.Eq.by_ref lk rk with
      | Ineq -> Het_map.Eq.Ineq
      | Eq -> if lk.eq lv rv then Eq else Ineq
  end

  (** The result of building an object. *)
  module Store_value = struct
    type 'a t = V : ('k, 'v) kv_entry -> ('k * 'v) t [@@unboxed]
  end

  (** The map of store keys to store values. *)
  module Tbl = Het_map.Make (Store_key) (Store_value)
end

module type Store_Sig = sig
  type context

  include module type of Store_factory (struct
    type nonrec context = context
  end)
end

module rec Store : sig
  type t =
    { mutable version : version;
      providers : Key_store.Provider_tbl.t;
      results : Key_store.Tbl.t
    }

  type context =
    { store : t;
      tracing : bool;
      mutable active : bool;
      mutable trace : Key_store.trace_entry list
    }
end = struct
  type t =
    { mutable version : version;
      providers : Key_store.Provider_tbl.t;
      results : Key_store.Tbl.t
    }

  type context =
    { store : t;
      tracing : bool;
      mutable active : bool;
      mutable trace : Key_store.trace_entry list
    }
end

and Key_store : (Store_Sig with type context := Store.context) = Store_factory (Store)

include Store
include Key_store

module Key = struct
  include Build_types

  type ('k, 'v) t = ('k, 'v) Key_store.rule

  let name (x : _ t) = x.name

  type ('k, 'v, 'f) factory = name:string -> key:(module KEY with type t = 'k) -> 'f -> ('k, 'v) t

  let next_id = ref 0

  let get_id () =
    let id = !next_id in
    incr next_id; Rule_id id

  let make (type k v) ~name ~(key : (module KEY with type t = k)) ?value_eq id build : (k, v) rule =
    let module K = (val key) in
    { id; name; build; eq = K.equal; hash = K.hash; pp = K.pp; value_eq }

  let builtin ~name ~key f =
    let id = get_id () in
    make ~name ~key id f

  let mk_oracle ~eq ~f _ key = function
    | Absent -> { value = f key None; changed = RecomputeChange }
    | Recompute old | DependencyChange old ->
        let value = f key (Some old) in
        { value; changed = (if eq old value then RecomputeSame else RecomputeChange) }

  let oracle ?(eq = ( == )) ~name ~key f =
    let id = get_id () in
    make ~name ~key id (mk_oracle ~eq ~f)

  let mk_key ~eq ~f store key = function
    | Absent -> { value = f store key; changed = RecomputeChange }
    | Recompute value -> { value; changed = NoChange }
    | DependencyChange old ->
        let value = f store key in
        { value; changed = (if eq old value then RecomputeSame else RecomputeChange) }

  let key ?(eq = ( == )) ~name ~key f =
    let id = get_id () in
    make ~name ~key id (mk_key ~eq ~f)

  let deferred ?(eq = ( == )) ~name ~key () =
    let id = get_id () in
    let build store key =
      match Provider_tbl.get store.store.providers id with
      | None -> Printf.sprintf "No provider for %S" name |> failwith
      | Some (P provider) -> provider store key
    in
    make ~name ~key ~value_eq:eq id build
end

module Builder = struct
  type t = Provider_tbl.t

  let builtin (type k v) (rule : (k, v) Key.t) provider providers =
    if Provider_tbl.mem providers rule.id then
      invalid_arg (Printf.sprintf "Builder.add_deferred : Multiple factories for %s" rule.name);
    Provider_tbl.set providers rule.id (Provider.P provider)

  let get_eq k =
    match k.value_eq with
    | Some eq -> eq
    | None -> invalid_arg (Printf.sprintf "No equality function for %s" k.name)

  let key key f providers = builtin key (Key.mk_key ~eq:(get_eq key) ~f) providers
  let oracle key f providers = builtin key (Key.mk_oracle ~eq:(get_eq key) ~f) providers

  let build add =
    let providers = Provider_tbl.create 8 in
    add providers;
    { version = 0; providers; results = Key_store.Tbl.create 32 }
end

let refresh x = x.version <- x.version + 1

let with_context ~store ~tracing f =
  let t = { tracing; trace = []; active = true; store } in
  let res = f t in
  t.active <- false;
  (res, t.trace)

(** Build a single term and return the result. This does not do dependency checking, nor does it
    update the store. *)
let build_result (type k v) store (rule : (k, v) Key.t) (key : k) ~has_change ~previous : v value =
  let start = Sys.time () in
  Log.debug (fun f ->
      f "Building %a (has_change=%b, previous=%b)" rule.pp key has_change (Option.is_some previous));
  let version = store.version in
  let new_result, trace =
    let previous =
      match previous with
      | Some (previous : v value) ->
          let c = previous.contents in
          if has_change then DependencyChange c else Recompute c
      | _ -> Absent
    in
    let res, trace = with_context ~store ~tracing:true (fun t -> rule.build t key previous) in
    (res, trace)
  in

  let delta = Sys.time () -. start in
  let new_value =
    { checked_at = version;
      changed_at = version;
      built_at = version;
      contents = new_result.value;
      trace
    }
  in
  let log kind =
    Log.info (fun f -> f "Finished %s[%a] in %.2f (%s)" rule.name rule.pp key delta kind)
  in
  match previous with
  | Some ({ changed_at; _ } as old) -> (
    match new_result.changed with
    | NoChange ->
        log "did nothing";
        (match trace with
        | [] -> ()
        | _ :: _ -> failwith "Key returned NoChange, but has dependencies!");
        { old with contents = new_result.value; checked_at = version }
    | RecomputeChange -> log "changed"; new_value
    | RecomputeSame -> log "same"; { new_value with changed_at })
  | _ ->
      (match new_result.changed with
      | NoChange -> failwith "Key returned NoChange for a new key!"
      | _ -> ());
      log "new"; new_value

(** Determine if any of this task's dependencies have changed. *)
let rec has_change (type k v) store (entry : (k, v) kv_entry) : bool =
  let { trace; built_at; _ } = entry.value in
  let has (Trace_entry entry) =
    let result = build_entry store entry in
    if result.changed_at <= built_at then false
    else (
      Log.debug (fun f ->
          f "Rebuilding: %a - %a was built more recently (%d > %d)" entry.rule.pp entry.key
            entry.rule.pp entry.key result.changed_at built_at);
      true)
  in
  List.exists has trace

and build_entry : 'k 'v. t -> ('k, 'v) kv_entry -> 'v value =
 fun store ({ value; _ } as entry) ->
  if value.checked_at = store.version then
    (* If we already checked this version, then don't check dependencies. *)
    value
  else
    (* Rebuild and update the key. *)
    let has_change = has_change store entry in
    let value = build_result store entry.rule entry.key ~has_change ~previous:(Some value) in
    entry.value <- value;
    value

(** Get a result, rebuilding it if required. *)
let rebuild ({ results; _ } as store) rule key =
  let bkey = Store_key.K (rule, key) in
  match Tbl.get results bkey with
  | Some (V entry) -> (entry, build_entry store entry)
  | None ->
      let value = build_result store rule key ~has_change:true ~previous:None in
      let entry = { rule; key; value } in
      Tbl.set results bkey (V entry); (entry, value)

let get store rule key =
  let _, value = rebuild store rule key in
  value.contents

let need ({ store; _ } as context) rule key =
  if not context.active then failwith "Calling need after computing the result.";
  let entry, value = rebuild store rule key in
  if context.tracing then context.trace <- Trace_entry entry :: context.trace;
  value.contents

let compute f store = with_context ~store ~tracing:false f |> fst

module Rule_itbl =
  Het_map.Make
    (Store_key)
    (struct
      type 'a t = int
    end)

let pp_store out { results; _ } =
  Format.fprintf out "digraph {:,  @<h>[";
  let counter = ref 0 and tbl = Rule_itbl.create (Tbl.length results) in
  let get_id k =
    match Rule_itbl.get tbl k with
    | Some i -> i
    | None ->
        let c = !counter in
        incr counter; Rule_itbl.set tbl k c; c
  in
  Tbl.to_seq results
  |> Seq.iter (fun (Tbl.Packed (k, V v)) ->
         let id = get_id k in
         Format.fprintf out "n_%d [label=%S];@;" id (Format.asprintf "%a" v.rule.pp v.key);
         List.iter
           (fun (Trace_entry t) ->
             Format.fprintf out "n_%d -> n_%d;@;" id (get_id (K (t.rule, t.key))))
           v.value.trace);
  Format.fprintf out "@]}"
