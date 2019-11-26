module Category = struct
  let id = ref 0

  let new_id () =
    let x = !id in
    id := x + 1;
    x

  type t =
    { parent : t option;
      name : string;
      comment : string;
      id : int
    }

  type values = ..

  module type Key = sig
    type value

    type values += Value of value

    val id : int

    val owner : t

    val term : value Term.t
  end

  type 'a key = (module Key with type value = 'a)

  let create ?parent ~name ~comment () = { parent; name; comment; id = new_id () }

  let add (type t) (term : t Term.t) owner : t key =
    ( module struct
      type value = t

      type values += Value of value

      let id = new_id ()

      let owner = owner

      let term = term
    end )
end

module Schema = struct
  module IntMap = Map.Make (Int)
  module IntSet = Set.Make (Int)
  open Category

  type cats = cat_wrapper IntMap.t

  and cat_wrapper =
    { cat : Category.t;
      terms : (module Key) IntMap.t;
      children : cats
    }

  type t =
    { cats : cats;
      keys : IntSet.t
    }

  let empty = { cats = IntMap.empty; keys = IntSet.empty }

  let singleton (type a) (key : a Category.key) : t =
    let module K = (val key : Category.Key with type value = a) in
    let rec make = function
      | { parent = None; id; _ } as cat ->
          IntMap.singleton id
            { cat; terms = IntMap.singleton K.id (module K : Key); children = IntMap.empty }
      | { parent = Some parent; id; _ } as cat ->
          IntMap.singleton id { cat; terms = IntMap.empty; children = make parent }
    in
    { cats = make K.owner; keys = IntSet.singleton K.id }

  let union l r =
    let rec union_cats l r =
      IntMap.union
        (fun _ l r ->
          Some
            { cat = l.cat;
              terms = IntMap.union (fun _ _ x -> Some x) l.terms r.terms;
              children = union_cats l.children r.children
            })
        l r
    in
    { cats = union_cats l.cats r.cats; keys = IntSet.union l.keys r.keys }

  type store =
    { schema : t;
      storage : Category.values IntMap.t
    }

  let get (type t) (key : t Category.key) { storage; schema } =
    let module K = (val key) in
    if IntSet.mem K.id schema.keys then
      match IntMap.find_opt K.id storage with
      | Some (K.Value v) -> v
      | _ -> Term.default K.term
    else invalid_arg "Key is not within this schema."

  let known_union = IntMap.union (fun _ _ x -> Some x)

  let to_term ({ cats; _ } as schema) : store Term.t =
    let open Term in
    let build_terms terms =
      IntMap.fold
        (fun _ key rest ->
          let module K = (val key : Key) in
          let+ term = K.term and+ rest = rest in
          IntMap.add K.id (K.Value term) rest)
        terms (const IntMap.empty)
    in
    let rec build_cats children =
      IntMap.fold
        (fun _ { cat = { name; comment; _ }; terms; children } rest ->
          let+ terms =
            group ~name ~comment
            @@ let+ terms = build_terms terms and+ children = build_cats children in
               known_union terms children
          and+ rest = rest in
          known_union terms rest)
        children (const IntMap.empty)
    in
    let+ storage = build_cats cats in
    { schema; storage }
end
