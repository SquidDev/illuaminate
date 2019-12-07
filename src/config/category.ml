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

    type values += Value of value Term.Repr.repr

    val id : int

    val owner : t

    val term : value Term.t
  end

  type 'a key = (module Key with type value = 'a)

  let create ?parent ~name ~comment () = { parent; name; comment; id = new_id () }

  let add (type t) (term : t Term.t) owner : t key =
    ( module struct
      type value = t

      type values += Value of value Term.Repr.repr

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
      all_terms : (module Key) IntMap.t
    }

  let empty = { cats = IntMap.empty; all_terms = IntMap.empty }

  let singleton (type a) (key : a Category.key) : t =
    let module K = (val key : Category.Key with type value = a) in
    let rec make = function
      | { parent = None; id; _ } as cat ->
          IntMap.singleton id
            { cat; terms = IntMap.singleton K.id (module K : Key); children = IntMap.empty }
      | { parent = Some parent; id; _ } as cat ->
          IntMap.singleton id { cat; terms = IntMap.empty; children = make parent }
    in
    { cats = make K.owner; all_terms = IntMap.singleton K.id (module K : Category.Key) }

  let merge_nodup l r =
    IntMap.union (fun _ l r -> if l == r then Some l else failwith "Duplicate keys") l r

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
    { cats = union_cats l.cats r.cats; all_terms = merge_nodup l.all_terms r.all_terms }

  type store =
    { schema : t;
      storage : Category.values IntMap.t
    }

  let get (type t) (key : t Category.key) { storage; schema } =
    let module K = (val key) in
    if IntMap.mem K.id schema.all_terms then
      match IntMap.find_opt K.id storage with
      | Some (K.Value v) -> Term.Repr.value v
      | _ -> Term.default K.term
    else invalid_arg "Key is not within this schema."

  let default schema = { schema; storage = IntMap.empty }

  let to_parser ({ cats; _ } as schema) : store Parser.fields =
    let open Parser in
    let build_keys keys =
      IntMap.fold
        (fun _ key rest ->
          let module K = (val key : Key) in
          let+ term = Term.Repr.to_repr_parser K.term and+ rest = rest in
          IntMap.add K.id (K.Value term) rest)
        keys (Parser.const IntMap.empty)
    in
    let rec build_cats children =
      IntMap.fold
        (fun _ { cat = { name; _ }; terms; children } rest ->
          let+ terms =
            (let+ terms = build_keys terms and+ children = build_cats children in
             merge_nodup terms children)
            |> fields |> field_opt ~name
          and+ rest = rest in
          merge_nodup (Option.value ~default:IntMap.empty terms) rest)
        children (const IntMap.empty)
    in
    let+ storage = build_cats cats in
    { schema; storage }

  let write_default out { cats; _ } : unit =
    let write_keys out keys prev =
      IntMap.fold
        (fun _ key prev ->
          let module K = (val key : Key) in
          Term.write_term out K.term prev)
        keys prev
    in
    let rec write_cat out { terms; children; _ } = write_keys out terms 1 |> write_cats out children
    and write_cats out keys prev =
      IntMap.fold
        (fun _ ({ cat = { name; comment; _ }; _ } as cat) prev ->
          Term.write_key out write_cat cat ~name ~comment prev)
        keys prev
      |> ignore
    in
    write_cats out cats 0

  let merge l r =
    if l.schema != r.schema then invalid_arg "The two stores have incompatible schemas"
    else
      let merge id x y =
        let key = IntMap.find id l.schema.all_terms in
        let module K = (val key : Key) in
        match (x, y) with
        | K.Value x, K.Value y -> Some (K.Value (Term.Repr.merge x y))
        | _, _ -> failwith "Malformed store"
      in
      { schema = l.schema; storage = IntMap.union merge l.storage r.storage }
end
