module Converter = struct
  open Sexplib.Conv

  type 'a t = (Sexplib.Sexp.t -> 'a) * ('a -> Sexplib.Sexp.t)

  let bool = (bool_of_sexp, sexp_of_bool)

  let string = (string_of_sexp, sexp_of_string)

  let float = (float_of_sexp, sexp_of_float)

  let int = (int_of_sexp, sexp_of_int)

  let list (f, t) = (list_of_sexp f, sexp_of_list t)
end

module Term = struct
  type 'a body =
    | Field of
        { default : 'a;
          converter : 'a Converter.t
        }
    | Group of 'a t

  and 'a t =
    | Node of
        { name : string;
          comment : string;
          body : 'a body
        }
    | Const : 'a -> 'a t
    | Map : (('a -> 'b) * 'a t) -> 'b t
    | Pair : ('a t * 'b t) -> ('a * 'b) t

  let group ~name ~comment body = Node { name; comment; body = Group body }

  let field ~name ~comment ~default converter =
    Node { name; comment; body = Field { default; converter } }

  let unit = Const ()

  let const x = Const x

  let ( let+ ) node map = Map (map, node)

  let ( and+ ) a b = Pair (a, b)

  let rec default : type a. a t -> a = function
    | Node { body; _ } -> default_body body
    | Map (f, t) -> f (default t)
    | Pair (l, r) -> (default l, default r)
    | Const x -> x

  and default_body : type a. a body -> a = function
    | Field { default; _ } -> default
    | Group g -> default g
end

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

module Storage = struct
  open Sexplib
  module S = Sexplib.Type_with_layout.Parsed

  let rec write_term : type a. Format.formatter -> a Term.t -> unit =
   fun out t ->
    let open Format in
    match t with
    | Term.Node { name; comment; body } ->
        (* Comment *)
        pp_print_string out ";; ";
        pp_print_string out comment;
        pp_force_newline out ();
        (* Value *)
        pp_open_box out 1;
        pp_print_string out "(";
        pp_print_string out name;
        pp_print_space out ();
        write_group out body;
        pp_print_string out ")";
        pp_close_box out ()
    | Map (_, t) -> write_term out t
    | Pair (l, r) -> write_term out l; pp_force_newline out (); write_term out r
    | Const _ -> ()

  and write_group : 'a. Format.formatter -> 'a Term.body -> unit =
   fun out t ->
    let open Format in
    match t with
    | Term.Field { default; converter = _, t } -> Sexp.pp_hum_indent 2 out (t default)
    | Group g ->
        pp_open_box out 1;
        pp_print_string out "(";
        write_term out g;
        pp_print_string out ")";
        pp_close_box out ()

  let write_default out term = write_term out term; Format.pp_print_newline out ()

  module StringMap = Map.Make (String)

  exception ParseError of Src_pos.Absolute.t * string

  (** Forget any annotations on the term. This is equivalent to the Sexplib version (though with no
      CPS transform), but on nodes with an absolute position. *)
  let rec forget = function
    | S.Atom (_, x, _) -> Sexp.Atom x
    | S.List (_, xs, _) -> Sexp.List (forget_list xs)

  and forget_list xs =
    List.filter_map
      (function
        | S.Comment _ -> None
        | Sexp t -> Some (forget t))
      xs

  (** Remove any comments from this list. *)
  let strip_comments =
    List.filter_map (function
      | S.Comment _ -> None
      | Sexp t -> Some t)

  (** Get the position of a sexp. *)
  let pos = function
    | S.Atom (p, _, _) -> p
    | S.List (p, _, _) -> p

  (** Read a single value from a key-value map, and return the map without that field, and the
      parsed value (or its default). *)
  let rec read : type a. S.t list StringMap.t -> a Term.t -> S.t list StringMap.t * a =
   fun fields t ->
    match t with
    | Term.Node { name; body; _ } -> (
      match StringMap.find_opt name fields with
      | None -> (fields, Term.default_body body)
      | Some value ->
          let value =
            match body with
            | Term.Field { converter = f, _; _ } -> (
                let value =
                  match value with
                  | [ x ] -> x
                  | _ :: x :: _ ->
                      raise
                        (ParseError (pos x, Printf.sprintf "Expected single value for %S." name))
                  | _ -> failwith "Config list should never be empty."
                in
                try f (forget value)
                with exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  let unwrap = function
                    | Conv_error.Of_sexp_error (exn, _) -> exn
                    | exn -> exn
                  in
                  let exn = unwrap exn in
                  Printexc.raise_with_backtrace (ParseError (pos value, Printexc.to_string exn)) bt
                )
            | Term.Group g -> read_group value g
          in
          (StringMap.remove name fields, value) )
    | Map (f, t) ->
        let fields, x = read fields t in
        (fields, f x)
    | Pair (l, r) ->
        let fields, l = read fields l in
        let fields, r = read fields r in
        (fields, (l, r))
    | Const x -> (fields, x)

  (** Build a map of key-value pairs, and then read any values from it. *)
  and read_group : type a. S.t list -> a Term.t -> a =
   fun fields t ->
    let add map = function
      | S.Atom (pos, _, _) ->
          raise (ParseError (pos, "Expected key-value pair, got an atom instead."))
      | List (pos, c, _) -> (
        match strip_comments c with
        | S.Atom (pos, k, _) :: (_ :: _ as value) ->
            if StringMap.mem k map then
              raise (ParseError (pos, Printf.sprintf "Duplicate key %S." k));
            StringMap.add k value map
        | List (pos, _, _) :: _ ->
            raise (ParseError (pos, "Expected atom for field key, but got a list."))
        | ([] | [ _ ]) as xs ->
            raise
              (ParseError
                 ( pos,
                   Printf.sprintf "Expected key-value pair, but this list only has %d children."
                     (List.length xs) )) )
    in
    let fields = List.fold_left add StringMap.empty fields in
    let fields, x = read fields t in
    match StringMap.min_binding_opt fields with
    | None -> x
    | Some (k, node) ->
        raise (ParseError (pos (List.hd node), Printf.sprintf "Unknown config key %S." k))

  let read buf term =
    let value = Parser_with_layout.sexps_abs Lexer.main_with_layout buf in
    try Ok (read_group (strip_comments value) term) with
    | ParseError ({ row; col }, msg) ->
        Error (Printf.sprintf "%s:%d:%d: %s" buf.lex_start_p.pos_fname row col msg)
    | Failure msg | Invalid_argument msg -> Error msg
end
