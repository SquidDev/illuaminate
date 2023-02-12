module Eq = struct
  type ('a, 'b) t =
    | Eq : ('a, 'a) t
    | Ineq : ('a, 'b) t

  type ('a, 'b) is_eq = 'a -> 'b -> ('a, 'b) t

  let by_ref (type a b) (x : a) (y : b) : (a, b) t =
    if Obj.repr x == Obj.repr y then Obj.magic Eq else Ineq

  let to_bool (type a b) (x : (a, b) t) =
    match x with
    | Eq -> true
    | Ineq -> false
end

module type HASH = sig
  type 'a t

  val hash : 'a t -> int
  val equal : 'a t -> 'b t -> ('a, 'b) Eq.t
end

module type VALUE = sig
  type 'a t
end

module Make (K : HASH) (V : VALUE) = struct
  type 'a key = 'a K.t
  type 'a value = 'a V.t
  type key_e = Key : 'a key -> key_e
  type value_e = Value : 'a value -> value_e
  type packed = Packed : 'a key * 'a value -> packed

  module Tbl = Hashtbl.Make (struct
    type t = key_e

    let hash (Key x) = K.hash x

    let equal (Key a) (Key b) =
      match K.equal a b with
      | Eq -> true
      | Ineq -> false
  end)

  type t = value_e Tbl.t

  let unsafe_cast (_ : 'a key) (v : 'b V.t) : 'a value = Obj.magic v
  let create = Tbl.create
  let set tbl key value = Tbl.replace tbl (Key key) (Value value)

  let get tbl key =
    match Tbl.find_opt tbl (Key key) with
    | None -> None
    (* This is "safe", as our equal function requires a proof that the types of the provided key and
       stored key are equal. *)
    | Some (Value v) -> Some (unsafe_cast key v)

  let get_exn tbl key =
    let (Value v) = Tbl.find tbl (Key key) in
    unsafe_cast key v

  let mem tbl k = Tbl.mem tbl (Key k)
  let length = Tbl.length

  let to_seq xs =
    Tbl.to_seq xs |> Seq.map (fun (Key key, Value value) -> Packed (key, unsafe_cast key value))
end
