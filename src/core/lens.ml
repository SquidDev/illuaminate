(** The tiniest and worst lens library every created.

    As OCaml has no implicits, it's very hard to have a nice way of separating getters and setter,
    while still allowing elegant composition. Thus we just ignore that, and have a combined lens
    type. *)

(** The type of polymorphic lenses.

    These are declared as a getter function ([get]) and a modification function ([over]). This
    allows for more convenient usage compared to a [get] and [set] function. *)
type ('s, 't, 'a, 'b) lens =
  { get : 's -> 'a;  (** Read a field from the structure. *)
    over : ('a -> 'b) -> 's -> 't  (** Modify a field in the structure. *)
  }

(** A more simple lens, which doesn't apply polymorphic operations. *)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens

(** Compose two lenses, applying the first and then the second. *)
let ( -| ) a b = { get = (fun x -> a.get x |> b.get); over = (fun f -> a.over (b.over f)) }

(** Read the lens's target *)
let ( ^. ) obj lens = lens.get obj

(** Set the lens's target. *)
let ( ^= ) lens value obj = lens.over (fun _ -> value) obj

(** Apply a function to the lens's target. *)
let ( %= ) lens fn obj = lens.over fn obj

(** Function composition. May be used with %= or ^=. *)
let ( % ) f g x = f (g x)

(** Various built-in lenses. *)
module Lenses = struct
  (** Get the first item in a pair. *)
  let fst = { get = fst; over = (fun f (x, y) -> (f x, y)) }

  (** Get the second item in a pair. *)
  let snd = { get = snd; over = (fun f (x, y) -> (x, f y)) }

  (** A lens which just returns itself. *)
  let id = { get = Fun.id; over = ( @@ ) }

  (** An unsafe lens operating on the contents of an option. *)
  let unsafe_option = { get = Option.get; over = Option.map }

  (** A lens operating on the last element of a list. *)
  let last =
    let rec over f = function
      | [] -> f None |> CCOption.to_list
      | [ x ] -> f (Some x) |> CCOption.to_list
      | x :: xs -> x :: over f xs
    in
    { get = CCList.last_opt; over }

  (** A lens operating on the first element of a list. *)
  let head =
    let over f = function
      | [] -> f None |> CCOption.to_list
      | x :: xs -> CCList.cons_maybe (f (Some x)) xs
    in
    { get = CCList.head_opt; over }
end
