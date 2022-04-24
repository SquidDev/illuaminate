include module type of Core

module Container : sig
  module type S = Contained_tbl.KeyContainer

  module Strong (M : Hashtbl.HashedType) : S with type t = M.t
  module Weak (M : Hashtbl.HashedType) : S with type t = M.t

  val strong : ?hash:('a -> int) -> eq:('a -> 'a -> bool) -> unit -> (module S with type t = 'a)
end

module Programs = Programs
