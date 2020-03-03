include module type of Core

module Container : sig
  module type S = Contained_tbl.KeyContainer

  module Strong (M : Hashtbl.HashedType) : S with type t = M.t

  module Weak (M : Hashtbl.HashedType) : S with type t = M.t
end

module Programs = Programs
