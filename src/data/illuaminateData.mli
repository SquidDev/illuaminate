include module type of Core
module Programs = Programs

module Keys : sig
  module Unit : KEY with type t = unit
end
