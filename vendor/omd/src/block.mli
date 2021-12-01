open Ast

module Pre : sig
  val of_channel :
    in_channel -> (attributes, string) Raw.block list * attributes link_def list

  val of_string :
    string -> (attributes, string) Raw.block list * attributes link_def list
end
