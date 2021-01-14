(** The "kind" of a Lua module. This controls how it is loaded and exposes members. *)
type t = Namespace of string [@@unboxed]

let compare (Namespace x) (Namespace y) = String.compare x y

(** The namespace for {!Doc_abstract_syntax.MKModule} modules. *)
let module_ = Namespace "module"

(** The namespace for {!Doc_abstract_syntax.MKLibrary} modules. *)
let library = Namespace "library"

(** Builtin modules. *)
let builtins = [ module_; library ]

let pp out (Namespace x) = Format.pp_print_string out x

let v x = Namespace x

(** A reference to a specific module. *)
module Ref = struct
  type nonrec t = t * string

  let pp out ((kind, name) : t) : unit = Format.fprintf out "%a!%s" pp kind name
end
