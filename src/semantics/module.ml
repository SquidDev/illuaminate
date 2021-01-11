module Kind = struct
  (** The "kind" of a Lua module. This controls how it is loaded and exposes members. *)
  type t = ModuleKind of string [@@unboxed]

  let compare (ModuleKind x) (ModuleKind y) = String.compare x y

  (** A legacy module, using the "module" directive. Global variables declared in this file are
      considered as exported by this file. *)
  let module_ = ModuleKind "module"

  (** A standard module, loaded using [require] which returns the term that it exports. *)
  let library = ModuleKind "library"

  let pp out (ModuleKind x) = Format.pp_print_string out x

  let v x = ModuleKind x
end

(** A reference to a specific module. *)
module Ref = struct
  type t = Kind.t * string

  let pp out ((kind, name) : t) : unit = Format.fprintf out "%a!%s" Kind.pp kind name
end
