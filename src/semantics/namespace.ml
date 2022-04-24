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
  type nonrec t =
    { namespace : t;  (** The namespace this page lives under. *)
      id : string;  (** The unique ID (under this namespace) of this page. *)
      title : string option
          (** A "pretty" name of this page if given. Otherwise, one should use the title. *)
    }

  (** Get the display name of this reference. *)
  let display_name x = Option.value ~default:x.id x.title

  let pp out ({ namespace; id; _ } : t) : unit = Format.fprintf out "%a!%s" pp namespace id
end
