type t

val key : (unit, t) IlluaminateData.Key.t

(** Search our list of modules for symbols matching a specific prefix. *)
val find_modules : string -> t -> Lsp.Types.SymbolInformation.t list
