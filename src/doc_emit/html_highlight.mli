(** Highlight a Lua string, rendering it as HTML *)
val lua : options:Html_options.t -> string -> Illuaminate.Html.node_

val lua_block :
  ?attrs:(string * string option) list -> options:Html_options.t -> string -> Illuaminate.Html.node_
