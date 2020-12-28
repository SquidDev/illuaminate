(** Highlight a Lua string, rendering it as HTML *)
val lua : options:Html_options.t -> string -> Html.Default.node

val lua_block : options:Html_options.t -> string -> Html.Default.node
