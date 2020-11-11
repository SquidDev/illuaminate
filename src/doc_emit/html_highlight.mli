(** Highlight a Lua string, rendering it as HTML *)
val lua : helpers:Html_basic.t -> string -> Html.Default.node

val lua_block : helpers:Html_basic.t -> string -> Html.Default.node
