(** Load a file, converting it to a HTML node depending on the file's type. *)
val load_file : options:Html_options.t -> Fpath.t -> (Html.Default.node, string) result
