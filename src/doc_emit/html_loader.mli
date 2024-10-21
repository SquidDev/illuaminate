(** Load a file, converting it to a HTML node depending on the file's type. *)
val load_file : options:Html_options.t -> Fpath.t -> (Illuaminate.Html.node_, string) result
