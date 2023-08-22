(** Attempt to this document into a single paragraph. *)
val as_single_paragraph : Cmarkit.Doc.t -> Cmarkit.Inline.t option

(** Render this inline span as plain text, removing all formatting options. *)
val inline_text : Cmarkit.Inline.t -> string

(** Get the contents of a list of {!Cmarkit.Block_line.t}. *)
val block_lines_contents : Cmarkit.Block_line.t list -> string

(** Render a format to a context. *)
val cprintf : Cmarkit_renderer.Context.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
