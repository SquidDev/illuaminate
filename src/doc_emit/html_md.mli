(** Emit Markdown for the HTML backend. *)

open IlluaminateSemantics

(** Render a markdown document to a HTML node.*)
val md : options:Html_options.t -> Doc.Syntax.Markdown.t -> Html.Default.node

(** Render a description to a HTML node. *)
val show_desc : options:Html_options.t -> Doc.Syntax.description option -> Html.Default.node

(** Render a description to a HTML node. If this description is a single paragraph, it will be
    rendered inline rather than wrapped in a [<p>] element. *)
val show_desc_inline : options:Html_options.t -> Doc.Syntax.description option -> Html.Default.node

(** Show the summary ({!Helpers.get_summary}) of a document. *)
val show_summary : options:Html_options.t -> Doc.Syntax.description option -> Html.Default.node
