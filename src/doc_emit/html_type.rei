open IlluaminateSemantics;

/** Show an optional specifier if required. */
let show_opt: (~kind: string, bool) => Illuaminate.Html.node_;

/** Check if a type is optional (i.e. is a union contianing `nil`), and extract the non-optional type component if so. */
let opt_ty: Doc.Syntax.Type.t => (bool, Doc.Syntax.Type.t);

/** Convert a type to HTML, using some resolve function to look up internal links. */
let show_type:
  (~options: Html_options.t, Doc.Syntax.Type.t) => Illuaminate.Html.node_;

/** Convert a potential type to HTML. */
let show_type_opt:
  (~options: Html_options.t, option(Doc.Syntax.Type.t)) =>
  Illuaminate.Html.node_;

/** Wrap a HTML node with a link to a reference, using some resolve function to look up internal links. */
let show_reference:
  (~options: Html_options.t, Reference.resolved, Illuaminate.Html.node_) =>
  Illuaminate.Html.node_;
