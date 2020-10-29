open Html.Default;
open IlluaminateSemantics;

/** Show an optional specifier if required. */
let show_opt: (~kind: string, bool) => node;

/** Convert a type to HTML, using some resolve function to look up internal links. */
let show_type: (~helpers: Html_basic.t, Doc.Syntax.Type.t) => node;

/** Convert a potential type to HTML. */
let show_type_opt:
  (~helpers: Html_basic.t, option(Doc.Syntax.Type.t)) => node;

/** Wrap a HTML node with a link to a reference, using some resolve function to look up internal links. */
let show_reference: (~helpers: Html_basic.t, Reference.resolved, node) => node;
