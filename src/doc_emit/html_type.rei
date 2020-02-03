open Html.Default;
open IlluaminateSemantics;

/** Convert a type to HTML, using some resolve function to look up internal links. */
let show_type: (~resolve: string => string, Doc.Syntax.Type.t) => node;

/** Convert a potential type to HTML. */
let show_type_opt:
  (~resolve: string => string, option(Doc.Syntax.Type.t)) => node;

/** Wrap a HTML node with a link to a reference, using some resolve function to look up internal links. */
let show_reference:
  (~resolve: string => string, Reference.resolved, node) => node;
