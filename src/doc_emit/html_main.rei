open IlluaminateSemantics.Doc.Syntax;

type page_list :=
  Map.Make(IlluaminateSemantics.Namespace).t(
    Map.Make(String).t(documented(page)),
  );

/** Emit an index file from a list of page.  */
let emit_index:
  (~options: Html_options.t, ~pages: page_list, Html.Default.node) =>
  Html.Default.node;

/** Emit a single page. */
let emit_page:
  (~options: Html_options.t, ~pages: page_list, documented(page)) =>
  Html.Default.node;
