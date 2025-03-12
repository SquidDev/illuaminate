open IlluaminateSemantics.Doc.Syntax;

type page_list :=
  Map.Make(IlluaminateSemantics.Namespace).t(
    Map.Make(String).t(documented(page)),
  );

/** Emit an index file from a list of page.  */
let emit_index:
  (~options: Html_options.t, ~pages: page_list, Illuaminate.Html.node_) =>
  Illuaminate.Html.node_;

/** Emit a single page. */
let emit_page:
  (~options: Html_options.t, ~pages: page_list, documented(page)) =>
  Illuaminate.Html.node_;
