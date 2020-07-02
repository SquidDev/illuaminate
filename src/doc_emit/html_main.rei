open IlluaminateSemantics.Doc.AbstractSyntax;
open IlluaminateSemantics.Doc.Extract;
open IlluaminateSemantics.Doc.Syntax;

/** Emit an index file from a list of modules.  */
let emit_modules:
  (
    ~site_title: string=?,
    ~resolve: string => string,
    ~modules: list(documented(module_info)),
    ~custom: list(Config.custom_kind)=?,
    Html.Default.node
  ) =>
  Html.Default.node;

/** Emit a single module. */
let emit_module:
  (
    ~site_title: string=?,
    ~resolve: string => string,
    ~source_link: source => option(string),
    ~modules: list(documented(module_info)),
    ~custom: list(Config.custom_kind)=?,
    documented(module_info)
  ) =>
  Html.Default.node;
