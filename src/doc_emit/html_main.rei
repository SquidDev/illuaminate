open IlluaminateSemantics.Doc.AbstractSyntax;
open IlluaminateSemantics.Doc.Extract;
open IlluaminateSemantics.Doc.Syntax;

/** Configuration options for the backend. */
module Options: {
  type t;
  let make:
    (
      ~site_title: string=?,
      ~site_image: string=?,
      ~site_css: string,
      ~site_js: string,
      ~resolve: string => string,
      ~source_link: source => option(string)=?,
      ~custom: list(Config.custom_kind)=?,
      unit
    ) =>
    t;
};

/** Emit an index file from a list of modules.  */
let emit_modules:
  (
    ~options: Options.t,
    ~modules: list(documented(module_info)),
    Html.Default.node
  ) =>
  Html.Default.node;

/** Emit a single module. */
let emit_module:
  (
    ~options: Options.t,
    ~modules: list(documented(module_info)),
    documented(module_info)
  ) =>
  Html.Default.node;
