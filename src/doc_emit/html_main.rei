module Doc := IlluaminateSemantics.Doc;
open Doc.Syntax;

type module_list :=
  Map.Make(IlluaminateSemantics.Module.Kind).t(
    Map.Make(String).t(documented(module_info)),
  );

/** Emit an index file from a list of modules.  */
let emit_modules:
  (~options: Html_options.t, ~modules: module_list, Html.Default.node) =>
  Html.Default.node;

/** Emit a single module. */
let emit_module:
  (
    ~options: Html_options.t,
    ~modules: module_list,
    documented(module_info)
  ) =>
  Html.Default.node;
