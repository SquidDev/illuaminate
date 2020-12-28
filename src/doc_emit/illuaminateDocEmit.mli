module Html : sig
  module Options : sig
    type t

    val make :
      ?site_title:string ->
      ?site_image:string ->
      site_css:string ->
      site_js:string ->
      resolve:(string -> string) ->
      data:IlluaminateData.t ->
      ?source_link:(IlluaminateSemantics.Doc.AbstractSyntax.source -> string option) ->
      ?custom:IlluaminateSemantics.Doc.Extract.Config.custom_kind list ->
      unit ->
      t
  end

  module Highlight : sig
    (** Highlight a Lua string, rendering it as HTML *)
    val lua : options:Html_options.t -> string -> Html.Default.node

    val lua_block : options:Html_options.t -> string -> Html.Default.node
  end

  (** Emit an index file from a list of modules. *)
  val emit_modules :
    options:Options.t ->
    modules:
      IlluaminateSemantics.Doc.Syntax.module_info IlluaminateSemantics.Doc.Syntax.documented list ->
    Html.Default.node ->
    Html.Default.node

  (** Emit a single module. *)
  val emit_module :
    options:Options.t ->
    modules:
      IlluaminateSemantics.Doc.Syntax.module_info IlluaminateSemantics.Doc.Syntax.documented list ->
    IlluaminateSemantics.Doc.Syntax.module_info IlluaminateSemantics.Doc.Syntax.documented ->
    Html.Default.node

  (** Load a file and convert it to HTML. This correctly handles loading markdown, HTML and text
      files. *)
  val load_file : options:Options.t -> Fpath.t -> (Html.Default.node, string) result

  (** The contents of the default JS file. *)
  val embedded_js : string

  (** The contents of the default CSS file. *)
  val embedded_css : string
end

module Summary = IlluaminateDocEmit__.Summary
