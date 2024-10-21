module Html : sig
  module Options : sig
    type t

    val make :
      ?site_title:string ->
      ?site_image:string ->
      ?site_url:string ->
      ?site_head:string ->
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
    val lua : options:Html_options.t -> string -> Illuaminate.Html.node_

    val lua_block :
      ?attrs:(string * string option) list ->
      options:Html_options.t ->
      string ->
      Illuaminate.Html.node_
  end

  module Assets = Html_assets
  module Doc := IlluaminateSemantics.Doc

  type page_list :=
    Doc.Syntax.page Doc.Syntax.documented Map.Make(String).t
    Map.Make(IlluaminateSemantics.Namespace).t

  (** Emit an index file from a list of pages. *)
  val emit_index :
    options:Options.t -> pages:page_list -> Illuaminate.Html.node_ -> Illuaminate.Html.node_

  (** Emit a single page. *)
  val emit_page :
    options:Options.t ->
    pages:page_list ->
    Doc.Syntax.page Doc.Syntax.documented ->
    Illuaminate.Html.node_

  (** Load a file and convert it to HTML. This correctly handles loading markdown, HTML and text
      files. *)
  val load_file : options:Options.t -> Fpath.t -> (Illuaminate.Html.node_, string) result

  (** The contents of the default JS file. *)
  val embedded_js : string

  (** The contents of the default CSS file. *)
  val embedded_css : string
end

module Summary = Summary
