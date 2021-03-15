open IlluaminateCore
open IlluaminateConfig

let src = Logs.Src.create __MODULE__

module Log = (val Logs.src_log src)

type site_properties =
  { site_title : string option;
    site_image : Fpath.t option;
    site_url : string option;
    embed_head : Fpath.t option;
    embed_css : Fpath.t option;
    embed_js : Fpath.t option;
    source_link : IlluaminateSemantics.Doc.AbstractSyntax.source -> string option
  }

type t =
  { site_properties : site_properties;
    index : Fpath.t option;
    destination : Fpath.t;
    json_index : bool
  }

let make_source_link ~root template =
  let git_commit =
    lazy
      (match
         IlluaminateExec.exec "git" [| "git"; "-C"; Fpath.to_string root; "rev-parse"; "HEAD" |]
       with
      | Error e ->
          Log.err (fun f -> f "Cannot find git commit (%s)\n%!" e);
          None
      | Ok l -> Some (String.trim l))
  in
  let link ~path ~start_line ~end_line =
    Fun.flip CCOpt.flat_map template @@ fun template ->
    Fun.flip Template.apply template @@ function
    | "path" -> path
    | "line" | "sline" -> string_of_int start_line |> Option.some
    | "eline" -> string_of_int end_line |> Option.some
    | "commit" -> Lazy.force git_commit
    | _ -> None
  in
  let source_link : IlluaminateSemantics.Doc.AbstractSyntax.source -> string option = function
    | Position { path; start_line; end_line } -> link ~path:(Some path) ~start_line ~end_line
    | Span span ->
        let path =
          (Span.filename span).path
          |> CCOpt.flat_map (Fpath.relativize ~root)
          |> CCOpt.map Fpath.to_string
        in
        link ~path ~start_line:(Span.start_line span) ~end_line:(Span.finish_line span)
  in
  source_link

let check_theme_field' name f l r =
  match l with
  | None -> r
  | Some _ ->
      Log.warn (fun f ->
          f "Config option `(doc (%s ...))' should be specified as `(doc (theme (%s ...))' instead."
            name name);
      f l

let check_theme_field name l r = check_theme_field' name Fun.id l r

let term =
  let open Term in
  let option ~ty (parse, print) =
    Converter.atom ~ty
      (function
        | ":none" -> Ok None
        | x -> parse x |> Result.map Option.some)
      (function
        | None -> ":none"
        | Some x -> print x)
  and base ~ty (parse, print) = Converter.atom ~ty parse print
  and string = (Result.ok, Fun.id)
  and path =
    ( (fun p ->
        CCString.drop_while (fun x -> x = '/') p
        |> Fpath.of_string
        |> Result.map_error (fun (`Msg msg) -> msg)),
      Fpath.to_string )
  in
  let site_properties_common =
    let+ site_title =
      field ~name:"title" ~comment:"A title to display for the site" ~default:None
        (option ~ty:"string" string)
    and+ site_image =
      field ~name:"logo" ~comment:"The path to a logo to display for this site." ~default:None
        (option ~ty:"path" path)
    and+ embed_js =
      field ~name:"scripts"
        ~comment:
          "A JavaScript file which should be included in the generated docs. This is appended to \
           the default scripts."
        ~default:None (option ~ty:"path" path)
    and+ embed_css =
      field ~name:"styles"
        ~comment:
          "A JavaScript file which should be included in the generated docs. This is appended to \
           the default styles."
        ~default:None (option ~ty:"path" path)
    and+ source_link =
      field ~name:"source-link"
        ~comment:
          "A link to an website containing hosting code. The URL is a templated string, where \
           `${foo}` is replaced by the contents of `foo` variable.\n\n\
           This accepts the following variables:\n\
          \ - path: The documented source's path, relative to the project root.\n\
          \ - sline/eline: The start and end line of the variable's definition.\n\
          \ - commit: The current commit hash, as returned by git rev-parse HEAD."
        ~default:None
        (option ~ty:"template" (Template.converter [ "path"; "line"; "sline"; "eline"; "commit" ]))
    in
    (site_title, site_image, embed_js, embed_css, source_link)
  in
  let+ site_title, site_image, embed_js, embed_css, source_link = site_properties_common
  and+ site_properties =
    group ~name:"site" ~comment:"HTML-specific properties"
    @@ let+ site_title, site_image, embed_js, embed_css, source_link = site_properties_common
       and+ site_url =
         field ~name:"url"
           ~comment:"The full URL the site is hosted on (e.g. https://example.com/docs)."
           ~default:None (option ~ty:"url" string)
       and+ embed_head =
         field ~name:"head"
           ~comment:"Additional content to be included in the <head> of the generated pages."
           ~default:None (option ~ty:"path" path)
       in
       let site_url =
         Option.map (fun url -> if CCString.suffix ~suf:"/" url then url else url ^ "/") site_url
       in
       fun root ->
         { site_title;
           site_image;
           site_url;
           embed_js;
           embed_css;
           embed_head = Option.map (Fpath.append root) embed_head;
           source_link = make_source_link ~root source_link
         }
  and+ index =
    field ~name:"index" ~comment:"A path to an index file." ~default:None (option ~ty:"path" path)
  and+ destination =
    field ~name:"destination" ~comment:"The folder to write to" ~default:(Fpath.v "doc")
      (base ~ty:"path" path)
  and+ json_index =
    field ~name:"json-index"
      ~comment:
        "Whether to create an index.json file, with a dump of all terms. This may be useful for \
         querying externally."
      ~default:true Converter.bool
  in
  fun root ->
    let ofile = Option.map (Fpath.append root) in
    let site_properties = site_properties root in
    { site_properties =
        { site_properties with
          site_title = check_theme_field "title" site_title site_properties.site_title;
          site_image = ofile @@ check_theme_field "title" site_image site_properties.site_image;
          embed_css = ofile @@ check_theme_field "title" embed_css site_properties.embed_css;
          embed_js = ofile @@ check_theme_field "title" embed_js site_properties.embed_js;
          source_link =
            check_theme_field' "title" (make_source_link ~root) source_link
              site_properties.source_link
        };
      index = ofile index;
      destination = Fpath.append root destination;
      json_index
    }

let category = Category.add term IlluaminateSemantics.Doc.Extract.Config.workspace
