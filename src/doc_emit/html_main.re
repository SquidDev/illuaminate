open Html.Default;
open Html_basic;
open Html_md;
open Html_value;
open IlluaminateSemantics.Reference;
open! IlluaminateSemantics.Doc.Syntax;
module Cfg = IlluaminateSemantics.Doc.Extract.Config;
module Namespace = IlluaminateSemantics.Namespace;
module NMap = Map.Make(Namespace);
module StringMap = Map.Make(String);

module Options = Html_options;

open Options;

let show_page_list = (f, {custom, _}, xs) => {
  let f' = (~title, ~kind) =>
    NMap.find_opt(kind, xs)
    |> Option.value(~default=StringMap.empty)
    |> f(~title, ~kind);
  [
    f'(~title="Globals", ~kind=Namespace.module_),
    f'(~title="Modules", ~kind=Namespace.library),
    custom
    |> CCList.map(({Cfg.id, display}) => {
         f'(~title=display, ~kind=Namespace(id))
       })
    |> many,
  ];
};

let page_list_item =
    (
      ~options as {resolve, _},
      ~current,
      {descriptor: {page_namespace, page_id, page_title, _} as m, _},
    ) =>
  switch (current) {
  | Some(current) when current === m => <strong> {str(page_title)} </strong>
  | _ =>
    let href =
      Helpers.reference_link((page_namespace, page_id), Module) |> resolve;
    <a href> {str(page_title)} </a>;
  };

let module_toc = (mod_types, mod_contents) => {
  let make_link = x => {
    let name =
      switch (x) {
      | Value(s)
      | Type(s) => s
      | _ => assert(false)
      };
    <a href={"#" ++ Option.get(section_of_name(x))}> {str(name)} </a>;
  };
  [
    switch (mod_contents) {
    | Table(fields) =>
      fields
      |> List.map(((name, _)) => make_link(Value(name)))
      |> show_list(~tag="h2", "Contents")
    | _ => nil
    },
    mod_types
    |> List.map(({descriptor: {type_name, _}, _}) => {
         make_link(Type(type_name))
       })
    |> show_list(~tag="h2", "Types"),
  ]
  |> many;
};

let page_toc = x =>
  switch (x.page_contents) {
  | Markdown => nil
  | Module({mod_types, mod_contents, _}) =>
    module_toc(mod_types, mod_contents)
  };

let template =
    (
      ~title,
      ~description=?,
      ~options as {resolve, site_title, site_image, site_url, _} as options,
      ~pages,
      ~current,
      body,
    ) => {
  let page_list = (~title, ~kind, xs) => {
    let expand =
      switch (current) {
      | None => true
      | Some(page) => page.page_namespace == kind
      };
    StringMap.to_seq(xs)
    |> Seq.map(snd)
    |> Seq.map(page_list_item(~options, ~current))
    |> List.of_seq
    |> show_list(~tag="h2", ~expand, title);
  };
  <html>
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title> {str(title)} </title>
      // OpenGraph information.
      <meta property="og:title" content=title />
      <meta property="og:type" content="website" />
      {switch (description) {
       | None => nil
       | Some(description) =>
         [
           <meta name="description" content=description />,
           <meta property="og:description" content=description />,
         ]
         |> many
       }}
      {switch (site_url, site_image) {
       | (Some(url), Some(image)) =>
         <meta property="og:image" content={url ++ image} />
       | _ => nil
       }}
      {switch (site_title) {
       | None => nil
       | Some(title) => <meta property="og:site_name" content=title />
       }}
      <link
        rel="stylesheet"
        href={resolve(options.site_css)}
        type_="text/css"
      />
      {switch (options.site_head) {
       | None => nil
       | Some(head) => raw(head)
       }}
    </head>
    <body>
      <nav>
        <button class_="nav-reveal" type_="button"> {raw("&#9776;")} </button>
        {let link = h => <h1> <a href={resolve("./")}> h </a> </h1>;
         switch (site_image, site_title) {
         | (Some(site_image), Some(site_title)) =>
           link(<img src={resolve(site_image)} alt=site_title />)
         | (Some(site_image), None) =>
           link(<img src={resolve(site_image)} />)
         | (None, Some(site_title)) => link(str(site_title))
         | (None, None) => nil
         }}
        <div class_="nav-links">
          {Option.fold(~none=nil, ~some=page_toc, current)}
          {show_page_list(page_list, options, pages) |> many}
        </div>
      </nav>
      <div id="main">
        <section id="content"> ...body </section>
        <footer>
          {let time = Unix.time() |> Unix.gmtime;
           Printf.sprintf(
             "Last updated on %04d-%02d-%02d",
             time.tm_year + 1900,
             time.tm_mon + 1,
             time.tm_mday,
           )
           |> str}
        </footer>
      </div>
      <script
        src={resolve(options.site_js)}
        type_="text/javascript"
        defer=""
      />
    </body>
  </html>;
};
let emit_index = (~options as {site_title, _} as options, ~pages, contents) => {
  let emit_page_row =
      (
        {
          descriptor: {page_namespace, page_id, page_title, _},
          description,
          _,
        },
      ) =>
    <tr>
      <th>
        <a href={Helpers.reference_link((page_namespace, page_id), Module)}>
          {str(page_title)}
        </a>
      </th>
      <td> {show_summary(~options, description)} </td>
    </tr>;

  let emit_module_group = (~title, ~kind as _, pages) =>
    if (StringMap.is_empty(pages)) {
      nil;
    } else {
      [
        <h2> {str(title)} </h2>,
        <table class_="definition-list">
          ...{
               StringMap.to_seq(pages)
               |> Seq.map(snd)
               |> Seq.map(emit_page_row)
               |> List.of_seq
             }
        </table>,
      ]
      |> many;
    };
  let description =
    switch (site_title) {
    | None => "Documentation index"
    | Some(x) => "Documentation index for " ++ x
    };

  let content = [
    contents,
    ...show_page_list(emit_module_group, options, pages),
  ];
  template(
    ~options,
    ~pages,
    ~current=None,
    ~title=Option.value(~default="Index", site_title),
    ~description,
    content,
  );
};

let emit_page =
    (
      ~options,
      ~pages,
      {descriptor: {page_title, page_contents, _} as m, _} as self,
    ) => {
  let content = [
    <h1> {str(page_title)} </h1>,
    show_preamble(~options, self),
    show_common(~options, self),
    ...switch (page_contents) {
       | Markdown => []
       | Module({mod_types, mod_contents, _}) => [
           show_value(~options, mod_contents),
           ...switch (mod_types) {
              | [] => []
              | _ => [
                  <h3> {str("Types")} </h3>,
                  ...List.map(show_type(~options), mod_types),
                ]
              },
         ]
       },
  ];
  let description =
    switch (self.description) {
    | None => None
    | Some(x) =>
      Helpers.get_summary(x.description)
      |> Omd.to_text
      |> CCString.trim
      |> Option.some
    };
  template(
    ~options,
    ~pages,
    ~current=Some(m),
    ~title=page_title,
    ~description?,
    content,
  );
};
