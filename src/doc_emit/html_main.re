open Html.Default;
open Html_basic;
open Html_md;
open Html_value;
open IlluaminateSemantics.Reference;
open! IlluaminateSemantics.Doc.Syntax;
module Cfg = IlluaminateSemantics.Doc.Extract.Config;

module Options = Html_options;

open Options;

let show_module_list = (f, {custom, _}, xs) => {
  let f' = (~title, ~kind) =>
    List.filter(k => k.descriptor.mod_kind == kind, xs) |> f(~title);
  [
    f'(~title="Globals", ~kind=Module),
    f'(~title="Modules", ~kind=Library),
    custom
    |> CCList.map(({Cfg.id, display}) => {
         f'(~title=display, ~kind=Custom(id))
       })
    |> many,
  ];
};

let module_list_item =
    (
      ~options as {resolve, _},
      ~current,
      {descriptor: {mod_name: name, _}, _},
    ) =>
  switch (current) {
  | Some({mod_name, _}) when name == mod_name =>
    <strong> {str(name)} </strong>
  | _ => <a href={"module/" ++ name ++ ".html" |> resolve}> {str(name)} </a>
  };

let module_toc = ({mod_types, mod_contents, _}) => {
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

let template =
    (~title, ~options as {resolve, _} as options, ~modules, ~current, body) => {
  let module_list = (~title, xs) =>
    List.map(module_list_item(~options, ~current), xs)
    |> show_list(~tag="h2", title);
  <html>
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title> {str(title)} </title>
      <link
        rel="stylesheet"
        href={resolve(options.site_css)}
        type_="text/css"
      />
    </head>
    <body>
      <nav>
        <button class_="nav-reveal" type_="button"> {raw("&#9776;")} </button>
        {let link = h => <h1> <a href={resolve("./")}> h </a> </h1>;
         switch (options.site_image, options.site_title) {
         | (Some(site_image), Some(site_title)) =>
           link(<img src={resolve(site_image)} alt=site_title />)
         | (Some(site_image), None) =>
           link(<img src={resolve(site_image)} />)
         | (None, Some(site_title)) => link(str(site_title))
         | (None, None) => nil
         }}
        <div class_="nav-links">
          {Option.fold(~none=nil, ~some=module_toc, current)}
          {show_module_list(module_list, options, modules) |> many}
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
let emit_modules =
    (~options as {site_title, _} as options, ~modules, contents) => {
  let emit_module_row = ({descriptor: {mod_name, _}, description, _}) =>
    <tr>
      <th>
        <a href={"module/" ++ mod_name ++ ".html"}> {str(mod_name)} </a>
      </th>
      <td> {show_summary(~options, description)} </td>
    </tr>;

  let emit_module_group = (~title, modules) =>
    switch (modules) {
    | [] => nil
    | _ =>
      [
        <h2> {str(title)} </h2>,
        <table class_="definition-list">
          ...{CCList.map(emit_module_row, modules)}
        </table>,
      ]
      |> many
    };

  let content = [
    contents,
    ...show_module_list(emit_module_group, options, modules),
  ];
  template(
    ~options,
    ~modules,
    ~current=None,
    ~title=Option.value(~default="Index", site_title),
    content,
  );
};

let emit_module =
    (
      ~options,
      ~modules,
      {descriptor: {mod_name, mod_contents, mod_types, _} as m, _} as self,
    ) => {
  let content = [
    <h1> <code> {str(mod_name)} </code> </h1>,
    show_preamble(~options, self),
    show_common(~options, self),
    show_value(~options, mod_contents),
    ...switch (mod_types) {
       | [] => []
       | tys => [
           <h2> {str("Types")} </h2>,
           ...List.map(show_type(~options), tys),
         ]
       },
  ];
  template(~options, ~modules, ~current=Some(m), ~title=mod_name, content);
};
