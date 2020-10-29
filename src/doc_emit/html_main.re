open Html.Default;
open Html_md;
open Html_type;
open IlluaminateSemantics.Reference;
open! IlluaminateSemantics.Doc.AbstractSyntax;
open! IlluaminateSemantics.Doc.Syntax;
module Cfg = IlluaminateSemantics.Doc.Extract.Config;

module Options = {
  type t = {
    site_title: option(string),
    site_image: option(string),
    site_css: string,
    site_js: string,
    helpers: Html_basic.t,
    source_link: source => option(string),
    custom: list(Cfg.custom_kind),
  };

  let make =
      (
        ~site_title=?,
        ~site_image=?,
        ~site_css,
        ~site_js,
        ~resolve,
        ~data,
        ~source_link=?,
        ~custom=[],
        (),
      ) => {
    site_title,
    site_image,
    site_css,
    site_js,
    helpers: {
      resolve,
      data,
    },
    source_link: Option.value(~default=Fun.const(None), source_link),
    custom,
  };
};

open Options;

let show_list = (~tag="h3", title, xs) =>
  switch (xs) {
  | [] => nil
  | _ =>
    [
      create_node(~tag, ~children=[str(title)], ()),
      <ul> ...{List.map(x => <li> x </li>, xs)} </ul>,
    ]
    |> many
  };

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

let show_arg = (~helpers, {arg_name, arg_opt, arg_type, arg_description}) =>
  <li>
    <span class_="parameter">
      {str(arg_name)}
      {show_opt(~kind="argument", arg_opt)}
    </span>
    {str(" ")}
    {show_type_opt(~helpers, arg_type)}
    {str(" ")}
    {show_desc_inline(~helpers, arg_description)}
  </li>;

let show_return = (~helpers, {ret_type, ret_many, ret_description}) =>
  <li>
    {switch (ret_type) {
     | None => nil
     | Some(ty) =>
       <span class_="type">
         {show_type(~helpers, ty)}
         {if (ret_many) {
            str("...");
          } else {
            nil;
          }}
       </span>
     }}
    {str(" ")}
    {show_desc_inline(~helpers, ret_description)}
  </li>;

let show_function = (~helpers, args, rets, throws) =>
  [
    (
      switch (args) {
      | []
      | [[]] => []
      | all_args => [
          <h3> {str("Parameters")} </h3>,
          ...all_args
             |> List.mapi((i, args) =>
                  [
                    if (i > 0) {
                      <h4> {str("Or")} </h4>;
                    } else {
                      nil;
                    },
                    <ol class_="parameter-list">
                      ...{List.map(show_arg(~helpers), args)}
                    </ol>,
                  ]
                )
             |> List.flatten,
        ]
      }
    )
    |> many,
    (
      switch (rets) {
      | []
      | [[]] => []
      | all_rets => [
          <h3> {str("Returns")} </h3>,
          ...{
               all_rets
               |> List.mapi((i, rets) =>
                    [
                      if (i > 0) {
                        <h4> {str("Or")} </h4>;
                      } else {
                        nil;
                      },
                      <ol class_="return-list">
                        ...{List.map(show_return(~helpers), rets)}
                      </ol>,
                    ]
                  )
               |> List.flatten;
             },
        ]
      }
    )
    |> many,
    List.map((x: description) => md(~helpers, x.description), throws)
    |> show_list("Throws"),
  ]
  |> many;

let show_preamble = (~helpers, {description, deprecated, _}) =>
  [
    switch (deprecated) {
    | None => nil
    | Some({deprecation_message}) =>
      <div class_="deprecated">
        <strong> {str("Deprecated")} </strong>
        {str(" ")}
        {show_desc_inline(~helpers, deprecation_message)}
      </div>
    },
    show_desc(~helpers, description),
  ]
  |> many;

let show_example = (~helpers, example) =>
  switch (example) {
  | RawExample(x) =>
    <pre class_="highlight highlight-lua">
      {Html_highlight.lua(~helpers, x.value)}
    </pre>
  | RichExample((x: description)) => md(~helpers, x.description)
  };

let show_see = (~helpers, {see_reference, see_label, see_description, _}) =>
  [
    <strong>
      {show_reference(~helpers, see_reference, str(see_label))}
    </strong>,
    str(" "),
    show_desc_inline(~helpers, see_description),
  ]
  |> many;

let show_common = (~helpers, {examples, see, _}) => {
  [
    show_list("Usage", List.map(show_example(~helpers), examples)),
    show_list("See also", List.map(show_see(~helpers), see)),
  ]
  |> many;
};

let show_pos = (~source_link, node) => {
  switch (Helpers.link(~source_link, node)) {
  | None => nil
  | Some(link) => <a href=link class_="source-link"> {str("Source")} </a>
  };
};

let rec show_named_value =
        (~options as {source_link, _} as options, name, field, value) => {
  let sec = Option.get(section_of_name(name));
  [
    <dt>
      <a name=sec href={"#" ++ sec} />
      <span
        class_={
          value.deprecated != None
            ? "definition-name definition-deprecated" : "definition-name"
        }>
        {str(field)}
        {value.descriptor |> get_suffix |> str}
      </span>
      {show_pos(~source_link, value)}
    </dt>,
    <dd> {show_documented_term(~options, value)} </dd>,
  ]
  |> many;
}

and show_member =
    (~options, type_name, {member_name, member_is_method, member_value}) => {
  let name =
    type_name ++ (if (member_is_method) {":"} else {"."}) ++ member_name;
  show_named_value(
    ~options,
    Member(type_name, member_name),
    name,
    member_value,
  );
}

and show_documented_term = (~options as {helpers, _} as options, value) =>
  [
    show_preamble(~helpers, value),
    show_value(~options, value.descriptor),
    show_common(~helpers, value),
  ]
  |> many

and show_value = (~options as {helpers, _} as options, value) => {
  switch (value) {
  | Table([_, ..._] as fs) =>
    [
      <table class_="definition-list">
        ...{
             fs
             |> List.map(((field, value)) =>
                  <tr
                    class_=?{
                      Option.map(
                        _ => "definition-deprecated",
                        value.deprecated,
                      )
                    }>
                    <th
                      class_="definition-name"
                      title=?{
                        Option.map(
                          _ => "This member is deprecated.",
                          value.deprecated,
                        )
                      }>
                      <a
                        href={
                          "#" ++ Option.get(section_of_name(Value(field)))
                        }>
                        {str(field)}
                        {value.descriptor |> get_suffix |> str}
                      </a>
                    </th>
                    <td> {show_summary(~helpers, value.description)} </td>
                  </tr>
                )
           }
      </table>,
      <dl class_="definition">
        ...{
             fs
             |> List.map(((field, value)) =>
                  show_named_value(~options, Value(field), field, value)
                )
           }
      </dl>,
    ]
    |> many
  | Table([]) => nil
  | Function({args, rets, throws, _}) =>
    show_function(~helpers, args, rets, throws)
  | Expr(_) => nil
  | Type(_) => nil
  | Unknown => nil
  | Undefined =>
    <p class_="undefined-value">
      {str("Error generating documentation for this term.")}
    </p>
  };
};

let show_type =
    (
      ~options as {helpers, _} as options,
      {descriptor: {type_name, type_members}, _} as desc,
    ) => {
  let sec = Option.get(section_of_name(Type(type_name)));
  [
    <h3>
      <a name=sec href={"#" ++ sec} />
      {str(" ")}
      <span> {str(type_name)} </span>
    </h3>,
    show_preamble(~helpers, desc),
    show_common(~helpers, desc),
    <dl class_="definition">
      ...{List.map(show_member(~options, type_name), type_members)}
    </dl>,
  ]
  |> many;
};

let module_list_item =
    (
      ~helpers as {Html_basic.resolve, _},
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
    (
      ~title,
      ~options as {helpers: {resolve, _} as helpers, _} as options,
      ~modules,
      ~current,
      body,
    ) => {
  let module_list = (~title, xs) =>
    List.map(module_list_item(~helpers, ~current), xs)
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
    (~options as {helpers, site_title, _} as options, ~modules, contents) => {
  let emit_module_row = ({descriptor: {mod_name, _}, description, _}) =>
    <tr>
      <th>
        <a href={"module/" ++ mod_name ++ ".html"}> {str(mod_name)} </a>
      </th>
      <td> {show_summary(~helpers, description)} </td>
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
      ~options as {helpers, _} as options,
      ~modules,
      {descriptor: {mod_name, mod_contents, mod_types, _} as m, _} as self,
    ) => {
  let content = [
    <h1> <code> {str(mod_name)} </code> </h1>,
    show_preamble(~helpers, self),
    show_common(~helpers, self),
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
