open Html.Default;
open Html_md;
open Html_type;
open IlluaminateSemantics.Reference;
open! IlluaminateSemantics.Doc.AbstractSyntax;
open! IlluaminateSemantics.Doc.Syntax;
module Cfg = IlluaminateSemantics.Doc.Extract.Config;

type t = {
  resolve: string => string,
  source_link: source => option(string),
};

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

let show_arg = (~resolve, {arg_name, arg_opt, arg_type, arg_description}) =>
  <li>
    <span class_="parameter">
      {str(arg_name)}
      {show_opt(~kind="argument", arg_opt)}
    </span>
    {str(" ")}
    {show_type_opt(~resolve, arg_type)}
    {str(" ")}
    {show_desc_inline(~resolve, arg_description)}
  </li>;

let show_return = (~resolve, {ret_type, ret_many, ret_description}) =>
  <li>
    {switch (ret_type) {
     | None => nil
     | Some(ty) =>
       <span class_="type">
         {show_type(~resolve, ty)}
         {if (ret_many) {
            str("...");
          } else {
            nil;
          }}
       </span>
     }}
    {str(" ")}
    {show_desc_inline(~resolve, ret_description)}
  </li>;

let show_function = (~resolve, args, rets, throws) =>
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
                      ...{List.map(show_arg(~resolve), args)}
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
                        ...{List.map(show_return(~resolve), rets)}
                      </ol>,
                    ]
                  )
               |> List.flatten;
             },
        ]
      }
    )
    |> many,
    List.map((x: description) => md(~resolve, x.description), throws)
    |> show_list("Throws"),
  ]
  |> many;

let show_preamble = (~resolve, {description, deprecated, _}) =>
  [
    switch (deprecated) {
    | None => nil
    | Some({deprecation_message}) =>
      <div class_="deprecated">
        <strong> {str("Deprecated")} </strong>
        {str(" ")}
        {show_desc_inline(~resolve, deprecation_message)}
      </div>
    },
    show_desc(~resolve, description),
  ]
  |> many;

let show_example = (~resolve, example) =>
  switch (example) {
  | RawExample(x) =>
    <pre class_="highlight highlight-lua">
      {Html_highlight.lua(x.value)}
    </pre>
  | RichExample((x: description)) => md(~resolve, x.description)
  };

let show_see = (~resolve, {see_reference, see_label, see_description, _}) =>
  [
    <strong>
      {show_reference(~resolve, see_reference, str(see_label))}
    </strong>,
    str(" "),
    show_desc_inline(~resolve, see_description),
  ]
  |> many;

let show_common = (~resolve, {examples, see, _}) => {
  [
    show_list("Usage", List.map(show_example(~resolve), examples)),
    show_list("See also", List.map(show_see(~resolve), see)),
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
      <span class_={
        value.deprecated != None
          ? "definition-name definition-deprecated"
          : "definition-name"
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

and show_documented_term = (~options as {resolve, _} as options, value) =>
  [
    show_preamble(~resolve, value),
    show_value(~options, value.descriptor),
    show_common(~resolve, value),
  ]
  |> many

and show_value = (~options as {resolve, _} as options, value) => {
  switch (value) {
  | Table([_, ..._] as fs) =>
    [
      <table class_="definition-list">
        ...{
             fs
             |> List.map(((field, value)) =>
                  <tr class_={
                    value.deprecated != None ? "definition-deprecated" : ""
                  }>
                    <th class_="definition-name">
                      <a
                        href={
                          "#" ++ Option.get(section_of_name(Value(field)))
                        }>
                        {str(field)}
                        {value.descriptor |> get_suffix |> str}
                      </a>
                    </th>
                    <td>
                      {if (value.deprecated != None) {
                        <strong> {str("Deprecated ")} </strong>
                      } else {
                        nil;
                      }}
                      {show_summary(~resolve, value.description)}
                    </td>
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
    show_function(~resolve, args, rets, throws)
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
      ~options as {resolve, _} as options,
      {descriptor: {type_name, type_members}, _} as desc,
    ) => {
  let sec = Option.get(section_of_name(Type(type_name)));
  [
    <h3>
      <a name=sec href={"#" ++ sec} />
      {str(" ")}
      <span> {str(type_name)} </span>
    </h3>,
    show_preamble(~resolve, desc),
    show_common(~resolve, desc),
    <dl class_="definition">
      ...{List.map(show_member(~options, type_name), type_members)}
    </dl>,
  ]
  |> many;
};

let module_list_item =
    (~resolve, ~current, {descriptor: {mod_name: name, _}, _}) =>
  switch (current) {
  | Some(current) when name == current => <strong> {str(name)} </strong>
  | _ => <a href={"module/" ++ name ++ ".html" |> resolve}> {str(name)} </a>
  };

let template =
    (~title, ~site_title, ~resolve, ~modules, ~custom=[], ~current, body) => {
  let module_list = (~title, ~kind) =>
    modules
    |> List.filter(k => k.descriptor.mod_kind == kind)
    |> List.map(module_list_item(~resolve, ~current))
    |> show_list(~tag="h2", title);
  <html>
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title> {str(title)} </title>
      <link rel="stylesheet" href={resolve("styles.css")} type_="text/css" />
    </head>
    <body>
      <div id="container">
        <div id="main">
          <nav>
            {switch (site_title) {
             | None => nil
             | Some(site_title) => <h1> {str(site_title)} </h1>
             }}
            {module_list(~title="Globals", ~kind=Module)}
            {module_list(~title="Modules", ~kind=Library)}
            {custom
             |> CCList.map(({Cfg.id, display}) => {
                  module_list(~title=display, ~kind=Custom(id))
                })
             |> many}
          </nav>
          <section id="content"> ...body </section>
        </div>
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
    </body>
  </html>;
};

let emit_modules = (~site_title=?, ~resolve, ~modules, ~custom=[], contents) => {
  let content = [
    contents,
    <table class_="definition-list">
      ...{
           modules
           |> CCList.map(m =>
                <tr>
                  <th>
                    <a href={"module/" ++ m.descriptor.mod_name ++ ".html"}>
                      {str(m.descriptor.mod_name)}
                    </a>
                  </th>
                  <td> {show_summary(~resolve, m.description)} </td>
                </tr>
              )
         }
    </table>,
  ];
  template(
    ~site_title,
    ~resolve,
    ~modules,
    ~custom,
    ~current=None,
    ~title=Option.value(~default="Index", site_title),
    content,
  );
};

let emit_module =
    (
      ~site_title=?,
      ~resolve,
      ~source_link,
      ~modules,
      ~custom=[],
      {descriptor: {mod_name, mod_contents, mod_types, _}, _} as self,
    ) => {
  let options = {resolve, source_link};
  let content = [
    <h1> <code> {str(mod_name)} </code> </h1>,
    show_preamble(~resolve, self),
    show_common(~resolve, self),
    show_value(~options, mod_contents),
    ...switch (mod_types) {
       | [] => []
       | tys => [
           <h2> {str("Types")} </h2>,
           ...List.map(show_type(~options), tys),
         ]
       },
  ];
  template(
    ~site_title,
    ~resolve,
    ~modules,
    ~custom,
    ~current=Some(mod_name),
    ~title=mod_name,
    content,
  );
};
