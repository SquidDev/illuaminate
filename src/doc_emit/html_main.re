open Html.Default;
open Html_md;
open Html_type;
open IlluaminateSemantics.Doc.Syntax;

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
      {if (arg_opt) {
         <span class_="optional" title="This argument is optional">
           {str("?")}
         </span>;
       } else {
         nil;
       }}
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
    List.map((Description(x)) => md(~resolve, x), throws)
    |> show_list("Throws"),
  ]
  |> many;

let show_example = (~resolve, example) =>
  switch (example) {
  | RawExample(x) => <pre class_="highlight highlight-lua"> {Html_highlight.lua(x)} </pre>
  | RichExample(Description(x)) => md(~resolve, x)
  };

let show_see = (~resolve, {see_reference, see_label, see_description}) =>
  [
    <strong>
      {show_reference(~resolve, see_reference, str(see_label))}
    </strong>,
    str(" "),
    show_desc_inline(~resolve, see_description),
  ]
  |> many;

let show_common = (~resolve, {examples, see, _}) =>
  [
    show_list("Usage", List.map(show_example(~resolve), examples)),
    show_list("See also", List.map(show_see(~resolve), see)),
  ]
  |> many;

let rec show_named_value = (~resolve, link, field, value) =>
  [
    <dt>
      <a name={"sec:" ++ link} href={"#sec:" ++ link} />
      {str(" ")}
      <span> {str(field)} {value.descriptor |> get_suffix |> str} </span>
    </dt>,
    <dd> {show_documented_term(~resolve, value)} </dd>,
  ]
  |> many

and show_member =
    (~resolve, type_name, {member_name, member_is_method, member_value}) => {
  let name =
    type_name ++ (if (member_is_method) {":"} else {"."}) ++ member_name;
  show_named_value(~resolve, "ty-" ++ name, name, member_value);
}

and show_documented_term = (~resolve, value) =>
  [
    show_desc(~resolve, value.description),
    show_value(~resolve, value.descriptor),
    show_common(~resolve, value),
  ]
  |> many

and show_value = (~resolve, value) => {
  switch (value) {
  | Table([_, ..._] as fs) =>
    [
      <table class_="definition_list">
        ...{
             fs
             |> List.map(((field, value)) =>
                  <tr>
                    <th>
                      <a href={"#sec:" ++ field}>
                        {str(field)}
                        {value.descriptor |> get_suffix |> str}
                      </a>
                    </th>
                    <td> {show_summary(~resolve, value.description)} </td>
                  </tr>
                )
           }
      </table>,
      <dl class_="definition">
        ...{
             fs
             |> List.map(((field, value)) =>
                  show_named_value(~resolve, field, field, value)
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
      ~resolve,
      {description, descriptor: {type_name, type_members}, _} as desc,
    ) =>
  [
    <h3>
      <a name={"sec:ty-" ++ type_name} href={"#sec:ty-" ++ type_name} />
      {str(" ")}
      <span> {str(type_name)} </span>
    </h3>,
    show_desc(~resolve, description),
    show_common(~resolve, desc),
    <dl class_="definition">
      ...{List.map(show_member(~resolve, type_name), type_members)}
    </dl>,
  ]
  |> many;

let module_list_item =
    (~resolve, ~current, {descriptor: {mod_name: name, _}, _}) =>
  switch (current) {
  | Some(current) when name == current => <strong> {str(name)} </strong>
  | _ => <a href={"module/" ++ name ++ ".html" |> resolve}> {str(name)} </a>
  };

let template = (~title, ~site_title, ~resolve, ~modules, ~current, body) =>
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
            {modules
             |> List.filter(k => k.descriptor.mod_kind == Module)
             |> List.map(module_list_item(~resolve, ~current))
             |> show_list(~tag="h2", "Globals")}
            {modules
             |> List.filter(k => k.descriptor.mod_kind == Library)
             |> List.map(module_list_item(~resolve, ~current))
             |> show_list(~tag="h2", "Modules")}
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

let emit_modules = (~site_title=?, ~resolve, ~modules, contents) => {
  let content = [
    contents,
    <table class_="definition_list">
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
    ~current=None,
    ~title=Option.value(~default="Index", site_title),
    content,
  );
};

let emit_module =
    (
      ~site_title=?,
      ~resolve,
      ~modules,
      {descriptor: {mod_name, mod_contents, mod_types, _}, _} as self,
    ) => {
  let content = [
    <h1> <code> {str(mod_name)} </code> {str(" library")} </h1>,
    show_desc(~resolve, self.description),
    show_common(~resolve, self),
    show_value(~resolve, mod_contents),
    ...switch (mod_types) {
       | [] => []
       | tys => [
           <h2> {str("Types")} </h2>,
           ...List.map(show_type(~resolve), tys),
         ]
       },
  ];
  template(
    ~site_title,
    ~resolve,
    ~modules,
    ~current=Some(mod_name),
    ~title=mod_name,
    content,
  );
};
