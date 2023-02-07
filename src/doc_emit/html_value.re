open Html.Default;
open Html_basic;
open Html_md;
open Html_type;
open Html_options;
open IlluaminateSemantics.Reference;
open! IlluaminateSemantics.Doc.Syntax;

let show_arg = (~options, {arg_name, arg_opt, arg_type, arg_description}) =>
  <li>
    <span class_="parameter">
      {str(arg_name)}
      {show_opt(~kind="argument", arg_opt != Required)}
    </span>
    {str(" ")}
    {show_type_opt(~options, arg_type)}
    {switch (arg_opt) {
     | Required
     | Optional => nil
     | Default(x) =>
       <span class_="default-value">
         {str(" = ")}
         <code class_="language language-lua">
           {Html_highlight.lua(~options, x)}
         </code>
       </span>
     }}
    {str(" ")}
    {show_desc_inline(~options, arg_description)}
  </li>;

let show_return = (~options, {ret_type, ret_many, ret_description}) =>
  <li>
    {switch (ret_type) {
     | None when !ret_many => nil
     | ty =>
       <span class_="type">
         {Option.fold(~none=nil, ~some=show_type(~options), ty)}
         {if (ret_many) {
            str("...");
          } else {
            nil;
          }}
       </span>
     }}
    {str(" ")}
    {show_desc_inline(~options, ret_description)}
  </li>;

let show_function = (~options, args, rets, throws) =>
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
                      ...{List.map(show_arg(~options), args)}
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
                        ...{List.map(show_return(~options), rets)}
                      </ol>,
                    ]
                  )
               |> List.flatten;
             },
        ]
      }
    )
    |> many,
    List.map((x: description) => md(~options, x.description), throws)
    |> show_list("Throws"),
  ]
  |> many;

let warning_icon = raw("<span aria-hidden=\"true\">🛈</span> ");

let show_preamble = (~options, {description, deprecated, _}) =>
  [
    switch (deprecated) {
    | None => nil
    | Some({deprecation_message}) =>
      <div class_="admonition admonition-caution">
        <h5 class_="admonition-heading">
          warning_icon
          {str("Deprecated")}
        </h5>
        {show_desc(~options, deprecation_message)}
      </div>
    },
    show_desc(~options, description),
  ]
  |> many;

let show_example = (~options, example) =>
  switch (example) {
  | RawExample(x) => Html_highlight.lua_block(~options, x.value)
  | RichExample(x: description) => md(~options, x.description)
  };

let show_see = (~options, {see_reference, see_label, see_description, _}) =>
  [
    <strong>
      {show_reference(~options, see_reference, str(see_label))}
    </strong>,
    str(" "),
    show_desc_inline(~options, see_description),
  ]
  |> many;

let show_change =
    (~options, {change_kind, change_version, change_description, _}) => {
  let version =
    switch (change_kind) {
    | Added => Printf.sprintf("New in version %s", change_version)
    | Changed => Printf.sprintf("Changed in version %s", change_version)
    };
  let version =
    switch (change_description) {
    | None => version
    | Some(_) => version ++ ":"
    };
  [
    <strong> {str(version)} </strong>,
    str(" "),
    show_desc_inline(~options, change_description),
  ]
  |> many;
};

let show_common = (~options, {examples, see, changes, _}) => {
  [
    show_list("Usage", List.map(show_example(~options), examples)),
    show_list("See also", List.map(show_see(~options), see)),
    show_list("Changes", List.map(show_change(~options), changes)),
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
        {switch (value.descriptor) {
         | Expr({value: Some(value), _}) => str(" = " ++ value)
         | Expr({ty, _}) =>
           let (is_opt, ty) = Html_type.opt_ty(ty);
           [
             show_opt(~kind="field", is_opt),
             <span class_="type-sep"> {str(":")} </span>,
             <span class_="type"> {show_type(~options, ty)} </span>,
           ]
           |> many;
         | value => get_suffix(value) |> str
         }}
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

and show_documented_term = (~options, value) =>
  [
    show_preamble(~options, value),
    show_value(~options, value.descriptor),
    show_common(~options, value),
  ]
  |> many

and show_value = (~options, value) => {
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
                    <td> {show_summary(~options, value.description)} </td>
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
    show_function(~options, args, rets, throws)
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
    (~options, {descriptor: {type_name, type_members}, _} as desc) => {
  let sec = Option.get(section_of_name(Type(type_name)));
  [
    <h3>
      <a name=sec href={"#" ++ sec} />
      {str(" ")}
      <span> {str(type_name)} </span>
    </h3>,
    show_preamble(~options, desc),
    show_common(~options, desc),
    <dl class_="definition">
      ...{List.map(show_member(~options, type_name), type_members)}
    </dl>,
  ]
  |> many;
};
