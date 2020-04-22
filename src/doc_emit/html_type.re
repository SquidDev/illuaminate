open Html.Default;
open! IlluaminateSemantics.Doc.Syntax.Type;

let show_opt = (~kind, optional) =>
  if (optional) {
    <span
      class_="optional" title={Printf.sprintf("This %s is optional", kind)}>
      {str("?")}
    </span>;
  } else {
    nil;
  };

let show_reference = (~resolve, x, label) => {
  let (link, class_) = Html_md.reference_attrs(~resolve, x, `Code);
  switch (link) {
  | Some(href) => <a href class_> label </a>
  | None => <span class_> label </span>
  };
};

let rec show_type = (~resolve, x) =>
  switch (x) {
  | NilTy => str("nil")
  | BoolTy(true) => str("true")
  | BoolTy(false) => str("false")
  | IntTy(x) => x |> CCInt.to_string |> str
  | NumberTy(x) => x |> CCFloat.to_string |> str
  | StringTy(x) => x |> String.escaped |> Printf.sprintf("\"%s\"") |> str
  | Named(r, l) => show_reference(~resolve, r, str(l))
  | Union([]) => nil
  | Union([x, ...xs]) =>
    [
      show_type(~resolve, x),
      ...xs |> CCList.flat_map(x => [str(" | "), show_type(~resolve, x)]),
    ]
    |> many
  | Function({args, return}) =>
    let args = {
      let (_, opt, nodes) =
        List.fold_left(show_type_arg(~resolve), (0, 0, []), args);
      [[str(String.make(opt, ']'))], ...nodes]
      |> List.rev
      |> List.flatten
      |> many;
    };
    let return =
      switch (return) {
      | ([], None) => nil
      | ([], Some(rest)) =>
        [str(":"), show_type(~resolve, rest), str("...")] |> many

      | ([_, ..._] as tys, None) =>
        [
          str(":"),
          ...tys
             |> List.map(show_type(~resolve))
             |> CCList.intersperse(str(", ")),
        ]
        |> many

      | ([_, ..._] as tys, Some(rest)) =>
        [
          str(":"),
          ...List.append(
               tys
               |> List.map(show_type(~resolve))
               |> CCList.intersperse(str(", ")),
               [str(","), show_type(~resolve, rest), str("...")],
             ),
        ]
        |> many
      };
    [str("function("), args, str(")"), return] |> many;
  | Table(fields) =>
    let fields =
      fields
      |> List.map(show_type_table_entry(~resolve))
      |> CCList.intersperse(str(", "))
      |> many;
    [str("{ "), fields, str(" }")] |> many;
  }

and show_type_arg = (~resolve, (i, o, b), {name, opt, ty}) => {
  let name =
    switch (name) {
    | None => ""
    | Some(name) => name ++ ": "
    };
  let pre =
    (if (opt) {"["} else {""})
    ++ (
      if (i > 0) {
        ", ";
      } else {
        "";
      }
    )
    ++ name;
  (
    i + 1,
    o + (if (opt) {1} else {0}),
    [[str(pre), show_type(~resolve, ty)], ...b],
  );
}

and show_type_table_entry = (~resolve, x) =>
  switch (x) {
  | Field({key, optional, value}) =>
    [
      str(key),
      show_opt(~kind="field", optional),
      str(" = "),
      show_type(~resolve, value),
    ]
    |> many
  | Item(ty) => show_type(~resolve, ty)
  | Many(ty) => [show_type(~resolve, ty), str("...")] |> many
  | Hash({key, optional, value}) =>
    [
      str("["),
      show_type(~resolve, key),
      str("]"),
      show_opt(~kind="field", optional),
      str(" = "),
      show_type(~resolve, value),
    ]
    |> many
  };

let show_type_opt = (~resolve, x) =>
  switch (x) {
  | None => nil
  | Some(ty) => <span class_="type"> {show_type(~resolve, ty)} </span>
  };
