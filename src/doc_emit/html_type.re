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

let opt_ty = ty =>
  switch (ty) {
  | Union(tys) =>
    let (is_opt, ty) =
      List.fold_right(
        (ty, (is_opt, tys)) =>
          switch (ty) {
          | NilTy => (true, tys)
          | _ => (is_opt, [ty] @ tys)
          },
        tys,
        (false, []),
      );
    (is_opt, Union(ty));
  | _ => (false, ty)
  };

let show_reference = (~options, x, label) => {
  let (link, class_) = Html_basic.reference_attrs(~options, x, `Code);
  switch (link) {
  | Some(href) => <a href class_> label </a>
  | None => <span class_> label </span>
  };
};

let rec show_type = (~options, x) =>
  switch (x) {
  | NilTy => str("nil")
  | BoolTy(true) => str("true")
  | BoolTy(false) => str("false")
  | IntTy(x) => x |> CCInt.to_string |> str
  | NumberTy(x) => x |> CCFloat.to_string |> str
  | StringTy(x) => x |> String.escaped |> Printf.sprintf("\"%s\"") |> str
  | Named(r, l) => show_reference(~options, r, str(l))
  | Union([]) => nil
  | Union([x, ...xs]) =>
    [
      show_type(~options, x),
      ...xs |> CCList.flat_map(x => [str(" | "), show_type(~options, x)]),
    ]
    |> many
  | Function({args, return}) =>
    let args = {
      let (_, opt, nodes) =
        List.fold_left(show_type_arg(~options), (0, 0, []), args);
      [[str(String.make(opt, ']'))], ...nodes]
      |> List.rev
      |> List.flatten
      |> many;
    };
    let return =
      switch (return) {
      | ([], None) => nil
      | ([], Some(rest)) =>
        [str(":"), show_type(~options, rest), str("...")] |> many

      | ([_, ..._] as tys, None) =>
        [
          str(":"),
          ...tys
             |> List.map(show_type(~options))
             |> CCList.intersperse(str(", ")),
        ]
        |> many

      | ([_, ..._] as tys, Some(rest)) =>
        [
          str(":"),
          ...List.append(
               tys
               |> List.map(show_type(~options))
               |> CCList.intersperse(str(", ")),
               [str(","), show_type(~options, rest), str("...")],
             ),
        ]
        |> many
      };
    [str("function("), args, str(")"), return] |> many;
  | Table(fields) =>
    let fields =
      fields
      |> List.map(show_type_table_entry(~options))
      |> CCList.intersperse(str(", "))
      |> many;
    [str("{ "), fields, str(" }")] |> many;
  }

and show_type_arg = (~options, (i, o, b), {name, opt, ty}) => {
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
    [[str(pre), show_type(~options, ty)], ...b],
  );
}

and show_type_table_entry = (~options, x) =>
  switch (x) {
  | Field({key, optional, value}) =>
    [
      str(key),
      show_opt(~kind="field", optional),
      str(" = "),
      show_type(~options, value),
    ]
    |> many
  | Item(ty) => show_type(~options, ty)
  | Many(ty) => [show_type(~options, ty), str("...")] |> many
  | Hash({key, optional, value}) =>
    [
      str("["),
      show_type(~options, key),
      str("]"),
      show_opt(~kind="field", optional),
      str(" = "),
      show_type(~options, value),
    ]
    |> many
  };

let show_type_opt = (~options, x) =>
  switch (x) {
  | None => nil
  | Some(ty) => <span class_="type"> {show_type(~options, ty)} </span>
  };
