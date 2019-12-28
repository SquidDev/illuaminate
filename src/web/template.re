open IlluaminateCore;
module JsHtml =
  Html.Make({
    open Js_of_ocaml;
    type event_handler = Js.t(Dom_html.event) => Js.t(bool);
  });
include JsHtml;

let level: Error.level => string =
  level =>
    switch (level) {
    | Critical => "critical"
    | Error => "error"
    | Warning => "warning"
    | Note => "note"
    };

let error = (line, ~fix=?, {Error.Error.span, message, tag, _}) => {
  <div class_={"error error-" ++ level(tag.level)}>
    <div class_="error-pos">
      {Printf.sprintf(
         "[%d:%d-%d:%d]: %s [%s]",
         span.start_line,
         span.start_col,
         span.finish_line,
         span.finish_col,
         message,
         tag.name,
       )
       |> str}
      {switch (fix) {
       | Some(fix) =>
         <button onClick=fix class_="error-fix"> {str("Fix")} </button>
       | None => nil
       }}
    </div>
    <div class_="error-line">
      <span class_="error-line-no">
        {str(string_of_int(span.start_line))}
      </span>
      <span class_="error-line-str"> {str(line)} </span>
    </div>
  </div>;
};

let some_errors = (program, ~fix=?, errors) => {
  let main =
    List.map(
      ({Error.Error.span, _} as err) => {
        let line_end =
          String.index_from_opt(program, span.start_bol, '\n')
          |> Option.value(~default=String.length(program));
        let line =
          String.sub(program, span.start_bol, line_end - span.start_bol);
        error(line, err);
      },
      errors,
    );
  let (errors, warnings) =
    List.fold_left(
      ((errors, warnings), {Error.Error.tag, _}) =>
        switch (tag.level) {
        | Critical
        | Error => (errors + 1, warnings)
        | Warning
        | Note => (errors, warnings + 1)
        },
      (0, 0),
      errors,
    );
  many([
    <div class_="error-summary error-some">
      {str(Printf.sprintf("✘ %d errors and %d warnings", errors, warnings))}
      {switch (fix) {
       | Some(fix) =>
         <button
           onClick=fix
           class_="error-fix"
           title="Attempt to fix as many problems as possible.">
           {str("Fix all")}
         </button>
       | None => nil
       }}
    </div>,
    ...main,
  ]);
};

let no_errors =
  <div class_="error-summary error-none"> {str("✓ No errors")} </div>;
