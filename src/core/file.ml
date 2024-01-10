type t =
  | Markdown of
      { attributes : (string Span.spanned * string Span.spanned) list;
        contents : string Span.spanned
      }
  | Lua of Syntax.program

let equal a b =
  match (a, b) with
  | Markdown x, Markdown y -> x.attributes == y.attributes && x.contents == y.contents
  | Lua x, Lua y -> x == y
  | _, _ -> false

let span = function
  | Markdown { contents; _ } -> contents.span
  | Lua p -> Syntax.Spanned.program p

let emit out = function
  | Lua x -> Emit.program out x
  | Markdown { attributes = []; contents } -> Format.pp_print_string out contents.value
  | Markdown { attributes; contents } ->
      Format.pp_print_string out "---@\n";
      List.iter (fun (x, y) -> Format.fprintf out "%s: %s@\n" x.Span.value y.Span.value) attributes;
      Format.pp_print_string out "---@\n";
      Format.pp_print_string out contents.value
