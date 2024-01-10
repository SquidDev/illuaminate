type t =
  | Markdown of
      { attributes : (string Span.spanned * string Span.spanned) list;
        contents : string Span.spanned
      }
  | Lua of Syntax.program

let ( = ) a b =
  match (a, b) with
  | Markdown x, Markdown y -> x.attributes == y.attributes && x.contents == y.contents
  | Lua x, Lua y -> x == y
  | _, _ -> false

let span = function
  | Markdown { contents; _ } -> contents.span
  | Lua p -> Syntax.Spanned.program p

let str_tok =
  let get { Span.value; span } =
    Node.Node { contents = Token.Ident value; leading_trivia = []; trailing_trivia = []; span }
  and over f { Span.value; span } =
    let node =
      Node.Node { contents = Token.Ident value; leading_trivia = []; trailing_trivia = []; span }
    in
    match f node with
    | Node.Node { contents = Token.Ident value; span; _ } -> { Span.value; span }
    | t ->
        failwith (Format.asprintf "Cannot convert token '%a' to identifier." (Node.pp Token.pp) t)
  in
  { Illuaminate.Lens.get; over }

let first =
  let get = function
    | Lua p -> Syntax.First.program.get p
    | Markdown { attributes = (x, _) :: _; _ } -> str_tok.get x
    | Markdown { attributes = []; contents } -> str_tok.get contents
  and over f = function
    | Lua p -> Lua (Syntax.First.program.over f p)
    | Markdown ({ attributes = (x, y) :: xs; _ } as md) ->
        Markdown { md with attributes = (str_tok.over f x, y) :: xs }
    | Markdown { attributes = []; contents } ->
        Markdown { attributes = []; contents = str_tok.over f contents }
  in
  { Illuaminate.Lens.get; over }

let last =
  let get = function
    | Lua p -> Syntax.Last.program.get p
    | Markdown { contents; _ } -> str_tok.get contents
  and over f = function
    | Lua p -> Lua (Syntax.First.program.over f p)
    | Markdown ({ contents; _ } as md) -> Markdown { md with contents = str_tok.over f contents }
  in
  { Illuaminate.Lens.get; over }

let emit out = function
  | Lua x -> Emit.program out x
  | Markdown { attributes = []; contents } -> Format.pp_print_string out contents.value
  | Markdown { attributes; contents } ->
      Format.pp_print_string out "---@\n";
      List.iter (fun (x, y) -> Format.fprintf out "%s: %s@\n" x.Span.value y.Span.value) attributes;
      Format.pp_print_string out "---@\n";
      Format.pp_print_string out contents.value
