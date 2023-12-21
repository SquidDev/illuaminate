module Number = struct
  type t =
    | Int of Int64.t
    | Float of float

  let parse x =
    (* FIXME: This is overly permissive, as it accepts 0b123 and 0o123. We should probably write our
       own parser here. *)
    match Int64.of_string_opt x with
    | Some x -> Ok (Int x)
    | None -> (
      match Float.of_string_opt x with
      | Some x -> Ok (Float x)
      | None -> Error ())
end

module String = struct
  type component = Literal_string_component.component =
    | Quote of char
    | Segment
    | Escape of char
    | Decimal_escape of char
    | Hex_escape of char
    | Unicode_escape of Uchar.t
    | Unknown_escape of char
    | Zap

  type spanned_component =
    { contents : component;
      start : int;
      length : int
    }

  type t =
    | Short_string of spanned_component list
    | Long_string of string

  let rec parse_short_str ~close ~acc ~str (lexer : Lexing.lexbuf) =
    let start = lexer.lex_curr_pos in
    match Literal_string_escape.component lexer with
    | exception Literal_string_component.Malformed -> Error ()
    | Quote c when c = close ->
        if start = String.length str - 1 then Ok (Short_string (List.rev acc)) else Error ()
    | result ->
        let result = { contents = result; start; length = lexer.lex_curr_pos - start } in
        parse_short_str ~close ~acc:(result :: acc) ~str lexer

  let parse_long_string str = Ok (Long_string str)

  let parse str =
    let len = String.length str in
    if len < 2 then Error ()
    else
      match str.[0] with
      | ('\'' | '"') as c ->
          let lexer = Lexing.from_string ~with_positions:false str in
          lexer.lex_curr_pos <- 1;
          parse_short_str ~close:c ~acc:[] ~str lexer
      | '[' -> parse_long_string str
      | _ -> Error ()

  let emit_component buf str { contents; start; length } =
    match contents with
    | Segment -> Buffer.add_substring buf str start length
    | Quote c | Escape c | Decimal_escape c | Hex_escape c | Unknown_escape c ->
        Buffer.add_char buf c
    | Unicode_escape c -> Buffer.add_utf_8_uchar buf c
    | Zap -> ()

  let parse_value str =
    parse str
    |> Result.map @@ function
       | Short_string [] -> ""
       | Short_string [ { contents = Segment; start; length } ] -> String.sub str start length
       | Short_string components ->
           let buffer = Buffer.create (String.length str) in
           List.iter (emit_component buffer str) components;
           Buffer.contents buffer
       | Long_string s -> s
end
