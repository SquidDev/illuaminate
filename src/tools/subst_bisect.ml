open IlluaminateCore

type sexp =
  | Atom of Span.t * string
  | List of Span.t * sexp list

let parse (file : Span.filename) (lexbuf : Lexing.lexbuf) =
  let open! Sexp_lexer in
  Span.Lines.using file lexbuf @@ fun lines ->
  try
    let rec go stack (head : sexp list) head_start : sexp list =
      let start = lexbuf.lex_curr_p in
      let token = token lines lexbuf in
      match token with
      | Skip -> go stack head head_start
      | String x ->
          go stack (Atom (Span.of_pos2 lines start lexbuf.lex_curr_p, x) :: head) head_start
      | Open -> go ((head, head_start) :: stack) [] start
      | Close -> (
        match stack with
        | [] -> raise (Error ("Closing ')' with no matching '('", start, lexbuf.lex_curr_p))
        | (xs, xs_pos) :: stack ->
            go stack
              (List (Span.of_pos2 lines head_start lexbuf.lex_curr_p, List.rev head) :: xs)
              xs_pos )
      | End -> (
        match stack with
        | [] -> List.rev head
        | _ :: _ ->
            let head_start' = { head_start with pos_cnum = head_start.pos_cnum + 1 } in
            raise (Error ("Unclosed '('", head_start, head_start')) )
    in
    let value = go [] [] lexbuf.lex_curr_p in
    Ok value
  with Error (err, start, fin) -> Error (Span.of_pos2 lines start fin, err)

let rec has_bisect = function
  | Atom (_, "bisect_ppx") -> true
  | Atom (_, _) -> false
  | List (_, xs) -> List.exists has_bisect xs

let inject ~contents ~at extra = CCString.take at contents ^ extra ^ CCString.drop at contents

let process ~root (path : Fpath.t) =
  let name = Fpath.relativize ~root path |> Option.get |> Fpath.to_string in
  let contents = CCIO.with_in (Fpath.to_string path) CCIO.read_all in
  match
    Lexing.from_string contents |> parse (Span.Filename.mk ~path ~name (Fpath.to_string path))
  with
  | Error (_, e) -> Logs.err (fun f -> f "Cannot parse %s (%s)" name e)
  | Ok (List (pos, Atom (_, "library") :: rest) :: _) ->
      let rec find = function
        | [] ->
            Logs.info (fun f -> f "%s Has no pre-processor. Adding one." name);
            let updated =
              inject ~contents ~at:(Span.finish_offset.get pos) "\n (preprocess (pps bisect_ppx))"
            in
            CCIO.with_out (Fpath.to_string path) (Fun.flip output_string updated)
        | List (pos, Atom (_, "preprocess") :: xs) :: _ ->
            if List.exists has_bisect xs then
              Logs.info (fun f -> f "%s already uses bisect_ppx. Doing nothing." name)
            else (
              Logs.info (fun f -> f "%s has existing pre-processor. Adding bisect_ppx to it." name);

              let updated = inject ~contents ~at:(Span.finish_offset.get pos - 1) " bisect_ppx" in
              CCIO.with_out (Fpath.to_string path) (Fun.flip output_string updated) )
        | _ :: xs -> find xs
      in
      find rest
  | Ok _ -> Logs.warn (fun f -> f "Skipping %s as it has no library" name)

let () =
  Logs.set_level ~all:true (Some Info);
  Logs.format_reporter () |> Logs.set_reporter;
  let root = Sys.getcwd () |> Fpath.v in
  IlluaminatePattern.parse "src/**/dune" |> IlluaminatePattern.iter (process ~root) ~root
