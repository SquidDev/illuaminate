open IlluaminateCore
module StringMap = Map.Make (String)

let ( >>= ) = Result.bind

let pos_of : Sexp.t -> Span.t = function
  | Atom (x, _) -> x
  | List (x, _) -> x

module Kind = struct
  type term = Sexp.t list

  (** All fields for each key, with their start and end position, in reverse order. *)
  type fields = (Span.t * Sexp.t list) list StringMap.t
end

type ('a, 'kind) parser = Span.t -> 'kind -> ('a * 'kind, Span.t * string) result

type 'a t = ('a, Kind.term) parser

type 'a fields = ('a, Kind.fields) parser

let const x _ state = Ok (x, state)

let unit : (unit, 'k) parser = fun p -> const () p

let ( let+ ) (node : ('a, 'k) parser) (f : 'a -> 'b) : ('b, 'k) parser =
 fun pos state -> node pos state |> Result.map (fun (x, state) -> (f x, state))

let ( and+ ) a b pos state =
  a pos state >>= fun (x, state) -> b pos state |> Result.map (fun (y, state) -> ((x, y), state))

let atom_res ~ty parse : 'a t =
 fun end_pos -> function
  | [] -> Error (end_pos, Printf.sprintf "Expected %s, got nothing" ty)
  | List (pos, _) :: _ -> Error (pos, Printf.sprintf "Expected %s, got list" ty)
  | Atom (pos, txt) :: xs -> (
    match parse txt with
    | Error e -> Error (pos, e)
    | Ok x -> Ok (x, xs))

let atom ~ty parse =
  atom_res ~ty (fun x ->
      match parse x with
      | None -> Error (Printf.sprintf "Expected %s" ty)
      | Some x -> Ok x)

let bool : bool t = atom ~ty:"true or false" bool_of_string_opt

let int : int t = atom ~ty:"int" int_of_string_opt

let float : float t = atom ~ty:"number" float_of_string_opt

let string : string t = atom ~ty:"string" Option.some

let check_empty x = function
  | [] -> Ok x
  | t :: _ -> Error (pos_of t, "Expected ')', got another s-expr.")

let parse_til_empty body end_pos state =
  body end_pos state >>= fun (x, state) -> check_empty x state

let many (term : 'a t) : 'a list t =
  let rec go xs : 'a list t =
   fun end_pos -> function
    | [] -> Ok (List.rev xs, [])
    | ss -> (
      match term end_pos ss with
      | Error e -> Error e
      | Ok (x, ss') -> go (x :: xs) end_pos ss')
  in
  go []

let some (term : 'a t) : 'a list t =
  let+ x = term and+ xs = many term in
  x :: xs

let in_list (term : 'a t) : 'a t =
 fun end_pos -> function
  | [] -> Error (end_pos, "Expected list, got nothing")
  | List (pos, xs) :: state ->
      parse_til_empty term (Span.finish pos) xs |> Result.map (fun x -> (x, state))
  | Atom (pos, _) :: _ -> Error (pos, "Expected list, got atom")

let list (term : 'a t) : 'a list t = in_list (many term)

let list_or_one (term : 'a t) : 'a list t =
 fun end_pos -> function
  | List (pos, xs) :: state ->
      parse_til_empty (many term) (Span.finish pos) xs |> Result.map (fun x -> (x, state))
  | state -> term end_pos state |> Result.map (fun (x, s) -> ([ x ], s))

let fields (body : 'a fields) : 'a t =
  let extract_field end_pos : Sexp.t list -> _ = function
    | [] -> Error (end_pos, "Expected key-value pair, but this list was empty.")
    | List (pos, _) :: _ -> Error (pos, "The first item of a key-value pair must be an atom.")
    | Atom (_, t) :: xs -> Ok (t, xs)
  in
  let rec gather res : Sexp.t list -> _ = function
    | [] -> Ok res
    | Atom (pos, _) :: _ -> Error (pos, "Expected key-value pair, got a string.")
    | List (pos, ss) :: xs -> (
      match extract_field (Span.finish pos) ss with
      | Error e -> Error e
      | Ok (key, value) ->
          gather
            (StringMap.update key (fun x -> Some ((pos, value) :: Option.value ~default:[] x)) res)
            xs)
  in
  let rec last = function
    | [] -> raise (Invalid_argument "Empty list")
    | [ x ] -> x
    | _ :: xs -> last xs
  in
  fun end_pos state ->
    match gather StringMap.empty state >>= body end_pos with
    | Error e -> Error e
    | Ok (x, state) -> (
      match StringMap.choose_opt state with
      | None -> Ok (x, [])
      | Some (key, xs) ->
          let pos, _ = last xs in
          let key, pos =
            StringMap.fold
              (fun key xs (key', pos') ->
                let pos, _ = last xs in
                if Span.compare pos pos' < 0 then (key, pos) else (key', pos'))
              state (key, pos)
          in
          Error (pos, Printf.sprintf "Unexpected key %S" key))

let field ~name (body : 'a t) : 'a fields =
 fun pos fields ->
  match StringMap.find_opt name fields with
  | None -> Error (pos, Printf.sprintf "Missing %s field" name)
  | Some [] -> failwith "Impossible"
  | Some ((pos, _) :: _ :: _) -> Error (pos, Printf.sprintf "Multiple definitions of %S" name)
  | Some [ (pos, xs) ] ->
      parse_til_empty body (Span.finish pos) xs
      |> Result.map (fun x -> (x, StringMap.remove name fields))

let field_opt ~name (body : 'a t) : 'a option fields =
 fun _ fields ->
  match StringMap.find_opt name fields with
  | None -> Ok (None, fields)
  | Some [] -> failwith "Impossible"
  | Some ((pos, _) :: _ :: _) -> Error (pos, Printf.sprintf "Multiple definitions of %S" name)
  | Some [ (pos, xs) ] ->
      parse_til_empty body (Span.finish pos) xs
      |> Result.map (fun x -> (Some x, StringMap.remove name fields))

let field_repeated ~name (body : 'a t) : 'a list fields =
 fun _ fields ->
  match StringMap.find_opt name fields with
  | None -> Ok ([], fields)
  | Some xs ->
      let rec go res = function
        | [] -> Ok (res, StringMap.remove name fields)
        | (pos, xs) :: xss -> (
          match parse_til_empty body (Span.finish pos) xs with
          | Error e -> Error e
          | Ok x -> go (x :: res) xss)
      in
      go [] xs

let parse state (body : 'a t) last : ('a, Span.t * string) result =
  let rec get_last : Sexp.t list -> Span.t = function
    | [] -> last
    | [ x ] -> pos_of x
    | _ :: xs -> get_last xs
  in
  parse_til_empty body (get_last state) state

let parse_buf (file : Span.filename) (lexbuf : Lexing.lexbuf) body =
  Span.Lines.using file lexbuf @@ fun lines ->
  try
    let rec go stack (head : Sexp.t list) head_start : Sexp.t list =
      let start = lexbuf.lex_curr_p in
      let token = Lexer.token lines lexbuf in
      match token with
      | Skip -> go stack head head_start
      | String x ->
          go stack (Atom (Span.of_pos2 lines start lexbuf.lex_curr_p, x) :: head) head_start
      | Open -> go ((head, head_start) :: stack) [] start
      | Close -> (
        match stack with
        | [] -> raise (Lexer.Error ("Closing ')' with no matching '('", start, lexbuf.lex_curr_p))
        | (xs, xs_pos) :: stack ->
            go stack
              (List (Span.of_pos2 lines head_start lexbuf.lex_curr_p, List.rev head) :: xs)
              xs_pos)
      | End -> (
        match stack with
        | [] -> List.rev head
        | _ :: _ ->
            let head_start' = { head_start with pos_cnum = head_start.pos_cnum + 1 } in
            raise (Lexer.Error ("Unclosed '('", head_start, head_start')))
    in
    let value = go [] [] lexbuf.lex_curr_p in
    parse value body (Span.of_pos2 lines lexbuf.lex_curr_p lexbuf.lex_curr_p)
  with Lexer.Error (err, start, fin) -> Error (Span.of_pos2 lines start fin, err)
