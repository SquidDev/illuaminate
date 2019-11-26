module S = Sexplib.Type_with_layout.Parsed
module StringMap = Map.Make (String)
module Pos = Sexplib.Src_pos.Absolute

let pos_of_t = function
  | S.Atom (pos, _, _) -> pos
  | S.List (pos, _, _) -> pos

let pos_of = function
  | S.Sexp t -> pos_of_t t
  | S.Comment (Plain_comment (pos, _)) -> pos
  | S.Comment (Sexp_comment (pos, _, _)) -> pos

let ( >>= ) = Result.bind

module Kind = struct
  type term = S.t_or_comment list

  (** All fields for each key, with their start and end position, in reverse order. *)
  type fields = (Pos.t * Pos.t * S.t_or_comment list) list StringMap.t
end

type ('a, 'kind) parser = Pos.t -> 'kind -> ('a * 'kind, Pos.t * string) result

type 'a t = ('a, Kind.term) parser

type 'a fields = ('a, Kind.fields) parser

let const x _ state = Ok (x, state)

let unit : (unit, 'k) parser = fun p -> const () p

let ( let+ ) (node : ('a, 'k) parser) (f : 'a -> 'b) : ('b, 'k) parser =
 fun pos state -> node pos state |> Result.map (fun (x, state) -> (f x, state))

let ( and+ ) a b pos state =
  a pos state >>= fun (x, state) -> b pos state |> Result.map (fun (y, state) -> ((x, y), state))

let atom_res ~ty parse =
  let rec go end_pos = function
    | [] -> Error (end_pos, Printf.sprintf "Expected %s, got nothing" ty)
    | S.Comment _ :: xs -> go end_pos xs
    | Sexp (List (pos, _, _)) :: _ -> Error (pos, Printf.sprintf "Expected %s, got list" ty)
    | Sexp (Atom (pos, txt, _)) :: xs -> (
      match parse txt with
      | Error e -> Error (pos, e)
      | Ok x -> Ok (x, xs) )
  in
  go

let atom ~ty parse =
  atom_res ~ty (fun x ->
      match parse x with
      | None -> Error (Printf.sprintf "Expected %s" ty)
      | Some x -> Ok x)

let bool : bool t = atom ~ty:"true or false" bool_of_string_opt

let int : int t = atom ~ty:"int" int_of_string_opt

let float : float t = atom ~ty:"number" float_of_string_opt

let string : string t = atom ~ty:"string" Option.some

let list (term : 'a t) : 'a list t =
  let rec parse_children end_pos xs = function
    | [] -> Ok (List.rev xs)
    | ss -> (
      match term end_pos ss with
      | Error e -> Error e
      | Ok (x, ss') -> parse_children end_pos (x :: xs) ss' )
  in
  let rec go end_pos = function
    | [] -> Error (end_pos, "Expected list, got nothing")
    | S.Comment _ :: xs -> go end_pos xs
    | Sexp (List (_, xs, end_pos)) :: state ->
        parse_children end_pos [] xs |> Result.map (fun x -> (x, state))
    | Sexp (Atom (pos, _, _)) :: _ -> Error (pos, "Expected list, got atom")
  in
  go

let rec check_empty x = function
  | [] -> Ok x
  | S.Comment _ :: xs -> check_empty x xs
  | S.Sexp t :: _ -> Error (pos_of_t t, "Expected ')', got another s-expr.")

let parse_til_empty body end_pos state =
  body end_pos state >>= fun (x, state) -> check_empty x state

let fields (body : 'a fields) : 'a t =
  let rec extract_field end_pos = function
    | [] -> Error (end_pos, "Expected key-value pair, but this list was empty.")
    | S.Comment _ :: xs -> extract_field end_pos xs
    | S.Sexp (List (pos, _, _)) :: _ ->
        Error (pos, "The first item of a key-value pair must be an atom.")
    | S.Sexp (Atom (_, t, _)) :: xs -> Ok (t, xs)
  in
  let rec gather res = function
    | [] -> Ok res
    | S.Comment _ :: xs -> gather res xs
    | S.Sexp (Atom (pos, _, _)) :: _ -> Error (pos, "Expected key-value pair, got a string.")
    | S.Sexp (List (start, ss, fin)) :: xs -> (
      match extract_field fin ss with
      | Error e -> Error e
      | Ok (key, value) ->
          gather
            (StringMap.update key
               (fun x -> Some ((start, fin, value) :: Option.value ~default:[] x))
               res)
            xs )
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
          let pos, _, _ = last xs in
          let key, pos =
            StringMap.fold
              (fun key xs (key', pos') ->
                let pos, _, _ = last xs in
                if Pos.geq pos' pos then (key, pos) else (key', pos'))
              state (key, pos)
          in
          Error (pos, Printf.sprintf "Unexpected key %S" key) )

let field ~name (body : 'a t) : 'a fields =
 fun pos fields ->
  match StringMap.find_opt name fields with
  | None -> Error (pos, Printf.sprintf "Missing %s field" name)
  | Some [] -> failwith "Impossible"
  | Some ((pos, _, _) :: _ :: _) -> Error (pos, Printf.sprintf "Multiple definitions of %S" name)
  | Some [ (_, end_pos, xs) ] ->
      parse_til_empty body end_pos xs |> Result.map (fun x -> (x, StringMap.remove name fields))

let field_opt ~name (body : 'a t) : 'a option fields =
 fun _ fields ->
  match StringMap.find_opt name fields with
  | None -> Ok (None, fields)
  | Some [] -> failwith "Impossible"
  | Some ((pos, _, _) :: _ :: _) -> Error (pos, Printf.sprintf "Multiple definitions of %S" name)
  | Some [ (_, end_pos, xs) ] ->
      parse_til_empty body end_pos xs
      |> Result.map (fun x -> (Some x, StringMap.remove name fields))

let field_repeated ~name (body : 'a t) : 'a list fields =
 fun _ fields ->
  match StringMap.find_opt name fields with
  | None -> Ok ([], fields)
  | Some xs ->
      let rec go res = function
        | [] -> Ok (res, StringMap.remove name fields)
        | (_, end_pos, xs) :: xss -> (
          match parse_til_empty body end_pos xs with
          | Error e -> Error e
          | Ok x -> go (x :: res) xss )
      in
      go [] xs

let parse state (body : 'a t) : ('a, Pos.t * string) result =
  let rec get_last : S.t_or_comment list -> Pos.t = function
    | [] -> { row = 1; col = 1 }
    | [ x ] -> pos_of x
    | _ :: xs -> get_last xs
  in
  parse_til_empty body (get_last state) state

type pos = Sexplib.Src_pos.Absolute.t =
  { row : int;
    col : int
  }

let parse_buf buf body =
  let value = Sexplib.(Parser_with_layout.sexps_abs Lexer.main_with_layout buf) in
  parse value body
