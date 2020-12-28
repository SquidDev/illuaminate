type t = Lex_template.t list

let parse allowed p =
  Lexing.from_string p
  |> Lex_template.main (Buffer.create 8) []
  |> CCResult.flat_map @@ fun r ->
     let rec go = function
       | [] -> Ok r
       | Lex_template.Raw _ :: xs -> go xs
       | Key k :: xs ->
           if List.mem k allowed then go xs else Error (Printf.sprintf "Unknown key %S" k)
     in
     go r

let apply fn template =
  let out = Buffer.create 32 in
  let rec go = function
    | [] -> Some (Buffer.contents out)
    | Lex_template.Raw x :: xs -> Buffer.add_string out x; go xs
    | Key k :: xs -> (
      match fn k with
      | Some x -> Buffer.add_string out x; go xs
      | None -> None )
  in
  go template

let pp out = List.iter (Lex_template.pp out)

let converter allowed = (parse allowed, Format.asprintf "%a" pp)
