open Pattern

let mk segments ~has_sep =
  let rec build xs = function
    | [] -> xs
    | Anything :: (Anything :: _ as segs) -> build xs segs
    | x :: segs -> build (x :: xs) segs
  in
  match build [] segments with
  | Anything :: _ as xs -> xs
  | xs -> if has_sep then xs else Anything :: xs

type builder =
  | Empty
  | Literal of Buffer.t
  | Regex of Re.t list

let add_char c : builder -> builder = function
  | Empty ->
      let b = Buffer.create 8 in
      Buffer.add_char b c; Literal b
  | Literal buf as b -> Buffer.add_char buf c; b
  | Regex rs -> Regex (Re.char c :: rs)

let add_re r : builder -> builder = function
  | Empty -> Regex [ r ]
  | Literal buf -> Regex [ r; Re.str (Buffer.contents buf) ]
  | Regex rs -> Regex (r :: rs)

let to_segment : builder -> segment option = function
  | Empty -> None
  | Literal b -> Some (Literal (Buffer.contents b))
  | Regex rs -> Some (Regex (List.rev rs |> Re.seq |> Re.whole_string |> Re.compile))

let not_sep = Re.(diff any (char '/'))
let ( **: ) x xs = CCList.cons_maybe (to_segment x) xs
