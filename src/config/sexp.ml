open IlluaminateCore

type t =
  | Atom of Span.t * string
  | List of Span.t * t list

type raw =
  | Atom of string
  | List of raw list

let is_raw_string str =
  let rec go i =
    if i < 0 then true
    else
      let c = str.[i] in
      c > ' ' && c <= '~' && go (i - 1)
  in
  go (String.length str - 1)

let rec pp out : raw -> unit =
  let open Format in
  function
  | Atom x -> if is_raw_string x then pp_print_string out x else fprintf out "%S" x
  | List [] -> pp_print_string out "()"
  | List (x :: xs) -> pp_open_box out 2; pp_print_string out "("; pp out x; pp_xs out xs

and pp_xs out : raw list -> unit =
  let open Format in
  function
  | [] -> pp_print_string out ")"; pp_close_box out ()
  | x :: xs -> pp_print_space out (); pp out x; pp_xs out xs
