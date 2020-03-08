type segment =
  | Anything  (** The [**] pattern. Matches 0 or more segments. *)
  | Literal of string  (** A segment which must have exactly this contents. *)
  | Regex of Re.re  (** A segment which must match this regex. *)

let pp_segment out = function
  | Anything -> Format.fprintf out "**"
  | Literal l -> Format.fprintf out "%s" l
  | Regex _ -> Format.fprintf out "[Regex]"

let rec pp out = function
  | [] -> ()
  | [ x ] -> pp_segment out x
  | x :: xs -> pp_segment out x; Format.pp_print_char out '/'; pp out xs
