module Converter = struct
  open Sexplib.Conv

  type 'a t = 'a Parser.t * ('a -> Sexplib.Sexp.t)

  let bool = (Parser.bool, sexp_of_bool)

  let string = (Parser.string, sexp_of_string)

  let float = (Parser.float, sexp_of_float)

  let int = (Parser.int, sexp_of_int)

  let list (f, t) = (Parser.list f, sexp_of_list t)
end

type 'a body =
  | Field of
      { default : 'a;
        converter : 'a Converter.t
      }
  | Group of 'a t

and 'a t =
  | Node of
      { name : string;
        comment : string;
        body : 'a body
      }
  | Const : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Pair : 'a t * 'b t -> ('a * 'b) t

let group ~name ~comment body = Node { name; comment; body = Group body }

let field ~name ~comment ~default converter =
  Node { name; comment; body = Field { default; converter } }

let unit = Const ()

let const x = Const x

let ( let+ ) node map = Map (map, node)

let ( and+ ) a b = Pair (a, b)

let rec default : type a. a t -> a = function
  | Node { body; _ } -> default_body body
  | Map (f, t) -> f (default t)
  | Pair (l, r) -> (default l, default r)
  | Const x -> x

and default_body : type a. a body -> a = function
  | Field { default; _ } -> default
  | Group g -> default g

let rec write_term : type a. Format.formatter -> a t -> unit =
 fun out t ->
  let open Format in
  match t with
  | Node { name; comment; body } ->
      (* Comment *)
      pp_print_string out ";; ";
      pp_print_string out comment;
      pp_force_newline out ();
      (* Value *)
      pp_open_box out 1;
      pp_print_string out "(";
      pp_print_string out name;
      pp_print_space out ();
      write_group out body;
      pp_print_string out ")";
      pp_close_box out ()
  | Map (_, t) -> write_term out t
  | Pair (l, r) -> write_term out l; pp_force_newline out (); write_term out r
  | Const _ -> ()

and write_group : 'a. Format.formatter -> 'a body -> unit =
 fun out t ->
  let open Format in
  match t with
  | Field { default; converter = _, t } -> Sexplib.Sexp.pp_hum_indent 2 out (t default)
  | Group g ->
      pp_open_box out 1;
      pp_print_string out "(";
      write_term out g;
      pp_print_string out ")";
      pp_close_box out ()

let write_default out term = write_term out term; Format.pp_print_newline out ()

let rec to_parser : type a. a t -> a Parser.fields =
  let open Parser in
  function
  | Node { name; body = Field { default; converter = (parse, _) }; _ } ->
     let+ value = field_opt ~name parse in
     Option.value ~default value
  | Node { name; body = Group body; _ } ->
     let+ value = field_opt ~name (to_parser body |> fields) in
     Option.value ~default:(default body) value
  | Const k -> Parser.const k
  | Map (f, x) -> (let+) (to_parser x) f
  | Pair (x, y) -> (and+) (to_parser x) (to_parser y)
