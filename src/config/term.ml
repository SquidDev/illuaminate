module Converter = struct
  type 'a t = 'a Parser.t * ('a -> Sexp.raw)

  let bool : bool t = (Parser.bool, fun x -> Atom (string_of_bool x))

  let string : string t = (Parser.string, fun x -> Atom x)

  let float : float t = (Parser.float, fun x -> Atom (string_of_float x))

  let int : int t = (Parser.int, fun x -> Atom (string_of_int x))

  let list (f, t) : 'a list t = (Parser.list f, fun x -> List (List.map t x))

  let atom ~ty parse print : 'a t = (Parser.atom_res ~ty parse, fun x -> Atom (print x))
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

let rec write_term : type a. Format.formatter -> a t -> int -> int =
 fun out t prev ->
  let open Format in
  match t with
  | Node { name; comment; body } ->
      (* Write 'prev' lines between the previous entry and this one. *)
      for _ = 1 to prev do
        pp_force_newline out ()
      done;
      (* Comment *)
      pp_print_string out ";; ";
      pp_print_string out comment;
      pp_force_newline out ();
      (* Value *)
      pp_open_box out 2;
      pp_print_string out "(";
      pp_print_string out name;
      write_group out body;
      pp_print_string out ")";
      pp_close_box out ();
      (* The next item should have one blank line. *)
      2
  | Map (_, t) -> write_term out t prev
  | Pair (l, r) -> write_term out l prev |> write_term out r
  | Const _ -> prev

and write_group : 'a. Format.formatter -> 'a body -> unit =
 fun out t ->
  match t with
  | Field { default; converter = _, t } ->
      Format.pp_print_space out ();
      Sexp.pp out (t default)
  | Group g -> write_term out g 1 |> ignore

let write_default out term = write_term out term 0 |> ignore

let rec to_parser : type a. a t -> a Parser.fields =
  let open Parser in
  function
  | Node { name; body = Field { default; converter = parse, _ }; _ } ->
      let+ value = field_opt ~name parse in
      Option.value ~default value
  | Node { name; body = Group body; _ } ->
      let+ value = field_opt ~name (to_parser body |> fields) in
      Option.value ~default:(default body) value
  | Const k -> Parser.const k
  | Map (f, x) -> ( let+ ) (to_parser x) f
  | Pair (x, y) -> ( and+ ) (to_parser x) (to_parser y)
