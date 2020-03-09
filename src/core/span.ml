module Filename = struct
  type t =
    { name : string;
      path : Fpath.t option;
      id : string;
      hash : int
    }

  let mk ?path ?name id =
    ( match path with
    | Some p when not (Fpath.is_abs p) -> invalid_arg "Filename.mk: path must be absolute"
    | _ -> () );
    { name = Option.value ~default:id name; path; id; hash = Hashtbl.hash id }

  let compare l r = String.compare l.id r.id

  let hash x = x.hash

  let equal l r = l == r || (l.hash = r.hash && l.id = r.id)

  let pp out f = Format.pp_print_string out f.id
end

type filename = Filename.t =
  { name : string;
    path : Fpath.t option;
    id : string;
    hash : int
  }

type t =
  { filename : filename;
    start_line : int;
    start_col : int;
    start_bol : int;
    finish_line : int;
    finish_col : int;
    finish_bol : int
  }
[@@deriving illuaminateDeriving_lens]

let filename = T.filename.get

let start_line = T.start_line

let start_col = T.start_col

let start_bol = T.start_bol.get

let finish_line = T.finish_line

let finish_col = T.finish_col

let pp out { filename; start_line; start_col; finish_line; finish_col; _ } =
  Format.fprintf out "%s[%d:%d-%d:%d]" filename.name start_line start_col finish_line finish_col

let compare a b =
  if a.filename <> b.filename then String.compare a.filename.name b.filename.name
  else Int.compare (a.start_col + a.start_bol) (b.start_col + b.start_bol)

let of_pos2 filename (start : Lexing.position) (fin : Lexing.position) =
  { filename;
    start_line = start.pos_lnum;
    start_col = start.pos_cnum - start.pos_bol + 1;
    start_bol = start.pos_bol;
    finish_line = fin.pos_lnum;
    finish_col = fin.pos_cnum - fin.pos_bol;
    finish_bol = fin.pos_bol
  }

let of_span2 { filename; start_line; start_col; start_bol; _ }
    { finish_line; finish_col; finish_bol; _ } =
  { filename; start_line; start_col; start_bol; finish_line; finish_col; finish_bol }

let finish s =
  { s with start_line = s.finish_line; start_col = s.finish_col; start_bol = s.finish_bol }

type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
