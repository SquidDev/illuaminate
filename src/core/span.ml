type filename =
  { name : string;
    path : string
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

let pp out { filename; start_line; start_col; finish_line; finish_col; _ } =
  Format.fprintf out "%s[%d:%d-%d:%d]" filename.name start_line start_col finish_line finish_col

let show = Format.asprintf "%a" pp

let of_pos2 filename (start : Lexing.position) (fin : Lexing.position) =
  { filename;
    start_line = start.pos_lnum;
    start_col = start.pos_cnum - start.pos_bol;
    start_bol = start.pos_bol;
    finish_line = fin.pos_lnum;
    finish_col = fin.pos_cnum - fin.pos_bol;
    finish_bol = fin.pos_bol
  }

let of_span2 { filename; start_line; start_col; start_bol; _ }
    { finish_line; finish_col; finish_bol; _ } =
  { filename; start_line; start_col; start_bol; finish_line; finish_col; finish_bol }

type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
