type filename = Illuaminate.File_id.t = private
  { name : string;
    path : Fpath.t option;
    id : string;
    hash : int
  }

module Lines = struct
  type t =
    { file : filename;
      map : Illuaminate.Position_map.t;
      builder : Illuaminate.Position_map.Builder.t option
    }

  let new_line { builder; _ } = Illuaminate.Position_map.Builder.new_line (Option.get builder)

  let using file buf f =
    Illuaminate.Position_map.Builder.using buf (fun builder map ->
        f { file; builder = Some builder; map })
    |> fst

  let position_map lines = lines.map

  let get_line lines offset =
    let line, _ = Illuaminate.Position_map.position_of lines.map (Pos offset) in
    (line :> int)

  let get_col lines offset =
    let _, col = Illuaminate.Position_map.position_of lines.map (Pos offset) in
    (col :> int)

  let get_pos lines offset =
    (Illuaminate.Position_map.position_of lines.map (Pos offset) :> int * int)

  let over_offset f lines offset =
    let offset = f offset in
    let (Pos max) = Illuaminate.Position_map.max_position lines.map in
    if offset < 0 || offset > max then
      Printf.sprintf "offset %d invalid: 0 <= offset <= %d" offset max |> invalid_arg;
    offset
end

type t =
  { lines : Lines.t;
    start : int;
    finish : int
  }

let unpos (Illuaminate.Position_map.Pos x) = x
let filename x = x.lines.file
let position_map x = x.lines.map

let to_error_position (span : t) : Illuaminate.Error.Position.t =
  { file = span.lines.file;
    position_map = span.lines.map;
    start = Pos span.start;
    finish = Pos span.finish
  }

let of_error_position
    ({ file; position_map; start = Pos start; finish = Pos finish } : Illuaminate.Error.Position.t)
    =
  { lines = { file; map = position_map; builder = None }; start; finish }

let start_line { lines; start; _ } = Lines.get_line lines start

let start_bol { lines; start; _ } =
  Illuaminate.Position_map.position_bol lines.map (Pos start) |> unpos

let start_col { lines; start; _ } = Lines.get_col lines start
let start_pos { lines; start; _ } = Lines.get_pos lines start

let start_offset =
  { Illuaminate.Lens.get = (fun { start; _ } -> start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_offset f lines start })
  }

let start_offset' { start; _ } = Illuaminate.Position_map.Pos start
let finish_line { lines; finish; _ } = Lines.get_line lines finish
let finish_col { lines; finish; _ } = Lines.get_col lines finish
let finish_pos { lines; finish; _ } = Lines.get_pos lines finish

let finish_offset =
  { Illuaminate.Lens.get = (fun { finish; _ } -> finish);
    over =
      (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_offset f lines finish })
  }

let pp out span =
  Format.fprintf out "%s[%d:%d-%d:%d]" (filename span).name (start_line span) (start_col span)
    (finish_line span) (finish_col span)

let show = Format.asprintf "%a" pp

let compare a b =
  if a.lines.file <> b.lines.file then Illuaminate.File_id.compare a.lines.file b.lines.file
  else if a.start <> b.start then Int.compare a.start b.start
  else Int.compare a.finish b.finish

let hash a = (a.start * 31) + (a.finish - a.start)

let of_pos2 lines (start : Lexing.position) (fin : Lexing.position) =
  { lines;
    start = start.pos_cnum;
    (* The [fin] will /generally/ be one after our actual finish position, thus we decrement it by
       one. However, we need to special-case eof, as that's a 0-width token.*)
    finish = (if fin.pos_cnum <= start.pos_cnum then start.pos_cnum else fin.pos_cnum - 1)
  }

let of_span2 ({ lines; start; _ } as s) ({ finish; _ } as f) =
  if s == f then s
  else (
    if start > finish then raise (Invalid_argument "start > finish");
    { lines; start; finish })

let start s = { s with finish = s.start }
let finish s = { s with start = s.finish }

type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
