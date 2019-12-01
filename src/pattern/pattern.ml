type t =
  { absolute : bool;
    pattern : Re.re
  }

let matches str { absolute; pattern } =
  let rec match_at idx =
    if idx >= String.length str then false
    else
      match String.index_from_opt str idx '/' with
      | None -> false
      | Some idx -> Re.execp ~pos:(idx + 1) pattern str || match_at (idx + 1)
  in
  Re.execp pattern str || ((not absolute) && match_at 0)
