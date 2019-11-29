type t =
  { absolute : bool;
    pattern : Str.regexp
  }

let matches str { absolute; pattern } =
  let rec match_at idx =
    if idx >= String.length str then false
    else
      match String.index_from_opt str idx '/' with
      | None -> false
      | Some idx -> Str.string_match pattern str (idx + 1) || match_at (idx + 1)
  in
  Str.string_match pattern str 0 || ((not absolute) && match_at 0)
