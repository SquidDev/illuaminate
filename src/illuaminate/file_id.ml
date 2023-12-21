type t =
  { name : string;
    path : Fpath.t option;
    id : string;
    hash : int
  }

let mk ?path ?name id =
  (match path with
  | Some p when not (Fpath.is_abs p) ->
      Format.asprintf "Filename.mk: path %a must be absolute" Fpath.pp p |> invalid_arg
  | _ -> ());
  { name = Option.value ~default:id name; path; id; hash = Hashtbl.hash id }

let compare l r = String.compare l.id r.id
let hash x = x.hash
let equal l r = l == r || (l.hash = r.hash && l.id = r.id)
let pp out f = Format.pp_print_string out f.id
