open Pattern

let src = Logs.Src.create ~doc:"File pattern parsing and matching" __MODULE__

module Log = (val Logs.src_log src)

type nonrec t = t

let parse = Parse.parse

let matches path { absolute; pattern } =
  let str = Fpath.to_string path |> CCString.replace ~sub:Fpath.dir_sep ~by:"/" in
  let rec match_at idx =
    if idx >= String.length str then false
    else
      match String.index_from_opt str idx '/' with
      | None -> false
      | Some idx -> Re.execp ~pos:(idx + 1) pattern str || match_at (idx + 1)
  in
  Re.execp pattern str || ((not absolute) && match_at 0)

(** A set of files we'll always skip over. Just saves us iterating over VCS directories and whatnot. *)
let ignored_files = List.to_seq [ ".git"; ".svn" ] |> Seq.map (fun x -> (x, ())) |> Hashtbl.of_seq

let iter_all iter ?path ~root pats =
  (* TODO: We should really be smarter about this. For instance, if we have a pattern /foo/bar/baz/,
     then we can start looking in foo/bar/baz instead. *)
  let rec visit path =
    let path_s = Fpath.to_string path in
    if not (Sys.file_exists path_s) then
      Log.warn (fun f -> f "File %a does not exist" Fpath.pp path)
    else if Sys.is_directory path_s then (
      Log.debug (fun f -> f "Scanning directory %a" Fpath.pp path);
      match Sys.readdir path_s with
      | files ->
          Array.iter
            (fun child -> if not (Hashtbl.mem ignored_files child) then visit Fpath.(path / child))
            files
      | exception Sys_error e -> Log.err (fun f -> f "Error when scanning %a:%s " Fpath.pp path e) )
    else if
      Fpath.is_rooted ~root path
      && List.exists (Fpath.relativize ~root path |> Option.get |> matches) pats
    then iter path
  in

  let path = Option.value ~default:root path in
  Log.info (fun f -> f "Listing files from %a" Fpath.pp path);
  visit path

let iter iter ?path ~root p = iter_all iter ?path ~root [ p ]
