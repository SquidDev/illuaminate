type t = Pattern.t

let parse = Parse.parse

let matches = Pattern.matches

module Paths = struct
  let to_absolute ?cwd path =
    if Filename.is_relative path then
      let cwd =
        match cwd with
        | Some x -> x
        | None -> Sys.getcwd ()
      in
      Filename.concat cwd path
    else path

  let make_relative ~path ~dir =
    let rec unzip ps ds =
      match (ps, ds) with
      | [], [] -> Some (Filename.current_dir_name)
      | ps, [] -> Some (String.concat Filename.dir_sep ps)
      | p :: ps, d :: ds when p = d -> unzip ps ds
      | _ , _ :: _ -> None

    in
    let path = CCString.split ~by:Filename.dir_sep path
    and dir = CCString.split ~by:Filename.dir_sep dir in
    unzip path dir
end
