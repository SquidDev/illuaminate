open IlluaminateCore
open IlluaminateSemantics
module FileMap = Map.Make (Fpath)

type file =
  { path : Fpath.t;
    file : Span.filename;
    file_id : Data.Files.id option;
    config : Config.t;
    parsed : Syntax.program option
  }

type t =
  { root : Fpath.t;
    errors : Error.t;
    mutable configs : Config.t option FileMap.t
  }

let create ?root errors =
  let root = CCOpt.get_lazy (fun () -> Fpath.(Sys.getcwd () |> v |> normalize)) root in
  { root; errors; configs = FileMap.empty }

let mk_name ~loader:{ root; _ } path =
  let path_s = Fpath.to_string path in
  let name =
    Fpath.(
      relativize ~root path |> Option.fold ~none:path_s ~some:(fun x -> normalize x |> to_string))
  in
  { Span.path = path_s; name }

let rec get_config ~loader dir =
  let dir = Fpath.normalize dir in
  match FileMap.find_opt dir loader.configs with
  | Some c -> c
  | None ->
      let config_path = Fpath.(dir / "illuaminate.sexp") in
      let config_path_s = Fpath.to_string config_path in
      let parent = Fpath.parent dir in
      let c =
        if Sys.file_exists config_path_s && not (Sys.is_directory config_path_s) then
          Config.of_file loader.errors (mk_name ~loader config_path)
        else if Fpath.is_root dir || dir = parent then Some Config.default
        else get_config ~loader parent
      in
      loader.configs <- FileMap.add dir c loader.configs;
      c

let get_config_for ~loader path =
  let str = Fpath.to_string path in
  if Sys.is_directory str then get_config ~loader path else get_config ~loader (Fpath.parent path)

(** Parse a file and report its errors. *)
let parse ~loader:{ errors; _ } ({ Span.path; _ } as file) =
  CCIO.with_in path (fun channel ->
      let lexbuf = Lexing.from_channel channel in
      match IlluaminateParser.parse file lexbuf with
      | Error err ->
          IlluaminateParser.Error.report errors err.span err.value;
          None
      | Ok tree -> Some tree)

let do_load_from ~loader ~files ~file_store path =
  get_config_for ~loader path
  |> Option.map @@ fun config ->
     let rec add is_root path =
       let path_s = Fpath.to_string path in
       if Sys.is_directory path_s then
         Sys.readdir path_s |> Array.iter (fun child -> add false Fpath.(path / child))
       else if is_root || Fpath.has_ext ".lua" path then
         let file = mk_name ~loader path in
         if Config.is_source config path then
           let parse = parse ~loader file in
           let file_id =
             parse
             |> Option.map (fun x ->
                    let files, id = Data.Files.add x !file_store in
                    file_store := files;
                    id)
           in
           files := { path; file; config; parsed = parse; file_id } :: !files
     in

     add true path; config

let load_from ~loader path =
  let files = ref [] and file_store = ref (Data.Files.create ()) in
  do_load_from ~loader ~files ~file_store path
  |> Option.map (fun config -> (config, !files, !file_store))

let load_from_many ~loader paths =
  let files = ref [] and file_store = ref (Data.Files.create ()) in
  List.iter (fun path -> do_load_from ~loader ~files ~file_store path |> ignore) paths;
  (!files, !file_store)
