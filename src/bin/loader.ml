open IlluaminateCore
open IlluaminateSemantics
module StringMap = Map.Make (String)

type file =
  { root : Fpath.t;
    path : Fpath.t;
    file : Span.filename;
    file_id : Data.Files.id option;
    config : Config.t;
    parsed : Syntax.program option
  }

type t =
  { relative : Fpath.t;
    errors : Error.t;
    mutable configs : Config.t option StringMap.t
  }

let create ?root errors =
  let relative = CCOpt.get_lazy (fun () -> Fpath.(Sys.getcwd () |> v |> normalize)) root in
  { relative; errors; configs = StringMap.empty }

let mk_name ~loader:{ relative; _ } path =
  let path_s = Fpath.to_string path in
  let name =
    Fpath.(
      relativize ~root:relative path
      |> Option.fold ~none:path_s ~some:(fun x -> normalize x |> to_string))
  in
  { Span.path = path_s; name }

let rec get_config ~loader dir =
  let dir = Fpath.normalize dir in
  match StringMap.find_opt (Fpath.to_string dir) loader.configs with
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
      loader.configs <- StringMap.add (Fpath.to_string dir) c loader.configs;
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

let do_load_from ~loader ~files ~file_store ~config root =
  let rec add is_root path =
    let path_s = Fpath.to_string path in
    if Sys.is_directory path_s then
      Sys.readdir path_s |> Array.iter (fun child -> add false Fpath.(path / child))
    else if is_root || Fpath.has_ext ".lua" path then
      let file = mk_name ~loader path in
      if Config.is_source config path && not (StringMap.mem path_s !files) then
        (* TODO: Warn on duplicate files. *)
        let parse = parse ~loader file in
        let file_id =
          parse
          |> Option.map (fun x ->
                 let files, id = Data.Files.add x !file_store in
                 file_store := files;
                 id)
        in
        files := StringMap.add path_s { root; path; file; config; parsed = parse; file_id } !files
  in

  add true root

let data_for files =
  Data.Files.create @@ fun file ->
  let { root; config; _ } = StringMap.find file.Span.path !files in
  { Data.root; config = Config.get_store config }

let keys m = StringMap.to_seq m |> Seq.map snd |> List.of_seq

let load_from ~loader path =
  get_config_for ~loader path
  |> Option.map @@ fun config ->
     let files = ref StringMap.empty in
     let file_store = ref (data_for files) in
     do_load_from ~loader ~files ~file_store ~config path;
     (config, keys !files, !file_store)

let load_from_many ~loader paths =
  let files = ref StringMap.empty in
  let file_store = ref (data_for files) in
  paths
  |> List.iter (fun path ->
         get_config_for ~loader path
         |> Option.iter (fun config -> do_load_from ~loader ~files ~file_store ~config path));
  (keys !files, !file_store)
