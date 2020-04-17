open IlluaminateCore
module FileStore = IlluaminateData.Programs.FileStore
module StringMap = Map.Make (String)
module Config = IlluaminateConfigFormat

let src = Logs.Src.create ~doc:"Loads files from directories." __MODULE__

module Log = (val Logs.src_log src)

type file =
  { root : Fpath.t;
    path : Fpath.t;
    file : Span.filename;
    config : Config.t;
    parsed : Syntax.program option
  }

type t =
  { relative : Fpath.t;
    errors : Error.t;
    mutable configs : Config.t option StringMap.t
  }

let normalise_p p = Fpath.(normalize p |> rem_empty_seg)

let create ?root errors =
  let relative = CCOpt.get_lazy (fun () -> Fpath.(Sys.getcwd () |> v |> normalise_p)) root in
  { relative; errors; configs = StringMap.empty }

let mk_name ~loader:{ relative; _ } path =
  let path_s = Fpath.to_string path in
  let name =
    Fpath.(relativize ~root:relative path |> Option.map (fun f -> normalise_p f |> to_string))
  in
  Span.Filename.mk ?name ~path path_s

let rec get_config ~loader dir =
  let dir = normalise_p dir in
  match StringMap.find_opt (Fpath.to_string dir) loader.configs with
  | Some c -> c
  | None ->
      Log.debug (fun f -> f "Searching for config in %a" Fpath.pp dir);
      let config_path = Fpath.(dir / "illuaminate.sexp") in
      let config_path_s = Fpath.to_string config_path in
      let parent = Fpath.parent dir in
      let c =
        if Sys.file_exists config_path_s && not (Sys.is_directory config_path_s) then (
          Log.info (fun f -> f "Loading config from %S" config_path_s);
          Config.of_file loader.errors (mk_name ~loader config_path) )
        else if Fpath.is_root dir || dir = parent then Some Config.default
        else get_config ~loader parent
      in
      loader.configs <- StringMap.add (Fpath.to_string dir) c loader.configs;
      c

let get_config_for ~loader path =
  let str = Fpath.to_string path in
  if Sys.is_directory str then get_config ~loader path else get_config ~loader (Fpath.parent path)

(** Parse a file and report its errors. *)
let parse ~loader:{ errors; _ } ({ Span.id; _ } as file) =
  CCIO.with_in id (fun channel ->
      let lexbuf = Lexing.from_channel channel in
      match IlluaminateParser.program file lexbuf with
      | Error err ->
          IlluaminateParser.Error.report errors err.span err.value;
          None
      | Ok tree -> Some tree)

let do_load_from ~loader ~files ~file_store ~config root =
  (* TODO: The behaviour of this is technically incorrect. What we really need to do is load all
     files within this project, and then only display lints/fixes for the selected ones.*)
  Log.info (fun f -> f "Loading files from %a" Fpath.pp root);
  let add path =
    let path_s = Fpath.to_string path in
    if not (StringMap.mem path_s !files) then (
      Log.debug (fun f -> f "Using source file %S" path_s);
      (* TODO: Warn on duplicate files. *)
      let file = mk_name ~loader path in
      let parsed = parse ~loader file in
      FileStore.update file_store file parsed;
      files := StringMap.add path_s { root; path; file; config; parsed } !files )
  in

  Config.files add config root

let builder files file_store builder =
  builder
  |> IlluaminateData.Builder.oracle IlluaminateData.Programs.Context.key (fun file _ ->
         let { root; config; _ } = StringMap.find file.Span.id files in
         { root = Some root; config = Config.get_store config })
  |> FileStore.builder file_store

let keys m = StringMap.to_seq m |> Seq.map snd |> List.of_seq

let load_from ~loader path =
  let path = Fpath.(append loader.relative path |> normalise_p) in
  get_config_for ~loader path
  |> Option.map @@ fun config ->
     let files = ref StringMap.empty in
     let file_store = FileStore.create () in
     do_load_from ~loader ~files ~file_store ~config path;
     (config, keys !files, builder !files file_store)

let load_from_many ~loader paths =
  let files = ref StringMap.empty in
  let file_store = FileStore.create () in
  paths
  |> List.iter (fun path ->
         let path = Fpath.(append loader.relative path |> normalise_p) in
         get_config_for ~loader path
         |> Option.iter (fun config -> do_load_from ~loader ~files ~file_store ~config path));
  (keys !files, builder !files file_store)
