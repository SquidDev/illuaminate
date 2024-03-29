open Illuaminate
open IlluaminateCore
module FileStore = IlluaminateData.Programs.FileStore
module StringMap = Map.Make (String)
module Config = IlluaminateConfigFormat

let src = Logs.Src.create ~doc:"Loads files from directories." __MODULE__

module Log = (val Logs.src_log src)

type file =
  { root : Fpath.t;
    path : Fpath.t;
    file : File_id.t;
    config : Config.t;
    body : File.t option
  }

type t =
  { relative : Fpath.t;
    mutable errors : Illuaminate.Error.t list;
    mutable configs : Config.t option StringMap.t
  }

let normalise_p p = Fpath.(normalize p |> rem_empty_seg)

let create ?root () =
  let relative = CCOption.get_lazy (fun () -> Fpath.(Sys.getcwd () |> v |> normalise_p)) root in
  { relative; errors = []; configs = StringMap.empty }

let mk_name ~loader:{ relative; _ } path =
  let path_s = Fpath.to_string path in
  let name =
    Fpath.(relativize ~root:relative path |> Option.map (fun f -> normalise_p f |> to_string))
  in
  File_id.mk ?name ~path path_s

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
          match Config.of_file (mk_name ~loader config_path) with
          | Ok x -> Some x
          | Error { span; value = message } ->
              loader.errors <-
                Illuaminate.Error.simple ~code:"config:parse" ~severity:Error
                  (Span.to_error_position span) (fun f -> f "%s" message)
                :: loader.errors;
              None)
        else if Fpath.is_root dir || dir = parent then Some Config.default
        else get_config ~loader parent
      in
      loader.configs <- StringMap.add (Fpath.to_string dir) c loader.configs;
      c

let get_config_for ~loader path =
  let str = Fpath.to_string path in
  if Sys.is_directory str then get_config ~loader path else get_config ~loader (Fpath.parent path)

(** Parse a file and report its errors. *)
let parse ~loader ({ File_id.id; _ } as file) =
  CCIO.with_in id (fun channel ->
      let lexbuf = Lexing.from_channel channel in
      match IlluaminateParser.program file lexbuf with
      | Error err ->
          loader.errors <- IlluaminateParser.Error.to_error err :: loader.errors;
          None
      | Ok tree -> Some (File.Lua tree))

let do_load_from ~loader ~files ~file_store ~config root =
  (* TODO: The behaviour of this is technically incorrect. What we really need to do is load all
     files within this project, and then only display lints/fixes for the selected ones.*)
  Log.info (fun f -> f "Loading files from %a" Fpath.pp root);
  let add_with path file body =
    let path_s = Fpath.to_string path in
    FileStore.update file_store file body;
    files := StringMap.add path_s { root; path; file; config; body } !files
  in
  let add path =
    let path_s = Fpath.to_string path in
    if not (StringMap.mem path_s !files) then
      (* TODO: Warn on duplicate files. *)
      match Fpath.get_ext path with
      | ".lua" ->
          Log.debug (fun f -> f "Using source file %S" path_s);
          let file = mk_name ~loader path in
          parse ~loader file |> add_with path file
      | ".md" ->
          Log.debug (fun f -> f "Using markdown file %S" path_s);
          let ({ File_id.id; _ } as file) = mk_name ~loader path in
          let attributes, contents =
            CCIO.with_in id @@ fun channel ->
            Lexing.from_channel channel |> IlluaminateParserMd.parse file
          in
          add_with path file (Some (Markdown { attributes; contents }))
      | _ -> ()
  in

  Config.files add config root

let add_rules files file_store builder =
  IlluaminateData.Builder.oracle IlluaminateData.Programs.Context.key
    (fun file _ ->
      match StringMap.find_opt file.File_id.id files with
      | Some { root; config; _ } -> { root = Some root; config = Config.get_store config }
      | None -> { root = None; config = Config.get_store Config.default })
    builder;
  FileStore.builder file_store builder

let keys m = StringMap.to_seq m |> Seq.map snd |> List.of_seq

let load_from ?root path =
  let loader = create ?root () in
  let path = Fpath.(append loader.relative path |> normalise_p) in
  let result =
    get_config_for ~loader path
    |> Option.map @@ fun config ->
       let files = ref StringMap.empty in
       let file_store = FileStore.create () in
       do_load_from ~loader ~files ~file_store ~config path;
       (config, keys !files, add_rules !files file_store)
  in
  (result, loader.errors)

let load_from_many ?root paths =
  let loader = create ?root () in
  let files = ref StringMap.empty in
  let file_store = FileStore.create () in
  paths
  |> List.iter (fun path ->
         let path = Fpath.(append loader.relative path |> normalise_p) in
         get_config_for ~loader path
         |> Option.iter (fun config -> do_load_from ~loader ~files ~file_store ~config path));
  (keys !files, add_rules !files file_store, loader.errors)
