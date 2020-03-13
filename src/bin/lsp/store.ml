open IlluaminateCore
open Lsp
module Data = IlluaminateData
module UriTbl = Hashtbl.Make (Uri)
module Config = IlluaminateConfigFormat

let src = Logs.Src.create ~doc:"Loading and storing of local files" __MODULE__

module Log = (val Logs.src_log src)

module Filename = struct
  let protocol =
    match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"

  let get_path uri = Uri.to_string uri |> CCString.chop_prefix ~pre:protocol |> Option.map Fpath.v

  let of_uri uri = Span.Filename.mk ?path:(get_path uri) (Uri.to_string uri)

  let to_uri_json { Span.id; _ } : Yojson.Safe.t = `String id

  let to_uri { Span.id; _ } = Uri.t_of_yojson (`String id)

  let of_fpath path = Span.Filename.mk ~path (Fpath.to_string path |> Uri.of_path |> Uri.to_string)
end

module FileDigest = struct
  type t = Digest.t * Mtime.t

  let default_duration = Mtime.Span.of_uint64_ns 5_000_000_000L (* 5s *)

  (** Read a file and perform some processing function (such as parsing) if the contents has changed
      since last time we checked. Limited to 5 seconds. *)
  let with_change ?(delay = default_duration) ~process ~path digest =
    let path_s = Fpath.to_string path in
    let error msg =
      Log.err (fun f -> f "Error reading file %S (%s)" path_s msg);
      None
    in
    let result ~digest result = Some ((digest, Mtime_clock.now ()), result) in
    let read ~new_digest =
      match open_in path_s with
      | exception Sys_error e -> error e
      | ic -> (
        match process ic with
        | res ->
            close_in ic;
            result ~digest:new_digest (Some res)
        | exception Sys_error e -> close_in_noerr ic; error e
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            close_in ic;
            Printexc.raise_with_backtrace e bt )
    in

    match digest with
    | Some (digest, time) when Mtime.Span.compare (Mtime.span (Mtime_clock.now ()) time) delay < 0
      ->
        (* Skip if we changed in strictly less than "delay" time. *)
        Some ((digest, time), None)
    | Some (digest, _) -> (
      match Digest.file path_s with
      | new_digest ->
          Log.debug (fun f ->
              f "Checking digest for %S (previously %s, now %s)" path_s (Digest.to_hex digest)
                (Digest.to_hex new_digest));
          if new_digest = digest then result ~digest None else read ~new_digest
      | exception Sys_error msg -> error msg )
    | None -> (
      match Digest.file path_s with
      | new_digest -> read ~new_digest
      | exception Sys_error msg -> error msg )

  let oracle ?delay ?container_k ?(eq_v = ( == )) ~name process =
    let open Data in
    let compute path previous =
      let previous = Option.join previous in
      match with_change ?delay ~process:(process path) ~path (Option.map fst previous) with
      | Some (t, None) -> Some (t, Option.get previous |> snd)
      | Some (t, Some v) -> Some (t, v)
      | None -> None
    in
    let compute_key = Key.oracle ?container_k ~name:(name ^ ".compute") compute in
    Key.key ?container_k ~eq_v:(Option.equal eq_v) ~name (fun store key ->
        need store compute_key key |> Option.map snd)
end

module Workspace = struct
  type t =
    { uri : Uri.t;
      path : Fpath.t option;
      name : string option
    }

  let workspace_for ~root (workspaces : t list) = function
    | { Span.path = None; _ } -> None
    | { Span.path = Some path; _ } ->
        let get_name best = function
          | { path = None; _ } -> best
          | { path = Some wk_path; _ } as workspace -> (
              if not (Fpath.is_rooted ~root:wk_path path) then best
              else
                let wk_len = Fpath.to_string path |> String.length in
                match best with
                | Some (_, best_len) when wk_len <= best_len -> best
                | _ -> Some (workspace, wk_len) )
        in
        List.fold_left get_name None workspaces |> Option.map fst |> CCOpt.or_ ~else_:root

  let config : (t, Config.t) Data.Key.t =
    let open Data in
    let read_config path chan =
      let parsed =
        Lexing.from_channel chan
        |> Config.of_lexer ~directory:(Fpath.parent path) (Filename.of_fpath path)
      in
      match parsed with
      | Ok o -> o
      | Error { value; _ } ->
          (* TODO: Better error handling. Publish diagnostics or something? *)
          Log.err (fun f -> f "Cannot parse config file %a: %s" Fpath.pp path value);
          Config.default
    in
    let config_key = FileDigest.oracle ~name:(__MODULE__ ^ ".Workspace.config_file") read_config in
    Key.key ~name:(__MODULE__ ^ ".Workspace.config") (fun store workspace ->
        workspace.path
        |> CCOpt.flat_map (fun path -> need store config_key Fpath.(path / "illuaminate.sexp"))
        |> Option.value ~default:Config.default)

  let context : (t, Data.Programs.Context.t) Data.Key.t =
    let open Data in
    Key.key ~name:(__MODULE__ ^ ".Workspace.context") (fun store workspace ->
        let config = need store config workspace in
        { Programs.Context.root = workspace.path; config = Config.get_store config })
end

type contents =
  | Open of Lsp.Text_document.t
  | FromFile of FileDigest.t

type document =
  { name : Span.filename;
    uri : Uri.t;
    mutable contents : contents;
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
    mutable file : Data.Programs.Files.id option;
    mutable workspace : Workspace.t option
  }

type t =
  { mutable root : Workspace.t option;
    mutable workspaces : Workspace.t list;
    directories : Workspace.t UriTbl.t;
    files : document UriTbl.t;
    data : Data.t;
    file_store : Data.Programs.Files.t
  }

let data { data; _ } = data

let default_context : Data.Programs.Context.t =
  { root = None; config = Config.get_store Config.default }

let create () =
  let files = UriTbl.create 64 in
  let file_store = Data.Programs.Files.create () in
  let data =
    let open Data in
    let open Builder in
    empty |> Programs.Files.builder file_store
    |> key Programs.Context.key (fun store name ->
           match Filename.to_uri name |> UriTbl.find_opt files with
           | Some { workspace = Some workspace; _ } -> need store Workspace.context workspace
           | Some { workspace = None; _ } -> default_context
           | None -> failwith "Unknown file!")
    |> build
  in
  { directories = UriTbl.create 4; workspaces = []; root = None; files; file_store; data }

let lex_file name doc =
  Text_document.text doc |> Lexing.from_string |> IlluaminateParser.program name

let get_file { files; _ } = UriTbl.find_opt files

(** Update the {!Data.Programs.Files.t} store. *)
let sync_file store file =
  ( match file with
  | { program = Error _; _ } -> ()
  | { program = Ok p; file = None; _ } as file ->
      let id = Data.Programs.Files.add p store.file_store in
      file.file <- Some id
  | { program = Ok p; file = Some id; _ } -> Data.Programs.Files.update id p );
  Data.refresh store.data

let update_file store (doc : document) contents =
  doc.contents <- Open contents;
  doc.program <- lex_file doc.name contents;
  sync_file store doc

let open_file store contents =
  let uri = Text_document.documentUri contents in
  match get_file store uri with
  | Some doc -> update_file store doc contents; doc
  | None ->
      (* TODO: Store the workspace in data system rather than on a file. *)
      let name = Filename.of_uri uri in
      let program = lex_file name contents in
      let workspace = Workspace.workspace_for ~root:store.root store.workspaces name in
      let doc = { name; uri; contents = Open contents; program; file = None; workspace } in
      UriTbl.add store.files uri doc;
      sync_file store doc;
      Log.info (fun f ->
          f "Opening %s in workspace %s" (Uri.to_string uri)
            (Option.fold ~none:"none" ~some:(fun f -> f.Workspace.uri |> Uri.to_string) workspace));
      doc

let set_workspace store ?root workspaces =
  Log.info (fun f ->
      f "Setting root to %s and additional workspaces to [%s]"
        (Option.fold ~none:"none" ~some:Uri.to_string root)
        ( List.map (fun { Protocol.WorkspaceFolder.uri; _ } -> Uri.to_string uri) workspaces
        |> String.concat ", " ));

  (* TODO: Don't reconstruct unless needed *)
  store.root <-
    Option.fold ~none:store.root
      ~some:(fun uri -> Some { uri; path = Filename.get_path uri; name = None })
      root;
  store.workspaces <-
    List.map
      (fun ({ uri; name } : Protocol.WorkspaceFolder.t) ->
        { uri; Workspace.path = Filename.get_path uri; name = Some name })
      workspaces
