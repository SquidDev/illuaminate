open IlluaminateCore
open Lsp
module Data = IlluaminateData
module UriTbl = Hashtbl.Make (Uri)
module Config = IlluaminateConfigFormat

let src = Logs.Src.create ~doc:"Loading and storing of local files" __MODULE__

module Log = (val Logs.src_log src)

type client_channel =
  { notify : Lsp.Server_notification.t -> unit;
    request : 'a. 'a Lsp.Server_request.t -> unit
  }

module Filename = struct
  let protocol =
    match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"

  let get_path uri = Uri.to_string uri |> CCString.chop_prefix ~pre:protocol |> Option.map Fpath.v

  let of_uri uri = Span.Filename.mk ?path:(get_path uri) (Uri.to_string uri)

  let to_uri_json { Span.id; _ } : Yojson.Safe.t = `String id

  let box x = Uri.t_of_yojson (`String x)

  let to_uri { Span.id; _ } = box id

  let of_fpath path = Span.Filename.mk ~path (Fpath.to_string path |> Uri.of_path |> Uri.to_string)
end

module FileDigest = struct
  type t = Digest.t * Mtime.t

  let default_duration = Mtime.Span.of_uint64_ns 5_000_000_000L (* 5s *)

  (** Read a file and perform some processing function (such as parsing) if the contents has changed
      since last time we checked. Limited to 5 seconds. *)
  let with_change ?(delay = default_duration) ~process ~path (digest : t option) =
    let path_s = Fpath.to_string path in
    let error msg =
      Log.warn (fun f -> f "Error reading file %S (%s)" path_s msg);
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

  type workspaces =
    { root : t option;
      workspaces : t list
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

  let workspaces : (unit, workspaces) Data.Key.t =
    let eq_v a b =
      CCOpt.equal ( == ) a.root b.root && CCList.equal ( == ) a.workspaces b.workspaces
    in
    Data.Key.deferred ~eq_v ~name:(__MODULE__ ^ ".Workspace.workspaces") ()

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

type document =
  { name : Span.filename;
    uri : Uri.t;
    mutable contents : Lsp.Text_document.t;
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result
  }

type t =
  { workspaces : Workspace.workspaces ref;
    files : document UriTbl.t;
    data : Data.t
  }

let data { data; _ } = data

let default_context : Data.Programs.Context.t =
  { root = None; config = Config.get_store Config.default }

let create () =
  let files = UriTbl.create 64 in
  let workspaces = ref { Workspace.root = None; workspaces = [] } in
  let data =
    let open Data in
    let open Builder in
    empty
    |> oracle Programs.Files.files (fun () _ -> [])
    |> oracle Workspace.workspaces (fun () _ -> !workspaces)
    |> key Programs.Context.key (fun store name ->
           let { Workspace.root; workspaces } = need store Workspace.workspaces () in
           Workspace.workspace_for ~root workspaces name
           |> Option.fold ~none:default_context ~some:(need store Workspace.context))
    |> build
  in
  { workspaces; files; data }

let lex_file name doc =
  Text_document.text doc |> Lexing.from_string |> IlluaminateParser.program name

let get_file { files; data; _ } =
  (* We refresh our data here - we're not changing anything, but this is the primary interaction
     point! It might be better to do that before every notification/request instead, but this'll do
     for now. *)
  Data.refresh data; UriTbl.find_opt files

let update_file store (doc : document) contents =
  doc.contents <- contents;
  doc.program <- lex_file doc.name contents;
  Data.refresh store.data

let open_file store contents =
  let uri = Text_document.documentUri contents in
  match get_file store uri with
  | Some doc -> update_file store doc contents; doc
  | None ->
      Log.info (fun f -> f "Opening %a" Uri.pp uri);
      let name = Filename.of_uri uri in
      let program = lex_file name contents in
      let doc = { name; uri; contents; program } in
      UriTbl.add store.files uri doc; Data.refresh store.data; doc

let set_workspace store ?root workspaces =
  Log.info (fun f ->
      f "Setting root to %s and additional workspaces to [%s]"
        (Option.fold ~none:"none" ~some:Uri.to_string root)
        (List.map (fun { Types.WorkspaceFolder.uri; _ } -> uri) workspaces |> String.concat ", "));

  (* TODO: Work out some sane hashing or caching implementation for workspaces - we currently use
     the identity. *)
  let current = !(store.workspaces) in
  store.workspaces :=
    { root =
        Option.fold ~none:current.root
          ~some:(fun uri -> Some { uri; path = Filename.get_path uri; name = None })
          root;
      workspaces =
        List.map
          (fun ({ uri; name } : Types.WorkspaceFolder.t) ->
            let uri = Filename.box uri in
            { uri; Workspace.path = Filename.get_path uri; name = Some name })
          workspaces
    }
