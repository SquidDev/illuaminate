open IlluaminateCore
open Lsp
open Lsp.Types
open Data
module Data = IlluaminateData
module UriTbl = Hashtbl.Make (Uri)
module Config = IlluaminateConfigFormat
module FilenameSet = Set.Make (Span.Filename)

module UriMap = Map.Make (struct
  type t = Uri.t

  let compare l r = String.compare (Uri.to_string l) (Uri.to_string r)
end)

let src = Logs.Src.create ~doc:"Loading and storing of local files" __MODULE__

module Log = (val Logs.src_log src)

type client_channel =
  { notify : Server_notification.t -> unit Fiber.t;
    request : 'a. 'a Server_request.t -> ('a, Jsonrpc.Response.Error.t) result Fiber.t
  }

module Filename = struct
  let protocol =
    match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"

  let get_path uri =
    if Uri.to_string uri |> CCString.prefix ~pre:protocol then Some (Uri.to_path uri |> Fpath.v)
    else None

  let of_uri uri = Span.Filename.mk ?path:(get_path uri) (Uri.to_string uri)
  let to_uri_json { Span.id; _ } : Yojson.Safe.t = `String id
  let box x = Uri.t_of_yojson (`String x)
  let to_uri { Span.id; _ } = box id
  let of_path path = Span.Filename.mk ~path (Fpath.to_string path |> Uri.of_path |> Uri.to_string)
end

module Workspace = struct
  type t =
    { uri : Uri.t;
      path : Fpath.t option;
      name : string option
    }

  type workspaces =
    { root : t option;
      workspaces : t UriMap.t
    }

  let pp out { uri; _ } = Uri.pp out uri

  let workspace_for ~root (workspaces : t UriMap.t) = function
    | { Span.path = None; _ } -> None
    | { Span.path = Some path; _ } ->
        let get_name _ v best =
          match v with
          | { path = None; _ } -> best
          | { path = Some wk_path; _ } as workspace -> (
              if not (Fpath.is_rooted ~root:wk_path path) then best
              else
                let wk_len = Fpath.to_string path |> String.length in
                match best with
                | Some (_, best_len) when wk_len <= best_len -> best
                | _ -> Some (workspace, wk_len))
        in
        UriMap.fold get_name workspaces None |> Option.map fst |> CCOption.or_ ~else_:root

  let workspaces : (unit, workspaces) Data.Key.t =
    let eq a b =
      CCOption.equal ( == ) a.root b.root && UriMap.equal ( == ) a.workspaces b.workspaces
    in
    Data.Key.deferred ~eq ~name:(__MODULE__ ^ ".Workspace.workspaces") ()

  let config : (t, Config.t) Data.Key.t =
    let open Data in
    let read_config path chan =
      let parsed =
        Lexing.from_channel chan
        |> Config.of_lexer ~directory:(Fpath.parent path) (Filename.of_path path)
      in
      match parsed with
      | Ok o -> o
      | Error { value; _ } ->
          (* TODO: Better error handling. Publish diagnostics or something? *)
          Log.err (fun f -> f "Cannot parse config file %a: %s" Fpath.pp path value);
          Config.default
    in
    let config_key = FileDigest.oracle ~name:(__MODULE__ ^ ".Workspace.config_file") read_config in
    Key.key ~name:(__MODULE__ ^ ".Workspace.config") ~pp @@ fun store workspace ->
    workspace.path
    |> CCOption.flat_map (fun path -> need store config_key Fpath.(path / "illuaminate.sexp"))
    |> Option.value ~default:Config.default

  let context : (t, Data.Programs.Context.t) Data.Key.t =
    let open Data in
    Key.key ~name:(__MODULE__ ^ ".Workspace.context") ~pp @@ fun store workspace ->
    let config = need store config workspace in
    { Programs.Context.root = workspace.path; config = Config.get_store config }

  let sources : (t, Fpath.t list) Data.Key.t =
    let delay = Mtime.Span.of_uint64_ns 60_000_000_000L (* 60s *) in
    let name = __MODULE__ ^ ".Workspace.sources" in
    let open Data in
    rate_limit ~delay ~name @@ fun store key previous ->
    let config = need store config key in
    let files = ref [] in
    let add f = if Fpath.get_ext f = ".lua" then files := f :: !files in
    (match key.path with
    | None -> Config.all_files add config
    | Some p -> Config.files add config p);
    let changed =
      match previous with
      | Absent -> Key.RecomputeChange
      | Recompute c | DependencyChange c ->
          if CCList.equal Fpath.equal c !files then Key.RecomputeSame else RecomputeChange
    in
    { value = !files; changed }

  let linters : (Span.filename, Error.Tag.filter * IlluaminateConfig.Schema.store) Data.Key.t =
    let open Data in
    Key.key
      ~container:(module Container.Strong (Span.Filename))
      ~pp:Span.Filename.pp
      ~name:(__MODULE__ ^ ".Workspace.linters")
    @@ fun store name ->
    let { root; workspaces } = need store workspaces () in
    let workspace = workspace_for ~root workspaces name in
    match workspace with
    | None ->
        Log.info (fun f -> f "Default linters for %a" (CCOption.pp Fpath.pp) name.path);
        Config.(get_linters default ())
    | Some workspace ->
        let config = need store config workspace in
        Log.info (fun f -> f "Getting linters for %a" (CCOption.pp Fpath.pp) name.path);
        Config.get_linters config ?path:name.path ()
end

type parsed_program = (Syntax.program, IlluaminateParser.Error.t Span.spanned) result

type parsed_file =
  | OnDisk of parsed_program FileDigest.t
  | Open of parsed_program
  | Unknown

let parsed_file : (Span.filename, parsed_file) Data.Key.t =
  let eq l r =
    if l == r then true
    else
      match (l, r) with
      | Unknown, Unknown -> true
      | OnDisk l, OnDisk r -> FileDigest.equal ~eq:( == ) l r
      | Open l, Open r -> l == r
      | _, _ -> false
  in
  Data.Key.deferred ~pp:Span.Filename.pp ~eq
    ~container:(module Data.Container.Strong (Span.Filename))
    ~name:(__MODULE__ ^ ".parsed_file") ()

let default_context : Data.Programs.Context.t =
  { root = None; config = Config.get_store Config.default }

type document =
  { name : Span.filename;
    uri : Uri.t;
    mutable contents : Lsp.Text_document.t;
    mutable program : parsed_program
  }

type t =
  { workspaces : Workspace.workspaces ref;
    files : document UriTbl.t;
    data : Data.t;
    mutable capabilities : ClientCapabilities.t option
  }

let create () =
  let files = UriTbl.create 64 in
  let workspaces = ref { Workspace.root = None; workspaces = UriMap.empty } in
  let data =
    let open Data in
    let open Builder in
    empty
    |> oracle Workspace.workspaces (fun () _ -> !workspaces)
    (* In order to extract a file, we try to read our open files, from disk, and then give up after
       that. *)
    |> oracle parsed_file (fun file previous ->
           match UriTbl.find_opt files (Filename.to_uri file) with
           | Some p -> Open p.program
           | None -> (
             match file.path with
             | None -> Unknown
             | Some path ->
                 let digest =
                   match previous with
                   | Some (OnDisk d) -> Some d
                   | None | Some (Open _ | Unknown) -> None
                 in
                 FileDigest.with_change
                   ~process:(fun chan -> Lexing.from_channel chan |> IlluaminateParser.program file)
                   ~path digest
                 |> Option.fold ~none:Unknown ~some:(fun x -> OnDisk x)))
    (* Then we use the above key to extract the actual program. *)
    |> key Programs.Files.file (fun store file ->
           match need store parsed_file file with
           | OnDisk { value; _ } | Open value -> (
             match value with
             | Ok x -> Some (Lua x)
             | Error _ -> None)
           | Unknown -> None)
    (* The file list is derived from all matching patterns in the config file for each workspace *)
    |> key Programs.Files.files (fun store () ->
           let workspaces = need store Workspace.workspaces () in
           let add workspace all =
             need store Workspace.sources workspace
             |> List.fold_left (fun s p -> FilenameSet.add (Filename.of_path p) s) all
           in
           let base = UriMap.fold (fun _ -> add) workspaces.workspaces FilenameSet.empty in
           (match workspaces.root with
           | None -> base
           | Some w -> add w base)
           |> FilenameSet.to_seq |> List.of_seq)
    (* A program's context is just derived from its workspace. *)
    |> key Programs.Context.key (fun store name ->
           let { Workspace.root; workspaces } = need store Workspace.workspaces () in
           Workspace.workspace_for ~root workspaces name
           |> Option.fold ~none:default_context ~some:(need store Workspace.context))
    |> build
  in
  { workspaces; files; data; capabilities = None }

let data { data; _ } = data

let get_file { files; data; _ } file =
  (* We refresh our data here - we're not changing anything, but this is the primary interaction
     point! It might be better to do that before every notification/request instead, but this'll do
     for now. *)
  Data.refresh data; UriTbl.find_opt files file

let lex_file name doc =
  Text_document.text doc |> Lexing.from_string |> IlluaminateParser.program name

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

let close_file store uri =
  Log.info (fun f -> f "Closing %a" Uri.pp uri);
  UriTbl.remove store.files uri;
  Data.refresh store.data

let update_workspace store ?root ~add ~remove () =
  Log.info (fun f ->
      let pp xs = List.map (fun { WorkspaceFolder.uri; _ } -> uri) xs |> String.concat ", " in
      f "Setting root to %s, adding workspaces [%s] and removing [%s]"
        (Option.fold ~none:"none" ~some:Uri.to_string root)
        (pp add) (pp remove));

  let current = !(store.workspaces) in
  let workspaces =
    List.fold_left
      (fun t k -> UriMap.remove (Filename.box k.WorkspaceFolder.uri) t)
      current.workspaces remove
  in
  let workspaces =
    List.fold_left
      (fun t ({ uri; name } : WorkspaceFolder.t) ->
        let uri = Filename.box uri in
        UriMap.add uri { uri; Workspace.path = Filename.get_path uri; name = Some name } t)
      workspaces add
  in
  store.workspaces :=
    { root =
        Option.fold ~none:current.root
          ~some:(fun uri -> Some { uri; path = Filename.get_path uri; name = None })
          root;
      workspaces
    }

let linters = Workspace.linters
let capabilities x = Option.get x.capabilities

let set_capabilities cap x =
  if Option.is_some x.capabilities then failwith "Cannot set capabilities twice.";
  x.capabilities <- Some cap
