open Illuaminate
open IlluaminateCore

module Context = struct
  type t =
    { root : Fpath.t option;
      config : IlluaminateConfig.Schema.store
    }

  let eq a b = a.root = b.root && a.config == b.config

  let key : (File_id.t, t) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Context") ~eq ~key:(module File_id) ()
end

module Files = struct
  let file : (File_id.t, File.t option) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Files.file") ~eq:(Option.equal File.equal)
      ~key:(module File_id)
      ()

  let files : (unit, File_id.t list) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Files.files") ~eq:(CCList.equal File_id.equal)
      ~key:(module Types.Unit)
      ()
end

module FileStore = struct
  module Tbl = Hashtbl.Make (struct
    type t = File_id.t

    let hash = Hashtbl.hash
    let equal = ( == )
  end)

  type t =
    { files : File.t Tbl.t;
      mutable file_list : File_id.t list option
    }

  let lazy_builder store builder =
    let get_file path _ =
      let store = Lazy.force store in
      Tbl.find_opt store.files path
    in
    let get_files () _ =
      let store = Lazy.force store in
      match store.file_list with
      | Some x -> x
      | None ->
          let files = Tbl.to_seq_keys store.files |> List.of_seq in
          store.file_list <- Some files;
          files
    in
    Core.Builder.oracle Files.file get_file builder;
    Core.Builder.oracle Files.files get_files builder

  let builder store = lazy_builder (lazy store)
  let create () = { files = Tbl.create 16; file_list = None }

  let update store path program =
    match (Tbl.find_opt store.files path, program) with
    | Some old, Some changed -> if old != changed then Tbl.replace store.files path changed
    | Some _, None ->
        Tbl.remove store.files path;
        store.file_list <- None
    | None, None -> ()
    | None, Some new_program ->
        Tbl.add store.files path new_program;
        store.file_list <- Option.map (fun x -> path :: x) store.file_list
end

type 'a key = (File_id.t, 'a option) Core.Key.t

let key ~name ?eq build : 'a key =
  let build data file =
    match Core.need data Files.file file with
    | Some (Lua x) -> Some (build data file x)
    | Some (Markdown _) | None -> None
  in
  Core.Key.key ~name ?eq:(Option.map Option.equal eq) ~key:(module File_id) build

let file_key ~name ?eq build =
  let build data file =
    match Core.need data Files.file file with
    | Some x -> Some (build data file x)
    | None -> None
  in
  Core.Key.key ~name ?eq:(Option.map Option.equal eq) ~key:(module File_id) build
