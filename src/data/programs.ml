open IlluaminateCore

module Context = struct
  type t =
    { root : Fpath.t option;
      config : IlluaminateConfig.Schema.store
    }

  let eq a b = a.root = b.root && a.config == b.config

  let key : (Span.filename, t) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Context") ~eq ~key:(module Span.Filename) ()
end

module Files = struct
  let file : (Span.filename, File.t option) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Files.file") ~eq:(Option.equal File.( = ))
      ~key:(module Span.Filename)
      ()

  let files : (unit, Span.filename list) Core.Key.t =
    Core.Key.deferred ~name:(__MODULE__ ^ ".Files.files")
      ~eq:(CCList.equal Span.Filename.equal)
      ~key:(module Types.Unit)
      ()
end

module FileStore = struct
  module Tbl = Hashtbl.Make (struct
    type t = Span.filename

    let hash = Hashtbl.hash
    let equal = ( == )
  end)

  type t =
    { files : File.t Tbl.t;
      mutable file_list : Span.filename list option
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

type 'a key = (Span.filename, 'a option) Core.Key.t

let key ~name ?eq build : 'a key =
  let build data file =
    match Core.need data Files.file file with
    | Some (Lua x) -> Some (build data file x)
    | Some (Markdown _) | None -> None
  in
  Core.Key.key ~name ?eq:(Option.map Option.equal eq) ~key:(module Span.Filename) build

let file_key ~name ?eq build =
  let build data file =
    match Core.need data Files.file file with
    | Some x -> Some (build data file x)
    | None -> None
  in
  Core.Key.key ~name ?eq:(Option.map Option.equal eq) ~key:(module Span.Filename) build
