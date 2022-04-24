open IlluaminateCore

module Context = struct
  type t =
    { root : Fpath.t option;
      config : IlluaminateConfig.Schema.store
    }

  let eq a b = a.root = b.root && a.config == b.config

  let key : (Span.filename, t) Core.Key.t =
    Core.Key.deferred ~pp:Span.Filename.pp
      ~container:(module Contained_tbl.StrongContainer (Span.Filename))
      ~eq ~name:(__MODULE__ ^ ".Context") ()
end

module Files = struct
  let file : (Span.filename, File.t option) Core.Key.t =
    Core.Key.deferred ~pp:Span.Filename.pp
      ~container:(module Contained_tbl.StrongContainer (Span.Filename))
      ~eq:(Option.equal File.( = )) ~name:(__MODULE__ ^ ".Files.file") ()

  let files : (unit, Span.filename list) Core.Key.t =
    Core.Key.deferred ~eq:(CCList.equal Span.Filename.equal) ~name:(__MODULE__ ^ ".Files.files") ()
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
    builder |> Core.Builder.oracle Files.file get_file |> Core.Builder.oracle Files.files get_files

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

type 'a key = (Syntax.program, 'a) Core.Key.t

let key ~name build =
  Core.Key.key ~name
    ~pp:(fun f k -> Syntax.Spanned.program k |> Span.filename |> Span.Filename.pp f)
    ~container:(Contained_tbl.weak ~eq:( == ) ())
    build

let on_program f : File.t option -> 'a option = function
  | Some (Lua x) -> Some (f x)
  | Some (Markdown _) | None -> None

let need_for data key file = Core.need data Files.file file |> on_program (Core.need data key)
let get_for data key file = Core.get data Files.file file |> on_program (Core.get data key)

let file_key ~name build =
  Core.Key.key ~name
    ~pp:(fun f k -> File.span k |> Span.filename |> Span.Filename.pp f)
    ~container:(Contained_tbl.weak ~eq:File.( = ) ())
    build
