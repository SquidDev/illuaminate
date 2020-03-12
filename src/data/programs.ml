open IlluaminateCore
module IntMap = Map.Make (Int)

module Context = struct
  type t =
    { root : Fpath.t option;
      config : IlluaminateConfig.Schema.store
    }

  let key : (Span.filename, t) Core.Key.t = Core.Key.oracle ~name:(__MODULE__ ^ ".Context") ()
end

module Files = struct
  type t =
    { mutable files : Syntax.program IntMap.t;
      mutable file_list : id list;
      mutable next_id : int
    }

  and id =
    { id : int;
      store : t
    }

  let file : (id, Syntax.program) Core.Key.t = Core.Key.oracle ~name:(__MODULE__ ^ ".Files.file") ()

  let files : (unit, id list) Core.Key.t = Core.Key.oracle ~name:(__MODULE__ ^ ".Files.files") ()

  let builder store builder =
    let get_file { id; store = this_store } =
      assert (store == this_store);
      IntMap.find id store.files
    in
    let get_files () = store.file_list in
    builder |> Core.Builder.oracle file get_file |> Core.Builder.oracle files get_files

  let create () = { files = IntMap.empty; file_list = []; next_id = 0 }

  let add program store =
    let id = store.next_id in
    let res = { id; store } in
    store.next_id <- store.next_id + 1;
    store.file_list <- res :: store.file_list;
    store.files <- IntMap.add id program store.files;
    res

  let update { id; store } program =
    let existing = IntMap.find id store.files in
    if existing != program then store.files <- IntMap.add id program store.files
end

type 'a key = (Syntax.program, 'a) Core.Key.t

module WeakProgram = Contained_tbl.WeakContainer (struct
  type t = Syntax.program

  let equal = ( == )

  let hash = Hashtbl.hash
end)

let key ~name build = Core.Key.key ~name ~container_k:(module WeakProgram) build
