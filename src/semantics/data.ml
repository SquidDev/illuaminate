open IlluaminateCore
module IntMap = Map.Make (Int)

type t =
  { files : Syntax.program IntMap.t;
    file_list : int list;
    next_id : int;
    store : CCHet.Tbl.t
  }

module Cache = Ephemeron.K1.Make (struct
  type t = Syntax.program

  let equal = ( == )

  let hash = Hashtbl.hash
end)

module Files = struct
  type nonrec t = t

  type id = int

  let create () = { files = IntMap.empty; file_list = []; next_id = 0; store = CCHet.Tbl.create () }

  let add program files =
    let id = files.next_id in
    (* Empty the store. This is really gross (both conceptually and in implementation, but until we
       have dependency tracking, we rather need it. *)
    CCHet.Tbl.to_list files.store
    |> List.iter (fun (CCHet.Pair (k, _)) -> CCHet.Tbl.remove files.store k);
    ( { files with
        next_id = files.next_id + 1;
        file_list = id :: files.file_list;
        files = IntMap.add id program files.files
      },
      id )

  let update id program files =
    let existing = IntMap.find id files.files in
    if existing == program then files
    else (
      CCHet.Tbl.to_list files.store
      |> List.iter (fun (CCHet.Pair (k, _)) -> CCHet.Tbl.remove files.store k);
      { files with files = IntMap.add id program files.files } )
end

type 'a status =
  | Loading
  | Loaded of 'a

type 'a key =
  { name : string;
    factory : t -> Syntax.program -> 'a;
    key : 'a status Cache.t CCHet.Key.t
  }

let of_files (f : Files.t) : t = f

let key ~name factory = { name; factory; key = CCHet.Key.create () }

let get program { name; factory; key } ({ store; _ } as t) =
  let programs =
    match CCHet.Tbl.find store key with
    | Some x -> x
    | None ->
        let map = Cache.create 8 in
        CCHet.Tbl.add store key map; map
  in
  match Cache.find_opt programs program with
  | Some (Loaded x) -> x
  | Some Loading -> failwith (Printf.sprintf "Loop loading %S." name)
  | None ->
      Cache.add programs program Loading;
      let built = factory t program in
      Cache.replace programs program (Loaded built);
      built

let files f = f.file_list

let get_for id key ({ files; _ } as t) =
  match IntMap.find_opt id files with
  | None -> raise Not_found
  | Some program -> get program key t
