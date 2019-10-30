type t = CCHet.Tbl.t

module Cache = Ephemeron.K1.Make (struct
  type t = Syntax.program

  let equal = ( == )

  let hash = Hashtbl.hash
end)

type 'a status =
  | Loading
  | Loaded of 'a

type 'a key =
  { name : string;
    factory : t -> Syntax.program -> 'a;
    key : 'a status Cache.t CCHet.Key.t
  }

let create () = CCHet.Tbl.create ()

let key ~name factory = { name; factory; key = CCHet.Key.create () }

let get program { name; factory; key } store =
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
      let built = factory store program in
      Cache.replace programs program (Loaded built);
      built
