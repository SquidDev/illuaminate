module D = IlluaminateData

let make_data name file =
  let context =
    { D.Programs.Context.root = None; config = IlluaminateConfig.Schema.(default empty) }
  in
  let files =
    let open D.Programs.FileStore in
    let store = create () in
    update store name (Some file); store
  in
  let open D.Builder in
  build @@ fun b ->
  D.Programs.FileStore.builder files b;
  oracle D.Programs.Context.key (fun _ _ -> context) b
