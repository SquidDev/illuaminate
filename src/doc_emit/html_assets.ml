(** Compute the hash of a file. *)
let hash_file path = Digest.file (Fpath.to_string path) |> Digest.to_hex |> CCString.take 8

(** [hashed_url path url]: Append a cachbuster (derived from the hash of [path]) to [url]. *)
let hashed_url path url = Printf.sprintf "%s?v=%s" url (hash_file path)

(** Find an asset located at the specified path, and copy it into the site's target directory,
    returning the new file name. *)
let find_asset : (Fpath.t, string option) IlluaminateData.Key.t =
  let module Fpath = struct
    include Fpath

    let hash p = String.hash (Fpath.to_string p)
  end in
  IlluaminateData.Key.deferred ~name:(__MODULE__ ^ ".find_asset") ~eq:(Option.equal String.equal)
    ~key:(module Fpath)
    ()

(** Register a handler for {!find_asset}. *)
let add_find_asset dest builder =
  let copy_asset path _ : string option =
    let path' = Fpath.to_string path in
    if not (Sys.file_exists path') then None
    else
      let digest = hash_file path in
      let dest_name =
        let path, ext = Fpath.split_ext path in
        Format.sprintf "%s-%s%s" (Fpath.filename path) digest ext
      in
      let dest_file = Fpath.(dest / dest_name) in
      In_channel.with_open_bin path' (fun inp ->
          Out_channel.with_open_bin (Fpath.to_string dest_file) (fun out -> CCIO.copy_into inp out));
      Some dest_name
  in
  IlluaminateData.Builder.oracle find_asset copy_asset builder
