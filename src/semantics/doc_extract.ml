open IlluaminateCore
open! Doc_syntax
module StringMap = Map.Make (String)
module NMap = Map.Make (Namespace)
module R = Resolve
module VarTbl = R.VarTbl
module D = IlluaminateData
module Resolve = Doc_extract_resolve
module U = Doc_extract_unresolved

(* Actually exported modules *)
module Config = Doc_extract_config
module Tag = Doc_extract_helpers.Tag

type t =
  { errors : Error.t;
    comments : unit U.CommentCollection.t;
    contents : page documented option;
    vars : value documented VarTbl.t
  }

let errors { errors; _ } = Error.errors errors

let detached_comments ({ comments; _ } : t) =
  U.CommentCollection.to_seq_keys comments |> List.of_seq

let crunch_pages : page documented list -> page documented = function
  | [] -> failwith "Impossible"
  | x :: xs ->
      let errs = Error.make () in
      List.fold_left Doc_extract_helpers.Merge.(documented (page ~errs)) x xs

let add_page map modu =
  NMap.update modu.descriptor.page_namespace
    (fun map ->
      Option.value map ~default:StringMap.empty
      |> StringMap.update modu.descriptor.page_id (fun x ->
             Some (modu :: Option.value ~default:[] x))
      |> Option.some)
    map

let build_resolver data contents =
  let all =
    List.fold_left
      (fun modules file ->
        D.need data D.Programs.Files.file file
        |> CCOpt.flat_map (D.need data U.unresolved_module_file)
        |> Option.fold ~none:modules ~some:(add_page modules))
      NMap.empty
      (D.need data D.Programs.Files.files ())
  in
  let current_scope =
    contents
    |> Option.map @@ fun current ->
       (* Bring all other modules with the same name into scope if required. *)
       let m =
         NMap.find_opt current.descriptor.page_namespace all
         |> CCOpt.flat_map (StringMap.find_opt current.descriptor.page_id)
       in
       match m with
       | None -> current
       | Some all -> crunch_pages (if List.memq current all then all else current :: all)
  in
  let all = NMap.map (StringMap.map (fun x -> lazy (crunch_pages x))) all in
  Resolve.context all current_scope

let get data program =
  let state, _ = D.need data U.Infer.key program in
  let contents = D.need data U.unresolved_module program in
  let cache, lift = build_resolver data contents in
  let contents = Option.map (Resolve.go_page ~cache lift) contents in
  let vars =
    VarTbl.to_seq state.vars
    |> Seq.map (fun (k, v) -> (k, Resolve.go_value_doc ~cache lift !v))
    |> VarTbl.of_seq
  in
  { contents; errors = state.errs; comments = state.unused_comments; vars }

let program = D.Programs.key ~name:(__MODULE__ ^ ".program") get

let file =
  D.Programs.file_key ~name:(__MODULE__ ^ ".file") @@ fun data -> function
  | Lua p -> D.need data program p
  | Markdown _ as file ->
      let contents = D.need data U.unresolved_module_file file in
      let cache, lift = build_resolver data contents in
      let contents = Option.map (Resolve.go_page ~cache lift) contents in
      { contents;
        errors = Error.make ();
        comments = U.CommentCollection.create 0;
        vars = VarTbl.create 0
      }

let get_page ({ contents; _ } : t) = contents

let get_var ({ vars; _ } : t) var = VarTbl.find_opt vars var

let get_pages =
  D.Key.key ~name:(__MODULE__ ^ ".get_pages") @@ fun data () ->
  D.need data D.Programs.Files.files ()
  |> List.fold_left
       (fun pages filename ->
         match D.need data D.Programs.Files.file filename |> Option.map (D.need data file) with
         | None | Some { contents = None; _ } -> pages
         | Some { contents = Some result; _ } -> add_page pages result)
       NMap.empty
  |> NMap.map (StringMap.map crunch_pages)
