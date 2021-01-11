open IlluaminateCore
open! Doc_syntax
module StringMap = Map.Make (String)
module MKMap = Map.Make (Module.Kind)
module R = Resolve
module VarTbl = R.VarTbl
module D = IlluaminateData
module Resolve = Doc_extract_resolve
module U = Doc_extract_unresolved

(* Actually exported modules *)
module Config = Doc_extract_config
module Tag = Doc_extract_helpers.Tag

type t =
  { current_module : U.result;
    errors : Error.t;
    comments : unit U.CommentCollection.t;
    contents : module_info documented option;
    vars : value documented VarTbl.t
  }

let errors { errors; _ } = Error.errors errors

let detached_comments ({ comments; _ } : t) =
  U.CommentCollection.to_seq_keys comments |> List.of_seq

let crunch_modules : module_info documented list -> module_info documented = function
  | [] -> failwith "Impossible"
  | x :: xs ->
      let errs = Error.make () in
      List.fold_left Doc_extract_helpers.Merge.(documented (modules ~errs)) x xs

let add_module map modu =
  MKMap.update modu.descriptor.mod_kind
    (fun map ->
      Option.value map ~default:StringMap.empty
      |> StringMap.update modu.descriptor.mod_name (fun x ->
             Some (modu :: Option.value ~default:[] x))
      |> Option.some)
    map

let get data program =
  let state, current_module = D.need data U.Infer.key program in
  let contents = D.need data U.unresolved_module program in
  let cache, lift =
    let all =
      List.fold_left
        (fun modules file ->
          match D.Programs.need_for data U.unresolved_module file with
          | None | Some None -> modules
          | Some (Some result) -> add_module modules result)
        MKMap.empty
        (D.need data D.Programs.Files.files ())
    in
    let current_scope =
      contents
      |> Option.map @@ fun current ->
         (* Bring all other modules with the same name into scope if required. *)
         let m =
           MKMap.find_opt current.descriptor.mod_kind all
           |> CCOpt.flat_map (StringMap.find_opt current.descriptor.mod_name)
         in
         match m with
         | None -> current
         | Some all -> crunch_modules (if List.memq current all then all else current :: all)
    in
    let all = MKMap.map (StringMap.map (fun x -> lazy (crunch_modules x))) all in
    Resolve.context all current_scope
  in
  let contents = Option.map (Resolve.go_module ~cache lift) contents in
  let vars =
    VarTbl.to_seq state.vars
    |> Seq.map (fun (k, v) -> (k, Resolve.go_value_doc ~cache lift !v))
    |> VarTbl.of_seq
  in
  { current_module; contents; errors = state.errs; comments = state.unused_comments; vars }

let key = D.Programs.key ~name:__MODULE__ get

let get_module ({ contents; _ } : t) = contents

let get_var ({ vars; _ } : t) var = VarTbl.find_opt vars var

let get_modules =
  D.Key.key ~name:(__MODULE__ ^ ".get_modules") @@ fun data () ->
  D.need data D.Programs.Files.files ()
  |> List.fold_left
       (fun modules file ->
         match D.Programs.need_for data key file with
         | None | Some { contents = None; _ } -> modules
         | Some { contents = Some result; _ } -> add_module modules result)
       MKMap.empty
  |> MKMap.map (StringMap.map crunch_modules)
