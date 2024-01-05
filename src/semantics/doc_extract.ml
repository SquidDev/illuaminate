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

module Extract_error = struct
  type t = Doc_extract_helpers.extract_error =
    | Value_mismatch of IlluaminateCore.Span.t * value * value
    | Func_and_type of IlluaminateCore.Span.t
    | Func_and_field of IlluaminateCore.Span.t

  let code = "doc:extract"
  let severity = Illuaminate.Error.Error

  let to_error = function
    | Value_mismatch (pos, left, right) ->
        Illuaminate.Error.simple ~code ~severity (IlluaminateCore.Span.to_error_position pos)
          (fun f ->
            f "Conflicting definitions, cannot merge `%s` and `%s`"
              (Doc_extract_helpers.Value.debug_name left)
              (Doc_extract_helpers.Value.debug_name right))
    | Func_and_type pos ->
        Illuaminate.Error.simple ~code ~severity (IlluaminateCore.Span.to_error_position pos)
          (fun f -> f "Documentation comment cannot have both @param/@return and @type")
    | Func_and_field pos ->
        Illuaminate.Error.simple ~code ~severity (IlluaminateCore.Span.to_error_position pos)
          (fun f -> f "Documentation comment cannot have both @param/@return and @type")
end

type t =
  { errors : Doc_extract_helpers.extract_error list;
    comments : unit U.CommentCollection.t;
    contents : page documented option;
    vars : value documented VarTbl.t
  }

let errors { errors; _ } = errors

let detached_comments ({ comments; _ } : t) =
  U.CommentCollection.to_seq_keys comments |> List.of_seq

let crunch_pages : page documented list -> page documented = function
  | [] -> failwith "Impossible"
  | x :: xs ->
      List.fold_left Doc_extract_helpers.Merge.(documented (page ~report:(fun _ -> ()))) x xs

let add_page map modu =
  let ref = modu.descriptor.page_ref in
  NMap.update ref.namespace
    (fun map ->
      Option.value map ~default:StringMap.empty
      |> StringMap.update ref.id (fun x -> Some (modu :: Option.value ~default:[] x))
      |> Option.some)
    map

let build_resolver data contents =
  let all =
    List.fold_left
      (fun modules file ->
        match D.need data U.unresolved_module_file file with
        | None | Some None -> modules
        | Some (Some x) -> add_page modules x)
      NMap.empty
      (D.need data D.Programs.Files.files ())
  in
  let current_scope =
    contents
    |> Option.map @@ fun current ->
       (* Bring all other modules with the same name into scope if required. *)
       let m =
         let ref = current.descriptor.page_ref in
         NMap.find_opt ref.namespace all |> CCOption.flat_map (StringMap.find_opt ref.id)
       in
       match m with
       | None -> current
       | Some all -> crunch_pages (if List.memq current all then all else current :: all)
  in
  let all = NMap.map (StringMap.map (fun x -> lazy (crunch_pages x))) all in
  Resolve.context all current_scope

let for_program data filename =
  let state, _ = D.need data U.Infer.key filename |> Option.get in
  let contents = D.need data U.unresolved_module filename |> Option.get in
  let cache, lift = build_resolver data contents in
  let contents = Option.map (Resolve.go_page ~cache lift) contents in
  let vars =
    VarTbl.to_seq state.vars
    |> Seq.map (fun (k, v) -> (k, Resolve.go_value_doc ~cache lift !v))
    |> VarTbl.of_seq
  in
  { contents; errors = state.errs; comments = state.unused_comments; vars }

let file =
  D.Programs.file_key ~name:(__MODULE__ ^ ".file") @@ fun data filename contents ->
  match contents with
  | Lua _ -> for_program data filename
  | Markdown _ ->
      let contents = D.need data U.unresolved_module_file filename |> Option.get in
      let cache, lift = build_resolver data contents in
      let contents = Option.map (Resolve.go_page ~cache lift) contents in
      { contents; errors = []; comments = U.CommentCollection.create 0; vars = VarTbl.create 0 }

let get_page ({ contents; _ } : t) = contents
let get_var ({ vars; _ } : t) var = VarTbl.find_opt vars var

let all_pages =
  D.Key.key ~name:(__MODULE__ ^ ".get_pages") ~key:(module D.Keys.Unit) @@ fun data () ->
  D.need data D.Programs.Files.files ()
  |> List.fold_left
       (fun pages filename ->
         match D.need data file filename with
         | None | Some { contents = None; _ } -> pages
         | Some { contents = Some result; _ } -> add_page pages result)
       NMap.empty
  |> NMap.map (StringMap.map crunch_pages)

let public_pages =
  D.Key.key ~name:(__MODULE__ ^ ".public_pages") ~key:(module D.Keys.Unit) @@ fun data () ->
  D.need data all_pages ()
  |> NMap.map (fun pages -> StringMap.filter (fun _ modu -> not modu.local) pages)
