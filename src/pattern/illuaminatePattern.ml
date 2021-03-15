open Pattern

let src = Logs.Src.create ~doc:"File pattern parsing and matching" __MODULE__

module Log = (val Logs.src_log src)

module type S = sig
  type t

  val matches : Fpath.t -> t -> bool

  val iter : (Fpath.t -> unit) -> ?path:Fpath.t -> root:Fpath.t -> t -> unit
end

type t = Pattern.segment list

let pp = Pattern.pp

let parse str = Lexing.from_string ~with_positions:false str |> Parse.main

let matches_seg p = function
  | Literal l -> p = l
  | Regex r -> Re.execp r p
  | Anything -> true

let rec matches_pat paths pats =
  match (paths, pats) with
  (* Trivial cases: Empty patterns accept anything, empty paths fail *)
  | _, [ Anything ] | _, [] -> true
  | [], _ :: _ -> false
  (* Simple literals just consume immediately *)
  | path :: paths, ((Literal _ | Regex _) as pat) :: pats ->
      if matches_seg path pat then matches_pat paths pats else false
  (* Otherwise, we've got a/b/c vs **/d/e. Try a/b/c ~ d/e and then b/c ~ **/d/e. Note, this is
     technically backtracking: it'd be good to optimise this in the future. *)
  | _ :: paths', Anything :: pats' -> matches_pat paths pats' || matches_pat paths' pats

let matches path pattern = matches_pat (Fpath.segs path) pattern

let of_seqs x = String.concat "/" x |> Fpath.v

(** The same as [matches_pat] but tracking the current sequence. *)
let rec match_end ~before paths pats =
  match (paths, pats) with
  | _, [ Anything ] | _, [] -> Some (List.rev before |> of_seqs, of_seqs paths)
  | [], _ :: _ -> None
  | path :: paths, ((Literal _ | Regex _) as pat) :: pats ->
      if matches_seg path pat then match_end ~before:(path :: before) paths pats else None
  | path :: paths', Anything :: pats' -> (
      let before = path :: before in
      match match_end ~before paths' pats with
      | None -> match_end ~before paths pats'
      | Some _ as x -> x)

let match_end path pattern = match_end ~before:[] (Fpath.segs path) pattern

(** A set of files we'll always skip over. Just saves us iterating over VCS directories and whatnot. *)
let ignored_files =
  List.to_seq [ ".git"; ".svn"; "_build"; "_opam"; "_esy"; "node_modules" ]
  |> Seq.map (fun x -> (x, ()))
  |> Hashtbl.of_seq

let iter_pats iter ~root path pats =
  let rec visit path =
    let path_s = Fpath.to_string path in
    if not (Sys.file_exists path_s) then
      Log.info (fun f -> f "File %a does not exist" Fpath.pp path)
    else if Sys.is_directory path_s then (
      Log.debug (fun f -> f "Scanning directory %a" Fpath.pp path);
      match Sys.readdir path_s with
      | files ->
          Array.iter
            (fun child -> if not (Hashtbl.mem ignored_files child) then visit Fpath.(path / child))
            files
      | exception Sys_error e -> Log.err (fun f -> f "Error when scanning %a:%s " Fpath.pp path e))
    else if
      Fpath.is_rooted ~root path
      && List.exists (Fpath.relativize ~root path |> Option.get |> matches) pats
    then iter path
  in

  Log.info (fun f -> f "Listing all files from %a" Fpath.pp path);
  visit path

module Union = struct
  type pattern = t

  module StringMap = Map.Make (String)

  type t =
    | Empty
    | Always
    | Any of pattern list
    | Branch of
        { literals : t StringMap.t;
          regexes : (Re.re * t) list
        }

  let rec of_pattern = function
    | [] -> Always
    | Literal l :: xs -> Branch { literals = StringMap.singleton l (of_pattern xs); regexes = [] }
    | Regex r :: xs -> Branch { literals = StringMap.empty; regexes = [ (r, of_pattern xs) ] }
    | Anything :: _ as xs -> Any [ xs ]

  let rec union left right =
    match (left, right) with
    | Empty, x | x, Empty -> x
    | Always, _ | _, Always -> Always
    | Any l, Any r -> Any (CCList.union ~eq:( == ) l r)
    | Any ps, (Branch _ as b) | (Branch _ as b), Any ps ->
        List.fold_left (fun b p -> union b (of_pattern p)) b ps
    | Branch l, Branch r ->
        Branch
          { literals = StringMap.union (fun _ l r -> Some (union l r)) l.literals r.literals;
            regexes = l.regexes @ r.regexes
          }

  let of_list = List.fold_left (fun b p -> union b (of_pattern p)) Empty

  let rec matches_union paths = function
    | Empty -> false
    | Always -> true
    | Any ps -> List.exists (matches_pat paths) ps
    | Branch { literals; regexes } -> (
      match paths with
      | [] -> false
      | path :: paths ->
          (match StringMap.find_opt path literals with
          | None -> false
          | Some u -> matches_union paths u)
          || List.exists (fun (re, u) -> Re.execp re path && matches_union paths u) regexes)

  let matches p = matches_union (Fpath.segs p)

  let iter_union iter ~root =
    let rec visit path = function
      | Empty -> ()
      | Always -> iter_pats iter ~root path [ [] ]
      | Any xs -> iter_pats iter ~root path xs
      | Branch { literals; regexes = []; _ } ->
          StringMap.iter (fun child v -> visit Fpath.(path / child) v) literals
      | Branch { literals; regexes; _ } -> (
        match Sys.readdir (Fpath.to_string path) with
        | children ->
            children
            |> Array.iter @@ fun child ->
               let base = StringMap.find_opt child literals |> Option.value ~default:Empty in
               List.fold_left
                 (fun matching (re, this) ->
                   if Re.execp re child then union matching this else matching)
                 base regexes
               |> visit Fpath.(path / child)
        | exception Sys_error e -> Log.err (fun f -> f "Cannot read %a: %s" Fpath.pp path e))
    in
    visit

  let iter iter ?path ~root p =
    match path with
    | None -> iter_union iter ~root root p
    | Some path ->
        if Fpath.is_rooted ~root (Fpath.to_dir_path path) then iter_union iter ~root path p
end

let iter iter ?path ~root p = Union.of_list [ p ] |> Union.iter iter ?path ~root
