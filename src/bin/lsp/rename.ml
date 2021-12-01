open IlluaminateCore
open IlluaminateSemantics
open Resolve
open Lsp.Types
open Lsp_convert
module Data = IlluaminateData
module StringMap = Map.Make (String)

let err f = Format.kasprintf Result.error f

(** Whether this variable is suitable for renaming. *)
let check_renamable { definitions; kind; name; _ } =
  match kind with
  | ImplicitArg _ -> err "Implicit %s cannot be renamed." name
  (* For now we prohibit renaming globals. If we're doing this, we should rename their usages in
     other modules too. *)
  | Global -> err "Global %S cannot be renamed." name
  (* We can rename a variable if all its definitions have a corresponding assignment. *)
  | Loop { def; _ } | Local { def; _ } | Arg { def; _ } ->
      if List.for_all (fun (x, _) -> Option.is_some x) definitions then Ok def
      else err "Variable %S cannot be renamed." name

(** Find the original definition. *)
let find_definition state d =
  let { definitions; _ } = state in
  CCList.find_map
    (function
      | Some ({ node; _ } as v), _ when node == d -> Some v
      | _ -> None)
    definitions
  |> Option.get

let check data position program =
  match Locate.locate position program with
  | Var v
    when Data.get data Resolve.key program |> Resolve.get_var v |> check_renamable |> Result.is_ok
    ->
      Some (Syntax.Spanned.var v |> range)
  | _ -> None

(** Check a predicate holds for all usages of a variable. *)
let check_usages state f =
  let { usages; definitions; _ } = state in
  let check_def = function
    | None, _ -> None
    | Some v, _ -> f v
  in
  CCList.find_map f usages
  |> CCOption.or_lazy ~else_:(fun () -> CCList.find_map check_def definitions)

(** Check whether any usages and definitions of this variable are shadowed by the new name. *)
let check_usage_conflicts v existing_var new_name =
  (* Ensure at every usage point the variable resolves to the same as it originally did. *)
  check_usages v @@ fun { snapshot; node; _ } ->
  let def = StringMap.find_opt new_name snapshot in
  match (def, existing_var) with
  | None, None -> None
  | None, Some _ -> failwith "Impossible: Variables cannot vanish."
  | Some { kind = Global; _ }, None -> failwith "Impossible: Globals should not appear in scope."
  | Some v, None -> Some (node, v)
  | Some d, Some e -> if d == e then None else Some (node, d)

(** Check whether any usages and definitions of this variable are shadowed by the new name. *)
let check_definition_conflicts v existing_var =
  (* Ensure that for every usage point, the variable is not in scope. *)
  check_usages existing_var @@ fun { snapshot; node; _ } ->
  let def = StringMap.find_opt v.name snapshot in
  match def with
  | Some v' when v == v' -> Some node
  | _ -> None

(** Apply a rename, returning the list of edits. *)
let apply_rename var new_name =
  let of_usage xs x : TextEdit.t list =
    let span = Syntax.Spanned.var x.node in
    { range = range span; newText = new_name } :: xs
  in
  let of_definition xs (x, _) =
    match x with
    | None -> xs
    | Some v -> of_usage xs v
  in
  List.fold_left of_definition (List.fold_left of_usage [] var.usages) var.definitions

let rename data position new_name program =
  if not (Ident.is new_name) then err "%S is not a valid identifier." new_name
  else
    match Locate.locate position program with
    | Var v -> (
        let resolved = Data.get data Resolve.key program in
        let var = Resolve.get_var v resolved in
        Result.bind (check_renamable var) @@ fun def ->
        (* Lookup our new variable at the original definition point. *)
        let existing_var = StringMap.find_opt new_name (find_definition var def).snapshot in

        match check_usage_conflicts var existing_var new_name with
        | Some (Var node, def) ->
            err "Usage of %s on line %d would be shadowed by the definition of %s on line %d."
              (Node.contents.get node)
              (Node.span node |> Span.start_line)
              new_name
              (Kind.definition def.kind |> Option.get |> Span.start_line)
        | None -> (
          (* Find a previous definition of this variable at the given point. *)
          match
            CCOption.or_lazy ~else_:(fun () -> Resolve.get_global resolved new_name) existing_var
            |> CCOption.flat_map (check_definition_conflicts var)
          with
          | None -> Ok (apply_rename var new_name)
          | Some (Var node) ->
              err "Renaming %s would shadow usage of %s on line %d." var.name new_name
                (Node.span node |> Span.start_line)))
    | _ -> Error "Not a variable."
