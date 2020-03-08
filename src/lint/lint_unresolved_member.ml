open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Module_resolve

(** Determine if a documented term actually has a doc comment. *)
let is_documented = function
  | { Doc.Syntax.description = None; examples = []; see = []; _ } -> false
  | _ -> true

(** Determine if a documented node is "interesting". Something is interesting, if it comes from
    another module or it has a doc comment. *)
let is_interesting r value =
  match R.Reference.root r with
  | `Reference (Unknown _) | `Var _ -> is_documented value
  | `Reference (Internal _ | External _) -> false

let linter =
  let tag = Error.Tag.make Error.Warning "var:unresolved-member" in
  let check data ~table ~idx ~key =
    match R.get_name data table with
    | None -> []
    | Some (r, ({ descriptor = Table _; _ } as n)) when is_interesting r n -> (
      (* For now, we only warn on /interesting/ tables. This hopefully catches most typos, while not
         causing any false-positives. *)
      match R.get_name data idx with
      | Some _ -> []
      | None -> [ note ~tag "Unknown field %s in %a" (Node.contents.get key) R.Reference.pp r ] )
    | Some _ -> []
  in
  let expr () { data; program; _ } = function
    | Ref (NDot { tbl = Ref table; key; _ } as idx) ->
        let data = IlluaminateData.need data R.key program in
        check data ~table ~idx ~key
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ()
