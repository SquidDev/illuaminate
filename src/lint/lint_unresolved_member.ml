open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Module_resolve

let tag = Error.Tag.make Error.Warning "var:unresolved-member"

let check data ~table ~idx ~key =
  match R.get_name data table with
  | None -> []
  | Some (r, ({ descriptor = Table _; _ } as n)) when R.is_interesting r n -> (
    (* For now, we only warn on /interesting/ tables. This hopefully catches most typos, while not
       causing any false-positives. *)
    match R.get_name data idx with
    | Some _ -> []
    | None -> [ note ~tag "Unknown field %s in %a" (Node.contents.get key) R.Reference.pp r ] )
  | Some _ -> []

let expr () { data; program; _ } = function
  | Ref (NDot { tbl = Ref table; key; _ } as idx) ->
      let data = IlluaminateData.need data R.key program in
      check data ~table ~idx ~key
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ()
