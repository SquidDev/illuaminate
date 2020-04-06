open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Module_resolve

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:unresolved-member"

let check r data ~table ~idx ~key =
  match R.get_name data table with
  | None -> ()
  | Some (rf, ({ descriptor = Table _; _ } as n)) when R.is_interesting rf n -> (
    (* For now, we only warn on /interesting/ tables. This hopefully catches most typos, while not
       causing any false-positives. *)
    match R.get_name data idx with
    | Some _ -> ()
    | None -> r.r ~tag "Unknown field %s in %a" (Node.contents.get key) R.Reference.pp rf )
  | Some _ -> ()

let expr () { data; program; _ } r = function
  | Ref (NDot { tbl = Ref table; key; _ } as idx) ->
      let data = IlluaminateData.need data R.key program in
      check r data ~table ~idx ~key
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
