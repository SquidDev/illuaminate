open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter

open struct
  module R = Module_resolve
end

let linter =
  let tag = Error.Tag.make Error.Warning "var:unknown-module-member" in
  let expr () { data; program; _ } = function
    | Ref (NDot { tbl = Ref tbl; key; _ } as whole) -> (
        let data = IlluaminateData.get data R.key program in
        (* Find nodes tbl.whole where tbl is resolved by whole isn't *)
        match (R.get_name data tbl, lazy (R.get_name data whole)) with
        | None, _ | Some _, (lazy (Some _)) -> []
        | Some (r, _), (lazy None) ->
            [ note ~tag "Unknown field %s in %a" (Node.contents.get key) Reference.pp_resolved r ] )
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ()
