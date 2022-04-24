open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens
module Separator = Lint_table_trailing.Separator

let tag = Error.Tag.make ~attr:[ Default ] ~level:Note "format:table-separator"
let fix tok = Fixer.fix (Result.ok % (Node.contents ^= tok))

let expr kind _ r = function
  | Table { table_body; _ } ->
      let tok = Separator.token kind in
      let go = function
        | _, Some t when Node.contents.get t <> tok ->
            r.e ~tag ~fix:(fix tok) ~kind:Token ~source:t
              "Table fields should be separated by a %s." (Separator.show kind)
        | _ -> ()
      in
      List.iter go table_body
  | _ -> ()

let linter = make ~options:Separator.options ~tags:[ tag ] ~expr ()
