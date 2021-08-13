open IlluaminateCore
open IlluaminateSemantics
open Linter
open Syntax

let tag = Error.Tag.make ~attr:[ Deprecated; Default ] ~level:Warning "var:deprecated"

let pp_description : Doc.Syntax.description option -> _ = function
  | Some { description = Omd.Paragraph (_, x) :: _; _ } ->
      let desc = Omd.to_plain_text x |> String.trim in
      Some (fun out -> Format.pp_print_string out desc)
  | _ -> None

let expr () ctx r = function
  | Ref name -> (
      let resolve = IlluaminateData.need ctx.data Module_resolve.key ctx.program in
      match Module_resolve.get_name resolve name with
      | Some (_, { deprecated = Some { deprecation_message }; _ }) ->
          r.r ~tag ?detail:(pp_description deprecation_message) "Using deprecated member."
      | None | Some (_, { deprecated = None; _ }) -> ())
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
