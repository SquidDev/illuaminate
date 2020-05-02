open IlluaminateCore
open IlluaminateSemantics
open Linter
open Syntax

let tag = Error.Tag.make ~attr:[ Deprecated; Default ] ~level:Warning "var:deprecated"

let pp_description ({ description; _ } : Doc.Syntax.description) out =
  Omd.to_text description |> String.trim |> Format.pp_print_string out

let expr () ctx r = function
  | Ref name -> (
      let resolve = IlluaminateData.need ctx.data Module_resolve.key ctx.program in
      match Module_resolve.get_name resolve name with
      | Some (_, { deprecated = Some { deprecation_message }; _ }) ->
          r.r ~tag
            ?detail:(Option.map pp_description deprecation_message)
            "Using deprecated member."
      | None | Some (_, { deprecated = None; _ }) -> () )
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
