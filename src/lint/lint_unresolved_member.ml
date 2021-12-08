open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Module_resolve
module SSet = Set.Make (String)

let options : SSet.t IlluaminateConfig.Category.key =
  let open IlluaminateConfig in
  let open Term in
  category
  |> Category.add
     @@ let+ f =
          field ~name:"dynamic-modules" ~default:[]
            ~comment:
              "Modules which may have members which are not documented. Modules within this list \
               are skipped by the `var:unresolved-member` warning."
            Converter.(list string)
        in
        SSet.of_list f

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:unresolved-member"

let should_skip skip : R.Reference.t -> bool = function
  | Var _ | Dot _ | Reference (External _ | Unknown _) -> false
  | Reference (Internal { in_module; _ }) -> SSet.mem in_module.id skip

let check skip r data ~table ~idx ~key =
  match R.get_name data table with
  | None -> ()
  | Some (rf, ({ descriptor = Table _; _ } as n))
    when R.is_interesting rf n && not (should_skip skip rf) -> (
    (* For now, we only warn on /interesting/ tables. This hopefully catches most typos, while not
       causing any false-positives. *)
    match R.get_name data idx with
    | Some _ -> ()
    | None -> r.r ~tag "Unknown field %s in %a" (Node.contents.get key) R.Reference.pp rf)
  | Some _ -> ()

let expr skip { data; program; _ } r = function
  | Ref (NDot { tbl = Ref table; key; _ } as idx) ->
      let data = IlluaminateData.need data R.key program in
      check skip r data ~table ~idx ~key
  | _ -> ()

let linter = make ~options ~tags:[ tag ] ~expr ()
