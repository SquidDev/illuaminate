open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let linter =
  let tag = Error.Tag.make Error.Warning "doc:unresolved-reference" in

  let check ~notes ~span = function
    | Reference.Unknown x -> notes := note ~span ~tag "Unknown reference %S." x :: !notes
    | Internal _ | External _ -> ()
  in

  let rec type_ ~notes ~span = function
    | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
    | Named (r, _) -> check ~notes ~span r
    | Function { args; return = rs, r } ->
        List.iter (fun { Type.ty; _ } -> type_ ~notes ~span ty) args;
        List.iter (type_ ~notes ~span) rs;
        Option.iter (type_ ~notes ~span) r
    | Table xs -> List.iter (table_entry ~notes ~span) xs
    | Union xs -> List.iter (type_ ~notes ~span) xs
  and table_entry ~notes ~span = function
    | Type.Field { value = x; optional = _; key = _ } | Item x | Many x -> type_ ~notes ~span x
    | Hash { key; optional = _; value } -> type_ ~notes ~span key; type_ ~notes ~span value
  in

  let check_abstract ~notes ~span =
    object
      inherit abstract_iter as super

      method! reference = check ~notes ~span

      method! type_ = type_ ~notes ~span

      method! omd node =
        let open Omd in
        super#omd node;
        match node with
        | Html ("illuaminate:ref", [ ("link", Some x) ], _) ->
            notes := note ~span ~tag "Unknown reference %S." x :: !notes
        | _ -> ()
    end
  in

  make_no_opt ~tags:[ tag ]
    ~program:(fun () context prog ->
      match Data.get prog E.key context.data |> E.get_module with
      | None -> []
      | Some m ->
          let notes = ref [] in
          let iter = iter_of (check_abstract ~notes) in
          iter#documented iter#module_info m;
          !notes)
    ()
