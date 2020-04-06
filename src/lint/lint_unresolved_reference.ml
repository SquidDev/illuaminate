open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:unresolved-reference"

let check ~r ~span = function
  | Reference.Unknown x -> r.r ~span ~tag "Unknown reference %S." x
  | Internal _ | External _ -> ()

let rec type_ ~r ~span = function
  | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
  | Named (n, _) -> check ~r ~span n
  | Function { args; return = rs, rest } ->
      List.iter (fun { Type.ty; _ } -> type_ ~r ~span ty) args;
      List.iter (type_ ~r ~span) rs;
      Option.iter (type_ ~r ~span) rest
  | Table xs -> List.iter (table_entry ~r ~span) xs
  | Union xs -> List.iter (type_ ~r ~span) xs

and table_entry ~r ~span = function
  | Type.Field { value = x; optional = _; key = _ } | Item x | Many x -> type_ ~r ~span x
  | Hash { key; optional = _; value } -> type_ ~r ~span key; type_ ~r ~span value

let check_abstract ~r ~span =
  object
    inherit abstract_iter as super

    method! reference = check ~r ~span

    method! type_ = type_ ~r ~span

    method! omd node =
      let open Omd in
      super#omd node;
      match node with
      | Html ("illuaminate:ref", [ ("link", Some x) ], _) ->
          r.r ~span ~tag "Unknown reference %S." x
      | _ -> ()
  end

let linter =
  make_no_opt ~tags:[ tag ]
    ~program:(fun () context r prog ->
      match IlluaminateData.need context.data E.key prog |> E.get_module with
      | None -> ()
      | Some m ->
          let iter = iter_of (check_abstract ~r) in
          iter#documented iter#module_info m)
    ()
