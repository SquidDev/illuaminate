open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:undocumented"

let arg_tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:undocumented-arg"

let return_tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:undocumented-return"

let check ~r ~tag ~span description msg =
  match description with
  | Some _ -> ()
  | None -> r.r ~span ~tag "%s" msg

let arg ~r ~span { arg_description; arg_name; _ } =
  Printf.sprintf "Argument `%s` is missing a description" arg_name
  |> check ~r ~span ~tag:arg_tag arg_description

let return ~r ~span { ret_description; _ } =
  check ~r ~span ~tag:return_tag ret_description "@return tag is missing a description"

let rec value ~r ~span = function
  | Function { args; rets; _ } ->
      List.iter (List.iter (arg ~r ~span)) args;
      List.iter (List.iter (return ~r ~span)) rets
  | Table xs ->
      xs
      |> List.iter @@ fun (k, x) ->
         Printf.sprintf "`%s` is exported, but has no documentation." k |> documented value ~r x
  | Type x -> ty ~r ~span x
  | Expr _ | Unknown | Undefined -> ()

and documented :
    type a.
    (r:Syntax.program reporter -> span:Span.t -> a -> unit) ->
    r:Syntax.program reporter ->
    a documented ->
    string ->
    unit =
 fun child ~r { description; descriptor; definition; _ } message ->
  check ~r ~span:definition ~tag description message;
  child ~r ~span:definition descriptor

and ty ~r ~span:_ { type_name; type_members; _ } =
  type_members
  |> List.iter @@ fun { member_name; member_value; _ } ->
     Printf.sprintf "Type member `%s:%s` is exported, but has no documentation." type_name
       member_name
     |> documented value ~r member_value

let modu ~r ~span { mod_contents; mod_types; _ } =
  mod_types
  |> List.iter (fun ({ descriptor = { type_name; _ }; _ } as t) ->
         Printf.sprintf "Type '%s' is exported, but has no documentation." type_name
         |> documented ty ~r t);
  value ~r ~span mod_contents

let linter =
  make_no_opt ~tags:[ tag; arg_tag; return_tag ]
    ~program:(fun () context r prog ->
      match IlluaminateData.need context.data E.key prog |> E.get_module with
      | None -> ()
      | Some m -> documented modu ~r m "Module is missing documentation")
    ()
