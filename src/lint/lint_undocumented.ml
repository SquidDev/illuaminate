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
    type a b.
    (r:b reporter -> span:Span.t -> a -> unit) -> r:b reporter -> a documented -> string -> unit =
 fun child ~r { description; descriptor; definition; _ } message ->
  check ~r ~span:definition ~tag description message;
  child ~r ~span:definition descriptor

and ty ~r ~span:_ { type_name; type_members; _ } =
  type_members
  |> List.iter @@ fun { member_name; member_value; _ } ->
     Printf.sprintf "Type member `%s:%s` is exported, but has no documentation." type_name
       member_name
     |> documented value ~r member_value

let linter =
  make_no_opt ~tags:[ tag; arg_tag; return_tag ]
    ~file:(fun () context r file ->
      match IlluaminateData.need context E.file file |> E.get_page with
      | None -> ()
      | Some { description; descriptor; definition; _ } ->
          (* Modules are a little odd, as we allow any module with the same name to have
             documentation. *)
          (match description with
          | Some _ -> ()
          | None ->
              let module MN = Map.Make (Namespace) in
              let module MS = Map.Make (String) in
              let has_any =
                IlluaminateData.need context E.all_pages ()
                |> MN.find_opt descriptor.page_ref.namespace
                |> CCOption.flat_map (MS.find_opt descriptor.page_ref.id)
                |> CCOption.flat_map (fun (x : _ documented) -> x.description)
                |> Option.is_some
              in
              if not has_any then r.r ~span:definition ~tag "Module is missing documentation");

          descriptor.page_types
          |> List.iter (fun ({ descriptor = { type_name; _ }; _ } as t) ->
                 Printf.sprintf "Type '%s' is exported, but has no documentation." type_name
                 |> documented ty ~r t);
          Option.iter (value ~r ~span:definition) descriptor.page_value)
    ()
