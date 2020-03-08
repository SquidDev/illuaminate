open IlluaminateCore
open Doc_syntax
open! Reference

module Key = struct
  type t = Syntax.var

  let equal = ( == )

  let hash = Hashtbl.hash
end

module VarCache = Hashtbl.Make (Key)
module StringMap = Map.Make (String)

module Reference = struct
  type t =
    | Reference of Reference.resolved
    | Var of Resolve.var
    | Dot of t * string

  let rec root : t -> _ = function
    | Reference r -> `Reference r
    | Var v -> `Var v
    | Dot (d, _) -> root d

  let rec pp out : t -> unit = function
    | Reference r -> Reference.pp_resolved out r
    | Var v -> Format.fprintf out "%s" v.name
    | Dot (d, f) -> Format.fprintf out "%a.%s" pp d f
end

open Reference

type t =
  { resolved : Resolve.t;
    modules : module_info documented StringMap.t;
    doc : Doc_extract.t;
    cache : (Reference.t * value documented) option VarCache.t
  }

let key =
  let open IlluaminateData in
  Programs.key ~name:__MODULE__ @@ fun data prog ->
  { resolved = need data Resolve.key prog;
    modules = need data Doc_extract.get_modules ();
    doc = need data Doc_extract.key prog;
    cache = VarCache.create 16
  }

let require = Some (Global.parse "require")

let mk_module ({ descriptor = { mod_name; mod_contents; _ }; definition; _ } as d) =
  Some
    ( Reference.Reference (Internal { in_module = mod_name; definition; name = Module }),
      { d with descriptor = mod_contents } )

let rec get_var ({ cache; resolved; doc; modules } as store) var :
    (Reference.t * value documented) option =
  let go () =
    let resolved = Resolve.get_var var resolved in
    let from_doc =
      (* Locate a documented module. *)
      match resolved with
      | { kind = Global; name; _ } -> (
        match StringMap.find_opt name modules with
        | Some ({ descriptor = { mod_kind = Module; _ }; _ } as d) -> mk_module d
        | _ -> None )
      | { kind = Local _; definitions = [ (_, OfExpr e) ]; _ } -> get_expr store e
      | _ -> None
    in
    match from_doc with
    | Some _ -> from_doc
    | None ->
        (* If we've found nothing, then attempt to find it in the current program. *)
        Doc_extract.get_var doc resolved |> Option.map (fun x -> (Var resolved, x))
  in
  match VarCache.find_opt cache var with
  | Some v -> v
  | None ->
      let v = go () in
      VarCache.add cache var v; v

and get_expr ({ resolved; modules; _ } as store) :
    Syntax.expr -> (Reference.t * value documented) option = function
  | Syntax.Ref n -> get_name store n
  | Parens { paren_expr = e; _ } -> get_expr store e
  | ECall (Call { fn; args }) when Global.of_expr resolved fn = require -> (
    match Syntax.Helpers.get_call_args args with
    | Some (Mono (String { lit_value; _ })) ->
        (* Identify calls to require("foo"), and find a module "foo". *)
        StringMap.find_opt lit_value modules |> CCOpt.flat_map mk_module
    | _ -> None )
  | _ -> None

and get_name store : Syntax.name -> (Reference.t * value documented) option = function
  | NVar v -> get_var store v
  | NDot { tbl; key; _ } ->
      let find = function
        | reference, { descriptor = Table ks; _ } ->
            let key = Node.contents.get key in
            List.assoc_opt key ks |> Option.map (fun x -> (Reference.Dot (reference, key), x))
        | _ -> None
      in
      get_expr store tbl |> CCOpt.flat_map find
  | NLookup _ -> None
