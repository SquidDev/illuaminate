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

type t =
  { resolved : Resolve.t;
    docs : module_info documented StringMap.t;
    cache : (resolved * value documented) option VarCache.t
  }

let key =
  Data.key ~name:__MODULE__ @@ fun data prog ->
  { resolved = Data.get prog Resolve.key data;
    docs = Doc_extract.get_modules data;
    cache = VarCache.create 16
  }

let require = Some (Global.parse "require")

let mk_module ({ descriptor = { mod_name; mod_contents; _ }; definition; _ } as d) =
  Some
    ( Internal { in_module = mod_name; definition; name = Module },
      { d with descriptor = mod_contents } )

let rec get_var ({ cache; resolved; docs } as store) var =
  let go () =
    match (Resolve.get_usage var resolved).var with
    | { kind = Global; name; _ } -> (
      match StringMap.find_opt name docs with
      | Some ({ descriptor = { mod_kind = Module; _ }; _ } as d) -> mk_module d
      | _ -> None )
    | { kind = Local _; definitions = [ OfExpr e ]; _ } -> get_expr store e
    | _ -> None
  in
  match VarCache.find_opt cache var with
  | Some v -> v
  | None ->
      let v = go () in
      VarCache.add cache var v; v

and get_expr ({ resolved; docs; _ } as store) : Syntax.expr -> (resolved * value documented) option
    = function
  | Syntax.Ref n -> get_name store n
  | Parens { paren_expr = e; _ } -> get_expr store e
  | ECall (Call { fn; args }) when Global.of_expr resolved fn = require -> (
    match Syntax.Helpers.get_call_args args with
    | Some (Mono (String { lit_value; _ })) -> (
      match StringMap.find_opt lit_value docs with
      | Some ({ descriptor = { mod_kind = Library; _ }; _ } as d) -> mk_module d
      | _ -> None )
    | _ -> None )
  | _ -> None

and get_name store : Syntax.name -> (resolved * value documented) option = function
  | NVar v -> get_var store v
  | NDot { tbl; key; _ } ->
      let find = function
        | reference, { descriptor = Table ks; _ } ->
            List.assoc_opt (Node.contents.get key) ks |> Option.map (fun x -> (reference, x))
        | _ -> None
      in
      get_expr store tbl |> CCOpt.flat_map find
  | NLookup _ -> None
