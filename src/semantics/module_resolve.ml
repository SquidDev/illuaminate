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
module NMap = Map.Make (Namespace)

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
    modules : value documented StringMap.t;
    libraries : value documented StringMap.t;
    doc : Doc_extract.t option;
    cache : (Reference.t * value documented) option VarCache.t
  }

let compute ~data ~resolved ?doc () =
  let modules ns =
    IlluaminateData.need data Doc_extract.all_pages ()
    |> NMap.find_opt ns
    |> Option.value ~default:StringMap.empty
    |> StringMap.to_seq
    |> Seq.filter_map (function
         | name, ({ descriptor = { page_value = Some value; _ }; _ } as x) ->
             Some (name, { x with descriptor = value })
         | _ -> None)
    |> StringMap.of_seq
  in
  { resolved;
    modules = modules Namespace.module_;
    libraries = modules Namespace.library;
    doc;
    cache = VarCache.create 16
  }

let key =
  let module D = IlluaminateData in
  D.Programs.key ~name:__MODULE__ @@ fun data filename _ ->
  compute ~data
    ~resolved:(D.need data Resolve.key filename |> Option.get)
    ?doc:(D.need data Doc_extract.file filename)
    ()

let require = Some (Global.parse "require")

let mk_module ~name ~ns ({ definition; _ } as d) =
  let in_module = { Namespace.Ref.namespace = ns; id = name; title = None } in
  (Reference.Reference (Internal { in_module; definition; name = Module }), d)

let rec get_var ({ cache; resolved; doc; modules; _ } as store) var :
    (Reference.t * value documented) option =
  let go () =
    let resolved = Resolve.get_var var resolved in
    let from_doc =
      (* Locate a documented module. *)
      match resolved with
      | { kind = Global; name; _ } ->
          StringMap.find_opt name modules |> Option.map (mk_module ~name ~ns:Namespace.module_)
      | { kind = Local _; definitions = [ (_, OfExpr e) ]; _ } -> get_expr store e
      | _ -> None
    in
    match from_doc with
    | Some _ -> from_doc
    | None ->
        (* If we've found nothing, then attempt to find it in the current program. *)
        doc
        |> CCOption.flat_map (fun doc -> Doc_extract.get_var doc resolved)
        |> Option.map (fun x -> (Var resolved, x))
  in
  match VarCache.find_opt cache var with
  | Some v -> v
  | None ->
      let v = go () in
      VarCache.add cache var v; v

and get_expr ({ resolved; libraries; _ } as store) :
    Syntax.expr -> (Reference.t * value documented) option = function
  | Syntax.Ref n -> get_name store n
  | Parens { paren_expr = e; _ } -> get_expr store e
  | ECall (Call { fn; args }) when Global.of_expr resolved fn = require -> (
    match Syntax.Helpers.get_call_args args with
    | Some (Mono (String str)) -> (
      (* Identify calls to require("foo"), and find a module "foo". *)
      match Illuaminate.Syntax.Literal.String.parse_value (Node.contents.get str) with
      | Ok name ->
          StringMap.find_opt name libraries |> Option.map (mk_module ~name ~ns:Namespace.library)
      | Error () -> None)
    | _ -> None)
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
      get_expr store tbl |> CCOption.flat_map find
  | NLookup _ -> None

let is_interesting r value =
  match Reference.root r with
  | `Reference (Unknown _) | `Var _ -> Doc_syntax.is_documented value
  | `Reference (Internal _ | External _) -> true

let global_modules =
  let open IlluaminateData in
  let module SSet = Set.Make (String) in
  let module SMap = Map.Make (String) in
  let get data () =
    need data Doc_extract.all_pages ()
    |> NMap.find_opt Namespace.module_ |> Option.value ~default:SMap.empty |> SMap.to_seq
    |> Seq.map fst
    |> Seq.fold_left (Fun.flip SSet.add) SSet.empty
  in
  Key.key ~name:(__MODULE__ ^ ".global_modules") ~eq:SSet.equal ~key:(module Keys.Unit) get
