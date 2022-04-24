(** A modified version of ppxlib's traverse library. *)

(** TODO: Pull this out and replace it with something sane. *)

(* The MIT License

   Copyright (c) 2018 Jane Street Group, LLC opensource@janestreet.com

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
   associated documentation files (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge, publish, distribute,
   sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or
   substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE. *)

open Ppxlib
open Ast_builder.Default
module LMap = Map.Make (Longident)
module LSet = Set.Make (Longident)

type options =
  { names : string LMap.t;  (** Maps type names to method names. *)
    skip : LSet.t;  (** Set of types which should not be visited at all. *)
    prefix : string  (** The prefix to use for the generated classes. *)
  }

let alphabet =
  Array.init
    (Char.code 'z' - Char.code 'a' + 1)
    (fun i -> String.make 1 (Char.chr (i + Char.code 'a')))

let vars_of_list ~get_loc = List.mapi (fun i x -> { txt = alphabet.(i); loc = get_loc x })
let evar_of_var { txt; loc } = evar ~loc txt
let pvar_of_var { txt; loc } = pvar ~loc txt
let tvar_of_var { txt; loc } = ptyp_var ~loc txt
let evars_of_vars = List.map evar_of_var
let pvars_of_vars = List.map pvar_of_var
let tvars_of_vars = List.map tvar_of_var
let record ~loc flds = pexp_record ~loc flds None

let construct ~loc id args =
  pexp_construct ~loc id
    (match args with
    | [] -> None
    | _ -> Some (pexp_tuple ~loc args))

let tuple ~loc es = pexp_tuple ~loc es
let abstract ~loc patt expr = pexp_fun ~loc Nolabel None patt expr

module Backends = struct
  class type what =
    object
      (** The name of this transformer. *)
      method name : string

      (** Generate a higher-ordered application *)
      method higher_apply : loc:Location.t -> expression -> expression list -> expression

      (** The type of a method/function on this object. *)
      method typ : loc:Location.t -> core_type -> core_type

      (** The type of a method on this object. *)
      method higher_typ : loc:Location.t -> core_type -> core_type

      method any : loc:Location.t -> expression

      method combine :
        loc:Location.t -> (string loc * expression) list -> reconstruct:expression -> expression
    end

  let iterator : what =
    object
      method name = "iter"
      method higher_apply = eapply
      method typ ~loc ty = [%type: [%t ty] -> unit]
      method higher_typ ~loc ty = [%type: [%t ty] -> unit]
      method any ~loc = [%expr fun _ -> ()]

      method combine ~loc combinators ~reconstruct:_ =
        match List.rev combinators with
        | [] -> [%expr ()]
        | (_, expr) :: rest ->
            List.fold_left (fun acc (_v, expr) -> pexp_sequence ~loc expr acc) expr rest
    end

  let mapper : what =
    object
      method name = "map"
      method higher_apply = eapply
      method typ ~loc ty = ptyp_arrow ~loc Nolabel ty ty
      method higher_typ ~loc ty = ptyp_arrow ~loc Nolabel ty ty
      method any ~loc = [%expr fun x -> x]

      method combine ~loc combinators ~reconstruct =
        List.fold_right
          (fun (v, expr) acc ->
            pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar_of_var v) ~expr ] acc)
          combinators reconstruct
    end

  let all = [ iterator; mapper ]
end

let method_name map name =
  match LMap.find_opt name map with
  | Some x -> x
  | None -> (
    match name with
    | Lident n -> n
    | Ldot (id, "t") | id -> Longident.flatten_exn id |> String.concat "_" |> String.lowercase_ascii
    )

type what = Backends.what

let mapper_type ~(what : what) ~loc type_name params =
  let vars = vars_of_list params ~get_loc:(fun t -> t.ptyp_loc) in
  let params = tvars_of_vars vars in
  let ty = ptyp_constr ~loc type_name params in
  let ty =
    List.fold_right
      (fun param ty ->
        let loc = param.ptyp_loc in
        ptyp_arrow ~loc Nolabel (what#higher_typ ~loc param) ty)
      params (what#typ ~loc ty)
  in
  ptyp_poly ~loc vars ty

let constrained_mapper ~(what : what) ?(is_gadt = false) mapper td =
  let vars = vars_of_list td.ptype_params ~get_loc:(fun (t, _) -> t.ptyp_loc) in
  let make_type params =
    let loc = td.ptype_loc in
    let ty = ptyp_constr ~loc (Loc.map td.ptype_name ~f:lident) params in
    List.fold_right
      (fun param ty ->
        let loc = param.ptyp_loc in
        ptyp_arrow ~loc Nolabel (what#higher_typ ~loc param) ty)
      params (what#typ ~loc:td.ptype_loc ty)
  in
  let typ =
    let loc = td.ptype_loc in
    ptyp_poly ~loc vars (make_type (tvars_of_vars vars))
  in
  let mapper =
    if is_gadt then
      let typs = List.map (fun v -> ptyp_constr ~loc:v.loc (Loc.map v ~f:lident) []) vars in
      List.fold_right
        (fun v e -> pexp_newtype ~loc:v.loc v e)
        vars
        (pexp_constraint ~loc:mapper.pexp_loc mapper (make_type typs))
    else mapper
  in
  pexp_poly ~loc:mapper.pexp_loc mapper (Some typ)

let rec pre_mem x skip =
  if LSet.mem x skip then true
  else
    match x with
    | Lident _ | Lapply _ -> false
    | Ldot (x, _) -> pre_mem x skip

let rec type_expr_mapper ~(what : what) ~options te =
  let loc = te.ptyp_loc in
  match te.ptyp_desc with
  | Ptyp_var s -> evar ~loc ("_" ^ s)
  | Ptyp_tuple tes ->
      let vars = vars_of_list tes ~get_loc:(fun t -> t.ptyp_loc) in
      let deconstruct = ppat_tuple ~loc (pvars_of_vars vars) in
      let reconstruct = tuple ~loc (evars_of_vars vars) in
      let mappers = map_variables ~what ~options vars tes in
      abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)
  | Ptyp_constr (path, _) when pre_mem path.txt options.skip -> what#any ~loc
  | Ptyp_constr (path, params) -> (
      let map =
        pexp_send ~loc (evar ~loc "self")
          { txt = method_name options.names path.txt; loc = path.loc }
      in
      match params with
      | [] -> map
      | _ -> eapply ~loc map (List.map (fun te -> type_expr_mapper ~options ~what te) params))
  | _ -> what#any ~loc

and map_variables ~(what : what) ~options vars tes =
  List.map2
    (fun te var ->
      (var, eapply ~loc:te.ptyp_loc (type_expr_mapper ~what ~options te) [ evar_of_var var ]))
    tes vars

let gen_record' ~(what : what) ~options ~loc lds =
  let vars = List.map (fun ld -> ld.pld_name) lds in
  let deconstruct =
    ppat_record ~loc (List.map (fun v -> (Loc.map v ~f:lident, pvar_of_var v)) vars) Closed
  in
  let reconstruct = record ~loc (List.map (fun v -> (Loc.map v ~f:lident, evar_of_var v)) vars) in
  let mappers = map_variables ~what ~options vars (List.map (fun ld -> ld.pld_type) lds) in
  (deconstruct, reconstruct, mappers)

let gen_record ~(what : what) ~options ~loc lds =
  let deconstruct, reconstruct, mappers = gen_record' ~what ~options lds ~loc in
  abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)

let is_constant_constructor cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> true
  | _ -> false

let erase_type_variables =
  object
    inherit Ast_traverse.map as super

    method! core_type_desc =
      function
      | Ptyp_var _ -> Ptyp_any
      | x -> super#core_type_desc x
  end

let gen_variant ~(what : what) ~options ~loc cds =
  if List.for_all is_constant_constructor cds then what#any ~loc
  else
    let cases =
      cds
      |> List.map (fun cd ->
             let cstr = Loc.map cd.pcd_name ~f:lident in
             let loc = cd.pcd_loc in
             let args =
               match cd.pcd_res with
               | None -> cd.pcd_args
               | Some _ ->
                   (* This is a big sur-approximation but it's enough for our only use of GADTs in
                      ppx_custom_format *)
                   erase_type_variables#constructor_arguments cd.pcd_args
             in
             match args with
             | Pcstr_tuple args ->
                 let vars = vars_of_list args ~get_loc:(fun t -> t.ptyp_loc) in
                 let deconstruct =
                   ppat_construct cstr ~loc
                     (match vars with
                     | [] -> None
                     | _ -> Some (ppat_tuple ~loc (pvars_of_vars vars)))
                 in
                 let reconstruct = construct cstr ~loc (evars_of_vars vars) in
                 let mappers = map_variables ~what ~options vars args in
                 case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None
             | Pcstr_record labels ->
                 let deconstruct, reconstruct, mappers = gen_record' ~loc ~options ~what labels in
                 let deconstruct = ppat_construct ~loc cstr (Some deconstruct) in
                 let reconstruct = construct ~loc cstr [ reconstruct ] in
                 case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None)
    in
    abstract ~loc (pvar ~loc "x") (pexp_match ~loc (evar ~loc "x") cases)

let gen_mapper ~(what : what) ~options td =
  let body =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_open -> what#any ~loc
    | Ptype_record lds -> gen_record ~what ~options lds ~loc
    | Ptype_variant cds -> gen_variant ~what ~options cds ~loc
    | Ptype_abstract -> (
      match td.ptype_manifest with
      | None -> what#any ~loc
      | Some te -> type_expr_mapper ~what ~options te)
  in
  List.fold_right
    (fun (ty, _) acc ->
      let loc = ty.ptyp_loc in
      match ty.ptyp_desc with
      | Ptyp_var s -> pexp_fun ~loc Nolabel None (pvar ~loc ("_" ^ s)) acc
      | _ -> pexp_fun ~loc Nolabel None (ppat_any ~loc) acc)
    td.ptype_params body

let type_deps ~skip tds =
  let collect =
    object
      inherit [int LMap.t] Ast_traverse.fold as super

      method! core_type t acc =
        let acc =
          match t.ptyp_desc with
          | Ptyp_constr (id, vars) when not (pre_mem id.txt skip) ->
              LMap.add id.txt (List.length vars) acc
          | _ -> acc
        in
        super#core_type t acc
    end
  in
  let map =
    List.fold_left
      (fun map td ->
        let map = collect#type_kind td.ptype_kind map in
        match (td.ptype_kind, td.ptype_manifest) with
        | Ptype_abstract, Some ty -> collect#core_type ty map
        | _ -> map)
      LMap.empty tds
  in
  List.fold_left (fun map td -> LMap.remove (Lident td.ptype_name.txt) map) map tds |> LMap.bindings

let gen_class ~(what : what) ~loc ~options tds =
  let tds = tds |> List.filter (fun x -> not (pre_mem (Lident x.ptype_name.txt) options.skip)) in
  let virtual_methods =
    List.map
      (fun (id, arity) ->
        pcf_method ~loc
          ( { txt = method_name options.names id; loc },
            Public,
            Cfk_virtual
              (mapper_type ~what ~loc { txt = id; loc } (List.init arity (fun _ -> ptyp_any ~loc)))
          ))
      (type_deps ~skip:options.skip tds)
  in
  let methods =
    List.map
      (fun td ->
        let loc = td.ptype_loc in
        let mapper = gen_mapper ~what ~options td in
        let is_gadt =
          match td.ptype_kind with
          | Ptype_variant cds ->
              List.exists
                (fun cd ->
                  match cd.pcd_res with
                  | None -> false
                  | Some _ -> true)
                cds
          | _ -> false
        in
        let mapper = constrained_mapper ~what ~is_gadt mapper td in
        pcf_method ~loc (td.ptype_name, Public, Cfk_concrete (Fresh, mapper)))
      tds
  in
  let virt =
    match virtual_methods with
    | [] -> Concrete
    | _ -> Virtual
  in
  class_infos ~loc ~virt ~params:[]
    ~name:{ loc; txt = options.prefix ^ what#name }
    ~expr:
      (pcl_structure ~loc
         (class_structure ~self:[%pat? (self : 'self)] ~fields:(virtual_methods @ methods)))

let gen_str ~(what : what) ~loc ~path:_ (rf, tds) prefix names skip =
  (match rf with
  | Nonrecursive ->
      (* The method name would clash... *)
      Location.raise_errorf ~loc "illuaminate_traverse doesn't support nonrec"
  | Recursive -> ());
  let prefix =
    match prefix with
    | None -> "traverse_"
    | Some x -> x
  and names =
    match names with
    | None -> LMap.empty
    | Some tys -> List.fold_left (fun map (ty, name) -> LMap.add ty name map) LMap.empty tys
  and skip =
    match skip with
    | None -> LSet.empty
    | Some tys -> List.fold_left (fun s name -> LSet.add name s) LSet.empty tys
  in
  let cl = gen_class ~what ~loc ~options:{ names; skip; prefix } tds in
  [ pstr_class ~loc:cl.pci_loc [ cl ] ]

let register () =
  let derivers =
    List.map
      (fun what ->
        let args =
          Deriving.Args.(
            empty
            +> arg "prefix" (estring __)
            +> arg "types" (pexp_tuple (pexp_ident __ ^:: estring __ ^:: nil) |> pack2 |> elist)
            +> arg "skip" (pexp_ident __ ||| (pexp_construct __ __ >>| fun f x _ -> f x) |> elist))
        in
        let generator = Deriving.Generator.make args (gen_str ~what) in
        Deriving.add ("illuaminateDeriving_traverse_" ^ what#name) ~str_type_decl:generator)
      Backends.all
  in
  Deriving.add_alias "illuaminateDeriving_traverse" (List.rev derivers) |> Deriving.ignore
