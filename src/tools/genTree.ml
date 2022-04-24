open Ppxlib
module A = Ast_builder.Default

let loc = Location.none

module Iter = struct
  let gen_basic_ty name ty =
    match ty.ptyp_desc with
    | Ptyp_constr ({ txt = Lident n; _ }, []) ->
        A.eapply ~loc (A.evar ~loc n) [ [%expr context]; A.evar ~loc name ]
    | _ ->
        A.eapply ~loc (A.evar ~loc "failwith")
          [ A.estring ~loc (Format.asprintf "NYI `%a` on %s" Pprintast.core_type ty name) ]

  let gen_record fields =
    let ps, es =
      fields
      |> List.map (fun { pld_name; pld_type; _ } ->
             ( ({ txt = Lident pld_name.txt; loc }, A.pvar ~loc pld_name.txt),
               gen_basic_ty pld_name.txt pld_type ))
      |> List.split
    in
    (A.ppat_record ~loc ps Closed, A.esequence ~loc es)

  let gen_ctor ({ pcd_args; _ } as ctor) =
    let p, e =
      match pcd_args with
      | Pcstr_tuple ts ->
          let ps, es =
            ts
            |> List.mapi (fun i t ->
                   let name = Char.chr (97 + i) |> String.make 1 in
                   (A.pvar ~loc name, gen_basic_ty name t))
            |> List.split
          in
          (A.ppat_tuple ~loc ps, A.esequence ~loc es)
      | Pcstr_record r -> gen_record r
    in
    (A.pconstruct ctor (Some p), e)

  let gen_def { ptype_name; ptype_kind; ptype_manifest; _ } =
    let body =
      match (ptype_kind, ptype_manifest) with
      | Ptype_variant v, _ ->
          v
          |> List.map (fun c ->
                 let p, e = gen_ctor c in
                 A.case ~lhs:p ~rhs:e ~guard:None)
          |> A.pexp_function ~loc
          |> A.eabstract ~loc [ [%pat? context] ]
      | Ptype_record r, _ ->
          let p, e = gen_record r in
          A.eabstract ~loc [ [%pat? context]; p ] e
      | Ptype_abstract, Some t ->
          A.eabstract ~loc [ [%pat? context]; A.pvar ~loc "x" ] (gen_basic_ty "x" t)
      | Ptype_open, _ | Ptype_abstract, _ ->
          failwith ("Cannot handle non-concrete type " ^ ptype_name.txt)
    in
    { pvb_pat = A.pvar ~loc ptype_name.txt; pvb_expr = body; pvb_loc = loc; pvb_attributes = [] }

  let gen ty_defs =
    let defs = List.map gen_def ty_defs in
    [%stri
      module Iter = struct
        let () = [%e A.pexp_let ~loc Recursive defs [%expr ()]]
      end]
end

module Elem = struct
  let gen_basic_ty name ty =
    match ty.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "token"; _ }, []) -> A.evar ~loc name
    | Ptyp_constr ({ txt = Lident n; _ }, []) -> A.eapply ~loc (A.evar ~loc n) [ A.evar ~loc name ]
    | _ ->
        A.eapply ~loc (A.evar ~loc "failwith")
          [ A.estring ~loc (Format.asprintf "NYI `%a` on %s" Pprintast.core_type ty name) ]

  let gen_record proj fields =
    let { pld_name; pld_type; _ } = proj fields in
    let pat = ({ txt = Lident pld_name.txt; loc }, A.pvar ~loc pld_name.txt)
    and expr = gen_basic_ty pld_name.txt pld_type in
    (A.ppat_record ~loc [ pat ] Open, expr)

  let gen_ctor field_proj tup_proj ({ pcd_args; _ } as ctor) =
    let p, e =
      match pcd_args with
      | Pcstr_tuple ts ->
          let pat, ty = tup_proj ts in
          (pat, gen_basic_ty "x" ty)
      | Pcstr_record r -> gen_record field_proj r
    in
    (A.pconstruct ctor (Some p), e)

  let gen_def field_proj tup_proj { ptype_name; ptype_kind; ptype_manifest; _ } =
    let body =
      match (ptype_kind, ptype_manifest) with
      | Ptype_variant v, _ ->
          v
          |> List.map (fun c ->
                 let p, e = gen_ctor field_proj tup_proj c in
                 A.case ~lhs:p ~rhs:e ~guard:None)
          |> A.pexp_function ~loc
      | Ptype_record r, _ ->
          let p, e = gen_record field_proj r in
          A.eabstract ~loc [ p ] e
      | Ptype_abstract, Some t -> A.eabstract ~loc [ A.pvar ~loc "x" ] (gen_basic_ty "x" t)
      | Ptype_open, _ | Ptype_abstract, _ ->
          failwith ("Cannot handle non-concrete type " ^ ptype_name.txt)
    in
    { pvb_pat = A.pvar ~loc ptype_name.txt; pvb_expr = body; pvb_loc = loc; pvb_attributes = [] }
end

module First = struct
  let gen_def =
    let gen_tail = function
      | [] -> assert false
      | x :: xs -> (A.ppat_tuple ~loc ([%pat? x] :: List.map (fun _ -> [%pat? _]) xs), x)
    in
    Elem.gen_def List.hd gen_tail

  let gen ty_defs =
    let defs = List.map gen_def ty_defs in
    [%stri
      module First = struct
        [%%i A.pstr_value ~loc Recursive defs]
      end]
end

module Last = struct
  let gen_def =
    let rec gen_tail = function
      | [] -> assert false
      | [ x ] -> ([ [%pat? x] ], x)
      | _ :: xs ->
          let ps, t = gen_tail xs in
          ([%pat? _] :: ps, t)
    in
    let gen_tail xs =
      let ps, t = gen_tail xs in
      (A.ppat_tuple ~loc ps, t)
    in
    let rec last = function
      | [] -> assert false
      | [ x ] -> x
      | _ :: xs -> last xs
    in
    Elem.gen_def last gen_tail

  let gen ty_defs =
    let defs = List.map gen_def ty_defs in
    [%stri
      module Last = struct
        [%%i A.pstr_value ~loc Recursive defs]
      end]
end

let () =
  let rec find_ty_def = function
    | [] -> failwith "Cannot find type."
    | { pstr_desc = Pstr_type (_, defs); _ } :: _
      when List.exists
             (function
               | { ptype_name = { txt = "expr"; _ }; _ } -> true
               | _ -> false)
             defs -> defs
    | _ :: xs -> find_ty_def xs
  in
  let ty_defs = find_ty_def Syntax.tree in
  [ Iter.gen; First.gen; Last.gen ]
  |> List.map (fun f -> f ty_defs)
  |> Pprintast.structure Format.std_formatter
