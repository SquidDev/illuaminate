open Ppxlib
module A = Ast_builder.Default

let gen ~loc ~path:_ ((_, tds) : rec_flag * type_declaration list) : structure =
  tds
  |> List.filter_map (fun td ->
         match td.ptype_kind with
         | Ptype_record fields ->
             let name =
               String.mapi
                 (fun i x -> if i = 0 then Char.uppercase_ascii x else x)
                 td.ptype_name.txt
             in
             let fields =
               List.map
                 (fun { pld_name; _ } ->
                   let name = A.ppat_var ~loc pld_name in
                   let long_name = { txt = Lident pld_name.txt; loc = pld_name.loc } in
                   let expr = A.evar ~loc pld_name.txt in
                   let pat = A.ppat_record ~loc [ (long_name, name) ] Open in
                   let over =
                     A.pexp_record ~loc
                       [ (long_name, A.eapply ~loc [%expr f'] [ expr ]) ]
                       (Some [%expr rest'])
                   in
                   [%stri
                     let [%p name] =
                       { Lens.get = (fun [%p pat] -> [%e expr]);
                         Lens.over = (fun f' ([%p pat] as rest') -> [%e over])
                       }])
                 fields
             in
             let modu =
               A.module_binding ~loc
                 ~name:{ txt = name; loc = td.ptype_name.loc }
                 ~expr:(A.pmod_structure ~loc fields)
             in
             Some (A.pstr_module ~loc modu)
         | Ptype_variant _ | Ptype_abstract | Ptype_open -> None)

let register () =
  let generator = Deriving.Generator.make_noarg gen in
  Deriving.add "illuaminateDeriving_lens" ~str_type_decl:generator |> ignore
