open Ppxlib
module A = Ast_builder.Default

let modu ~loc ~td contents =
  let name =
    String.mapi (fun i x -> if i = 0 then Char.uppercase_ascii x else x) td.ptype_name.txt
  in
  A.module_binding ~loc
    ~name:{ txt = Some name; loc = td.ptype_name.loc }
    ~expr:(A.pmod_structure ~loc contents)
  |> A.pstr_module ~loc |> Option.some

let gen ~loc ~path:_ ((_, tds) : rec_flag * type_declaration list) : structure =
  tds
  |> List.filter_map (fun td ->
         match td.ptype_kind with
         | Ptype_record fields ->
             let field { pld_name; pld_loc = loc; _ } =
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
                   }]
             in
             List.map field fields |> modu ~td ~loc
         | Ptype_variant variants ->
             let many = List.length variants > 1 in
             let variant = function
               | { pcd_name; pcd_args = Pcstr_tuple [ _ ]; pcd_loc = loc; _ } as ctor ->
                   let name = { txt = "_" ^ pcd_name.txt; loc } in
                   let pat = A.pconstruct ctor (Some [%pat? x]) in
                   let mk = A.econstruct ctor (Some [%expr f x]) in
                   let get =
                     if many then
                       [%expr
                         function
                         | [%p pat] -> x
                         | _ -> failwith "Incorrect type"]
                     else [%expr fun [%p pat] -> x]
                   and over =
                     if many then
                       [%expr
                         fun f -> function
                          | [%p pat] -> [%e mk]
                          | x -> x]
                     else [%expr fun f [%p pat] -> [%e mk]]
                   in
                   Some
                     [%stri
                       let [%p A.ppat_var ~loc name] = { Lens.get = [%e get]; over = [%e over] }]
               | _ -> None
             in
             List.filter_map variant variants |> modu ~td ~loc
         | Ptype_abstract | Ptype_open -> None)

let register () =
  let generator = Deriving.Generator.make_noarg gen in
  Deriving.add "illuaminateDeriving_lens" ~str_type_decl:generator |> ignore
