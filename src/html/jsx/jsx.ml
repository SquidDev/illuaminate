open Ppxlib
module Build = Ast_builder.Default

let make_opt ~loc name args rest : (arg_label * expression) list =
  match args with
  | [] -> rest
  | _ :: _ -> (Labelled name, Build.elist ~loc args) :: rest

let fixup =
  object
    inherit Ast_traverse.map as super

    method! expression exp =
      match super#expression exp with
      (* Locate nodes of the form "<exp>[@JSX]" *)
      | { pexp_attributes = [ { attr_name = { txt = "JSX"; _ }; _ } ];
          pexp_desc =
            Pexp_apply
              ( { pexp_desc = Pexp_ident { loc; txt = Lident name };
                  pexp_loc;
                  pexp_attributes;
                  pexp_loc_stack
                },
                args );
          _
        } ->
          let attributes, events, rest =
            List.fold_right
              (fun arg (attributes, events, rest) ->
                match arg with
                | (Labelled "children", _) as children -> (attributes, events, children :: rest)
                | Labelled name, value
                  when String.length name > 2
                       && name.[0] = 'o'
                       && name.[1] = 'n'
                       && name.[2] >= 'A'
                       && name.[2] <= 'Z' ->
                    (* If we've got an attribute matching on[A-Z], then we lowercase it and use it
                       as an event name. *)
                    let name =
                      String.length name - 2 |> String.sub name 2 |> String.lowercase_ascii
                    in
                    ( attributes,
                      Build.pexp_tuple ~loc:value.pexp_loc
                        [ Build.estring ~loc:value.pexp_loc name; value ]
                      :: events,
                      rest )
                | Labelled name, value ->
                    let n = String.length name in
                    (* Allow writing class_ instead of class, and some_attr rather than some-attr *)
                    let name = if name.[n - 1] = '_' then String.sub name 0 (n - 1) else name in
                    let name = String.map (fun x -> if x = '_' then '-' else x) name in
                    ( Build.pexp_tuple ~loc:value.pexp_loc
                        [ Build.estring ~loc:value.pexp_loc name; value ]
                      :: attributes,
                      events,
                      rest )
                | arg -> (attributes, events, arg :: rest))
              args ([], [], [])
          in
          let rest =
            rest |> make_opt ~loc "events" events |> make_opt ~loc "attributes" attributes
          in
          { pexp_attributes = [];
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_desc =
              Pexp_apply
                ( { pexp_desc = Pexp_ident { loc; txt = Lident "create_node" };
                    pexp_loc;
                    pexp_loc_stack;
                    pexp_attributes
                  },
                  (Labelled "tag", Build.estring ~loc name) :: rest )
          }
      | exp -> exp
  end

let () = Driver.register_transformation ~impl:fixup#structure "illuaminate.jsx"
