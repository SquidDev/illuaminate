open IlluaminateCore
open IlluaminateSemantics
module M = Module_resolve

let grammar_class = function
  | Emit.Keyword -> "keyword"
  | LiteralKeyword -> "literal-kw"
  | OperatorKeyword -> "operator-kw"
  | Symbol -> "symbol"
  | Identifier -> "ident"
  | String -> "string"
  | Number -> "number"
  | Comment -> "comment"

let transform_ref ~options ((r : M.Reference.t), t) =
  (* TODO: We really need to rethink a) how module_resolve does references and b) how on earth we
     generate anchors for values. *)
  let refr r = Html_basic.reference_attrs ~options r `Code |> fst |> Option.map (fun x -> (x, t)) in
  match r with
  | Reference (Internal _ as r) -> refr r
  | Dot (Reference (Internal ({ name = Module; _ } as r)), n) ->
      refr (Internal { r with name = Value n })
  | _ -> None

let emit ~options ~data ~input visit tree =
  let open Html.Default in
  (* TODO: Emit a true HTML node. Not sure how to do that elegantly though - we'd probably need to
     use a visitor within Emit instead. *)
  let res = Buffer.create (String.length input) in
  let out = Format.formatter_of_buffer res in

  let stack = ref [] in
  let push fn =
    let xs = !stack in
    if List.exists Fun.id xs then (
      stack := false :: xs;
      "" )
    else
      match fn () |> CCOpt.flat_map (transform_ref ~options) with
      | Some (url, (node : 'a Doc.Syntax.documented)) ->
          stack := true :: xs;
          let attrs = [ ("href", url) ] in
          let attrs =
            match node.description with
            | None -> attrs
            | Some { description; _ } ->
                let desc =
                  Helpers.get_summary description |> Omd.to_text
                  |> CCString.replace ~sub:"\n" ~by:" "
                  |> String.trim
                in
                ("title", desc) :: attrs
          in
          Format.asprintf "<a%a>" Html.Emitters.attrs attrs
      | None ->
          stack := false :: xs;
          ""
  and pop () =
    match !stack with
    | [] -> assert false
    | x :: xs ->
        stack := xs;
        if x then "</a>" else ""
  in

  Format.pp_set_mark_tags out true;
  Format.pp_set_margin out 120;
  Format.pp_set_formatter_stag_functions out
    { mark_open_stag =
        (function
        | Emit.Token t -> Printf.sprintf "<span class=\"%s\">" (grammar_class t)
        | Emit.Var v -> push @@ fun () -> M.get_var data v
        | Emit.Name n -> push @@ fun () -> M.get_name data n
        | _ -> "");
      mark_close_stag =
        (function
        | Emit.Token _ -> "</span>"
        | Emit.Var _ | Emit.Name _ -> pop ()
        | _ -> "");
      print_open_stag = (fun _ -> ());
      print_close_stag = (fun _ -> ())
    };
  visit out tree;
  Format.pp_print_flush out ();
  raw (Buffer.contents res)

let do_lua ~options:({ Html_options.data; _ } as options) input =
  let file = Span.Filename.mk "=input" in
  let expr = Lexing.from_string input |> IlluaminateParser.repl_exprs file in
  let program = lazy (Lexing.from_string input |> IlluaminateParser.program file) in

  match (expr, program) with
  | Ok tree, _ ->
      let data =
        IlluaminateData.get data M.key
        @@ { program =
               [ Return
                   { return_return = Lens.(tree.repl_eof |> Node.contents ^= Token.Return);
                     return_vals = Some tree.repl_exprs
                   }
               ];
             eof = tree.repl_eof
           }
      in
      (emit ~options ~data ~input Emit.repl_exprs tree, Some `Expr)
  | Error _, (lazy (Ok tree)) ->
      let data = IlluaminateData.get data M.key tree in
      (emit ~options ~data ~input Emit.program tree, Some `Stmt)
  | Error _, (lazy (Error _)) -> (Html.Default.str input, None)

let lua ~options input = do_lua ~options input |> fst

let lua_block ~options input =
  let highlighted, kind = do_lua ~options input in
  let kind =
    match kind with
    | None -> None
    | Some `Expr -> Some "expr"
    | Some `Stmt -> Some "stmt"
  in
  Html.Default.create_node ~tag:"pre"
    ~attributes:[ ("class", Some "highlight highlight-lua"); ("data-lua-kind", kind) ]
    ~children:[ highlighted ] ()
