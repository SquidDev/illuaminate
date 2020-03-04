open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let linter =
  let tag = Error.Tag.make Error.Warning "doc:malformed-example" in

  let check ~notes ~span contents =
    let file = { Span.name = "=input"; path = "=input" } in
    let program = Lexing.from_string contents |> IlluaminateParser.program file
    and expr = lazy (Lexing.from_string contents |> IlluaminateParser.repl_exprs file) in

    match (program, expr) with
    | Ok _, _ | Error _, (lazy (Ok _)) -> ()
    | Error error1, (lazy (Error error2)) ->
        let error = if Span.compare error1.span error2.span <= 0 then error1 else error2 in
        let detail out =
          let errs = Error.make () in
          IlluaminateParser.Error.report errs error.span error.value;
          Format.fprintf out "@[<v2>@;%a@]"
            (fun out -> Error.display_of_string ~out ~with_summary:false (fun _ -> Some contents))
            errs
        in
        notes := note ~span ~detail ~tag "Cannot parse example" :: !notes
  in

  let check_abstract ~notes ~span =
    object
      inherit abstract_iter as super

      method! omd node =
        let open Omd in
        super#omd node;
        match node with
        | Code ("lua", code) | Code_block ("lua", code) -> check ~notes ~span code
        | _ -> ()

      method! example =
        function
        | RawExample contents -> check ~notes ~span contents
        | RichExample _ as e -> super#example e
    end
  in

  make_no_opt ~tags:[ tag ]
    ~program:(fun () context prog ->
      match IlluaminateData.need context.data E.key prog |> E.get_module with
      | None -> []
      | Some m ->
          let notes = ref [] in
          let iter = iter_of (check_abstract ~notes) in
          iter#documented iter#module_info m;
          !notes)
    ()
