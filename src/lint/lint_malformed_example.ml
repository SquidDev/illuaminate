open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:malformed-example"

let check ~r ~span contents =
  let file = Span.Filename.mk "=input" in
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
      r.r ~span ~detail ~tag "Cannot parse example"

let check_abstract ~r ~span =
  object
    inherit abstract_iter as super

    method! description { description; description_pos } =
      let span = Option.value ~default:span description_pos in
      let f _ lang code =
        match lang with
        | "lua" -> check ~r ~span code
        | _ -> ()
      in
      Doc.AbstractSyntax.Omd'.iter_code_blocks f description

    method! example =
      function
      | RawExample { span; value } -> check ~r ~span value
      | RichExample _ as e -> super#example e
  end

let linter =
  make_no_opt ~tags:[ tag ]
    ~file:(fun () context r _ ->
      match IlluaminateData.need context.data E.file context.file |> Option.get |> E.get_page with
      | None -> ()
      | Some m ->
          let iter = iter_of (check_abstract ~r) in
          iter#documented iter#page m)
    ()
