open Js_of_ocaml
open IlluaminateCore
open IlluaminateLint

let input = Dom_html.(getElementById_coerce "code-input" CoerceTo.textarea) |> Option.get
let output = Dom_html.(getElementById_coerce "error-messages" CoerceTo.element) |> Option.get

(** Default config values for linters. *)
let store =
  let open IlluaminateConfig in
  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    Schema.empty Linters.all
  |> Schema.union (Schema.singleton IlluaminateSemantics.Doc.Extract.Config.key)
  |> Schema.default

(** Append virtual HTML node to a concrete element. *)
let rec render_to (out : Dom.node Js.t) : Template.node -> unit = function
  | Nil -> ()
  | Raw _ -> failwith "Cannot render raw HTML"
  | Text txt ->
      let text = Dom_html.document##createTextNode (Js.string txt) in
      out##appendChild (text :> Dom.node Js.t) |> ignore
  | Element { tag; attributes; events; children } ->
      let elem = Dom_html.document##createElement (Js.string tag) in
      let elem_n = (elem :> Dom.node Js.t) in
      List.iter (render_to elem_n) children;
      List.iter
        (fun (k, v) ->
          let k =
            match k with
            | "class" -> "className"
            | _ -> k
          in
          Js.Unsafe.set elem (Js.string k) (Js.string v))
        attributes;
      List.iter
        (fun (name, handler) ->
          let event = Dom_html.Event.make name in
          Dom.addEventListener elem event (Dom_html.handler handler) Js._false |> ignore)
        events;
      out##appendChild elem_n |> ignore
  | Many xs -> List.iter (render_to out) xs

(** Render a virtual HTML node, building a <div>. *)
let render (doc : Dom_html.document Js.t) html : Dom.node Js.t =
  let out = (Dom_html.createDiv doc :> Dom.node Js.t) in
  render_to out html;
  (out :> Dom.node Js.t)

let data name file =
  let open IlluaminateData in
  let context = { Programs.Context.root = None; config = store } in
  let files =
    let open Programs.FileStore in
    let store = create () in
    update store name (Some (Lua file));
    store
  in
  Builder.build @@ fun b ->
  Programs.FileStore.builder files b;
  Builder.oracle Programs.Context.key (fun _ _ -> context) b

(** Fix all errors within the program. *)
let rec fix_all () : unit =
  let lexbuf = input##.value |> Js.to_string |> Lexing.from_string in
  let name = Span.Filename.mk "=input" in
  match IlluaminateParser.program name lexbuf with
  | Error _ -> ()
  | Ok parsed ->
      let program, _ =
        Driver.lint_and_fix_all ~store ~data:(data name parsed) Linters.all (Lua parsed)
      in
      let new_contents = Format.asprintf "%a" File.emit program in
      input##.value := Js.string new_contents;
      lint ()

and minify () : unit =
  let lexbuf = input##.value |> Js.to_string |> Lexing.from_string in
  match IlluaminateParser.program (Span.Filename.mk "=input") lexbuf with
  | Error _ -> ()
  | Ok parsed ->
      let new_contents =
        Format.asprintf "%t" @@ fun out ->
        let module M = IlluaminateMinify in
        M.minify parsed |> M.Emit.(with_wrapping out "%a" program)
      in
      input##.value := Js.string new_contents;
      lint ()

(** Construct a formatter which emits HTML nodes. *)
and display_errors program =
  let minify _ = minify (); Js._true in
  function
  | [] -> Template.no_errors ~minify
  | errs -> Template.some_errors program ~minify ~fix:(fun _ -> fix_all (); Js._true) errs

(** Lint the input program and display the relevant error messages. *)
and lint () : unit =
  let errs = Error.make () in
  let input = Js.to_string input##.value in
  let lexbuf = Lexing.from_string input in
  let name = Span.Filename.mk "=input" in
  (match IlluaminateParser.program name lexbuf with
  | Error err -> IlluaminateParser.Error.report errs err.span err.value
  | Ok parsed ->
      let data = data name parsed in
      let tags _ = true in
      Linters.all
      |> List.iter @@ fun l ->
         Driver.lint ~store ~data ~tags l (Lua parsed)
         |> Driver.Notes.to_seq
         |> Seq.iter (Driver.Note.report_any errs));
  let out =
    Error.errors errs
    |> List.sort Error.Error.span_compare
    |> display_errors input |> render Dom_html.document
  in
  match Js.Opt.to_option output##.lastChild with
  | None -> output##appendChild out |> ignore
  | Some last -> output##replaceChild out last |> ignore

let () =
  (* When the input changes, relint. *)
  Dom.addEventListener input Dom_html.Event.input
    (Dom_html.handler (fun _ -> lint (); Js._true))
    Js._false
  |> ignore;

  (* Lint everything to get us started. *)
  lint ()
