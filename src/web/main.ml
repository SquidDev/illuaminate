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

let data () =
  let open IlluaminateData in
  let open Builder in
  let context = { Programs.Context.root = Fpath.v "/"; config = store } in
  empty
  |> Programs.Files.(create () |> builder)
  |> oracle Programs.Context.key (Fun.const context)
  |> build

(** Fix all errors within the program. *)
let rec fix_all () : unit =
  let lexbuf = input##.value |> Js.to_string |> Lexing.from_string in
  match IlluaminateParser.program { name = "input"; path = "input" } lexbuf with
  | Error _ -> ()
  | Ok parsed ->
      let program, _ = Driver.lint_and_fix_all ~store ~data:(data ()) Linters.all parsed in
      let new_contents = Format.asprintf "%a" Emit.program program in
      input##.value := Js.string new_contents;
      lint ()

(** Construct a formatter which emits HTML nodes. *)
and display_errors program = function
  | [] -> Template.no_errors
  | errs -> Template.some_errors program ~fix:(fun _ -> fix_all (); Js._true) errs

(** Lint the input program and display the relevant error messages. *)
and lint () : unit =
  let errs = Error.make () in
  let input = Js.to_string input##.value in
  let lexbuf = Lexing.from_string input in
  ( match IlluaminateParser.program { name = "input"; path = "input" } lexbuf with
  | Error err -> IlluaminateParser.Error.report errs err.span err.value
  | Ok parsed ->
      let data = data () in
      let tags _ = true in
      Linters.all
      |> List.iter (fun l ->
             Driver.lint ~store ~data ~tags l parsed |> List.iter (Driver.report_note errs)) );
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
