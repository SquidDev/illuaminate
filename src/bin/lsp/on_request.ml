open Lsp
open Lsp.Types
open IlluaminateCore
open IlluaminateSemantics
module D = IlluaminateData
open Lsp_convert
open CCFun

let src = Logs.Src.create ~doc:"Request handler" __MODULE__

module Log = (val Logs.src_log src)

let non_empty = function
  | [] -> None
  | _ :: _ as xs -> Some xs

let on_program ~default store ~uri f =
  match Store.get_file store uri with
  | Some { program = Ok prog; _ } -> Ok (f prog)
  | Some { program = Error _; _ } ->
      Log.debug (fun f -> f "%a is malformed" Uri.pp uri);
      Ok default
  | None ->
      Log.err (fun f -> f "%a is not open" Uri.pp uri);
      Ok default

let on_program' ~default store ~uri f = on_program ~default store ~uri:(Store.Filename.box uri) f

let dots_range = function
  | Resolve.IllegalDots -> assert false
  | BoundDots { node; _ } -> Node.span node

let make_edit ~uri ?version edits =
  match edits with
  | [] -> WorkspaceEdit.create ()
  | _ :: _ ->
      WorkspaceEdit.create
        ~documentChanges:[ `TextDocumentEdit { textDocument = { uri; version }; edits } ]
        ()

let apply_edit ~uri ?version edits =
  Server_request.WorkspaceApplyEdit
    (ApplyWorkspaceEditParams.create ~edit:(make_edit ~uri ?version edits) ())

(** Get documentation of a node. *)
module Hover = struct
  open Doc.Syntax

  let of_name ~store program name =
    let modules = D.get (Store.data store) Module_resolve.key program in
    match Module_resolve.get_name modules name with
    | Some (r, ({ descriptor = Unknown | Undefined; _ } as d))
      when not (Module_resolve.is_interesting r d) ->
        (* Skip nodes which have no interesting information. *)
        None
    | Some (refr, { description; descriptor; _ }) ->
        (* For now, we just print the summary (for instance, foo(a, b, c)) and description. *)
        let title =
          Format.asprintf "```\n%a%s\n```" Module_resolve.Reference.pp refr (get_suffix descriptor)
        in
        let value =
          match description with
          | None -> title
          | Some (d : description) -> Printf.sprintf "%s\n%s" title (Omd.to_markdown d.description)
        in
        Hover.create
          ~range:(range (Syntax.Spanned.name name))
          ~contents:(`MarkupContent (MarkupContent.create ~kind:Markdown ~value))
          ()
        |> Option.some
    | None -> None

  let find ~store ~position program : Hover.t option =
    match Locate.locate position program with
    | Var v -> of_name ~store program (NVar v)
    | Name n -> of_name ~store program n
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        None
end

(** Get the initial assignment to a variable. *)
module Declarations = struct
  let of_var ~store program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | v -> Resolve.Kind.definition v.kind |> Option.map (fun s -> `Location [ location s ])
    | exception Not_found -> None

  let of_name ~store program name : Locations.t option =
    let modules = D.get (Store.data store) Module_resolve.key program in
    match Module_resolve.get_name modules name with
    | Some (_, { definition; _ }) -> Some (`Location [ location definition ])
    | None -> None

  let of_dots ~store program d =
    let resolved = D.get (Store.data store) Resolve.key program in
    Resolve.get_dots d resolved
    |> CCOpt.flat_map (fun x -> x.Resolve.dot_node)
    |> Option.map (fun x -> `Location [ location (Node.span x) ])

  let find ~store ~position program : Locations.t option =
    match Locate.locate position program with
    | Var v -> of_var ~store program v
    | Name n -> of_name ~store program n
    | Expr (Dots d) -> of_dots ~store program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        None
end

(** Get all assignments to a variable. *)
module Definitions = struct
  let of_resolved_var (var : Resolve.var) : Locations.t option =
    let get_one = function
      | Some v, _ -> Some (location (Syntax.Spanned.var v.Resolve.node))
      | _ -> None
    in
    match List.filter_map get_one var.definitions with
    | [] -> None
    | [ x ] -> Some (`Location [ x ])
    | xs -> Some (`Location xs)

  let of_var ~store program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | v -> of_resolved_var v
    | exception Not_found -> None

  let find ~store ~position program : Locations.t option =
    match Locate.locate position program with
    | Var v -> of_var ~store program v
    | Name n -> Declarations.of_name ~store program n
    | Expr (Dots d) -> Declarations.of_dots ~store program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        None
end

module References = struct
  let of_var ~store program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | { usages; _ } ->
        List.rev_map (fun { Resolve.node; _ } -> location (Syntax.Spanned.var node)) usages
    | exception Not_found -> []

  let of_dots ~store program d =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_dots d resolved with
    | None -> []
    | Some { dot_usages; _ } -> List.rev_map (fun x -> location (dots_range x)) dot_usages

  let find ~store ~position program : Location.t list =
    match Locate.locate position program with
    | Var v -> of_var ~store program v
    | Expr (Dots d) -> of_dots ~store program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        []
end

module Highlights = struct
  open DocumentHighlight

  let build usages defs =
    Seq.fold_left (fun xs x -> x :: xs) (Seq.fold_left (fun xs x -> x :: xs) [] usages) defs

  let of_var ~store program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | { usages; definitions; _ } ->
        let usages =
          List.to_seq usages
          |> Seq.map (fun { Resolve.node; _ } ->
                 { kind = Some Read; range = range (Syntax.Spanned.var node) })
        and definitions =
          List.to_seq definitions
          |> Seq.filter_map (function
               | Some v, _ ->
                   Some { kind = Some Write; range = range (Syntax.Spanned.var v.Resolve.node) }
               | None, _ -> None)
        in
        build usages definitions
    | exception Not_found -> []

  let of_dots ~store program d =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_dots d resolved with
    | None -> []
    | Some { dot_node; dot_usages; _ } -> (
        let res =
          List.rev_map (fun x -> { kind = Some Read; range = range (dots_range x) }) dot_usages
        in
        match dot_node with
        | None -> res
        | Some node -> { kind = Some Write; range = range (Node.span node) } :: res )

  let find ~store ~position program : DocumentHighlight.t list =
    match Locate.locate position program with
    | Var v -> of_var ~store program v
    | Expr (Dots d) -> of_dots ~store program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        []
end

let worker (type res) (client : Store.client_channel) store (_ : ClientCapabilities.t) :
    res Client_request.t -> (res, Jsonrpc.Response.Error.t) result = function
  | Shutdown -> Ok ()
  | TextDocumentHover { textDocument = { uri }; position } ->
      on_program' ~default:None store ~uri (Hover.find ~store ~position)
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
      on_program' ~default:None store ~uri (Declarations.find ~store ~position)
  | TextDocumentDefinition { textDocument = { uri }; position } ->
      on_program' ~default:None store ~uri (Definitions.find ~store ~position)
  | TextDocumentReferences { textDocument = { uri }; position; _ } ->
      on_program' ~default:None store ~uri (non_empty % References.find ~store ~position)
  | TextDocumentHighlight { textDocument = { uri }; position; _ } ->
      on_program' ~default:None store ~uri (non_empty % Highlights.find ~store ~position)
  | TextDocumentPrepareRename { textDocument = { uri }; position } ->
      on_program' ~default:None store ~uri (Rename.check (Store.data store) position)
  | TextDocumentRename { textDocument = { uri }; position; newName } ->
      let make_edit = function
        | Ok edits -> make_edit ~uri edits
        | Error m ->
            Log.err (fun f -> f "Cannot rename: %s" m);
            client.notify (ShowMessage { type_ = Error; message = m });
            make_edit ~uri []
      in
      on_program' ~default:(WorkspaceEdit.create ()) store ~uri
        (make_edit % Rename.rename (Store.data store) position newName)
  | CodeAction { textDocument = { uri }; range; _ } ->
      on_program' ~default:None store ~uri (fun p -> Lint.code_actions store p range)
  | WorkspaceSymbol { query } ->
      D.get (Store.data store) Workspace_symbol.key ()
      |> Workspace_symbol.find_modules query
      |> Option.some |> Result.ok
  | ExecuteCommand { command = "illuaminate/fix"; arguments } -> (
    match arguments with
    | Some [ uri; `Int id ] -> (
        let uri = Uri.t_of_yojson uri in
        let duri = Uri.to_string uri in

        match Store.get_file store uri with
        | Some { program = Ok prog; contents; _ } -> (
            let fixed = Lint.fix store prog id in
            match fixed with
            | Error msg -> Error { code = ContentModified; message = msg; data = None }
            | Ok edit ->
                apply_edit ~uri:duri ~version:(Text_document.version contents) [ edit ]
                |> client.request;
                Ok `Null )
        | _ -> Ok `Null )
    | _ -> Error { code = InternalError; message = "Invalid arguments"; data = None } )
  | UnknownRequest ("$/illuaminate/dump", _) ->
      Format.asprintf "%a" (D.pp_store ~all:false) (Store.data store) |> Printf.printf "%s";
      Ok ()
  | _ ->
      Log.err (fun f -> f "Unknown message (not implemented)");
      Error { code = InternalError; message = "Not implemented"; data = None }

let handle client store caps req =
  try worker client store caps req
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling request: %s\n%s" e bt);
    Error { Jsonrpc.Response.Error.code = InternalError; message = e; data = None }
