open Lsp
open Lsp.Protocol
open IlluaminateCore
open IlluaminateSemantics
module D = IlluaminateData
open Lsp_convert

let src = Logs.Src.create ~doc:"Request handler" __MODULE__

module Log = (val Logs.src_log src)

let on_program ~default store ~uri f =
  match Store.get_file store uri with
  | Some { program = Ok prog; _ } -> Ok (store, f prog)
  | Some { program = Error _; _ } ->
      Log.debug (fun f -> f "%a is malformed" Uri.pp uri);
      Ok (store, default)
  | None ->
      Log.err (fun f -> f "%a is not open" Uri.pp uri);
      Ok (store, default)

let dots_range = function
  | Resolve.IllegalDots -> assert false
  | BoundDots { node; _ } -> Node.span node

let make_edit ~uri ~version edits =
  Server_request.WorkspaceApplyEdit
    { label = None;
      edit =
        [ { changes = [];
            documentChanges = [ TextDocumentEdit { textDocument = { uri; version }; edits } ]
          }
        ]
    }

(** Get the initial assignment to a variable. *)
module Declarations = struct
  open Locations

  let of_resolved_var ~uri (var : Resolve.var) : Locations.t option =
    let rec get = function
      | [] -> None
      | (Some var, Resolve.Declare) :: _ -> Some (Location (location ~uri (Syntax.Spanned.var var)))
      | [ (Some v, _) ] -> (
        match var.kind with
        | Local _ ->
            (* If we're a local, use the first assignment as our alternative. *)
            Some (Location (location ~uri (Syntax.Spanned.var v)))
        | _ -> None )
      | _ :: xs -> get xs
    in
    get var.definitions

  let of_var ~store ~uri program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | v -> of_resolved_var ~uri v
    | exception Not_found -> None

  let of_dots ~store ~uri program d =
    let resolved = D.get (Store.data store) Resolve.key program in
    Resolve.get_dots d resolved
    |> CCOpt.flat_map (fun x -> x.Resolve.dot_node)
    |> Option.map (fun x -> Location (location ~uri (Node.span x)))

  let find ~store ~position ~uri program : Locations.t option =
    match Locate.locate position program with
    | Var v -> of_var ~store ~uri program v
    | Expr (Dots d) -> of_dots ~store ~uri program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        None
end

(** Get all assignments to a variable. *)
module Definitions = struct
  open Locations

  let of_resolved_var ~uri (var : Resolve.var) : Locations.t option =
    let get_one = function
      | Some var, _ -> Some (location ~uri (Syntax.Spanned.var var))
      | _ -> None
    in
    match List.filter_map get_one var.definitions with
    | [] -> None
    | [ x ] -> Some (Location x)
    | xs -> Some (Locations xs)

  let of_var ~store ~uri program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | v -> of_resolved_var ~uri v
    | exception Not_found -> None

  let find ~store ~position ~uri program : Locations.t option =
    match Locate.locate position program with
    | Var v -> of_var ~store ~uri program v
    | Expr (Dots d) -> Declarations.of_dots ~store ~uri program d
    | n ->
        Log.debug (fun f -> f "Not a variable node (%a)" Locate.pp_node_short n);
        None
end

module References = struct
  let of_var ~store ~uri program v =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_var v resolved with
    | { usages; _ } ->
        List.rev_map (fun { Resolve.node; _ } -> location ~uri (Syntax.Spanned.var node)) usages
    | exception Not_found -> []

  let of_dots ~store ~uri program d =
    let resolved = D.get (Store.data store) Resolve.key program in
    match Resolve.get_dots d resolved with
    | None -> []
    | Some { dot_usages; _ } -> List.rev_map (fun x -> location ~uri (dots_range x)) dot_usages

  let find ~store ~position ~uri program : Location.t list =
    match Locate.locate position program with
    | Var v -> of_var ~store ~uri program v
    | Expr (Dots d) -> of_dots ~store ~uri program d
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
               | Some v, _ -> Some { kind = Some Write; range = range (Syntax.Spanned.var v) }
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

let worker (type res) rpc store (_ : Initialize.ClientCapabilities.t) :
    res Client_request.t -> (Store.t * res, Jsonrpc.Response.Error.t) result = function
  | Shutdown -> Ok (store, ())
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
      on_program ~default:None store ~uri (Declarations.find ~store ~position ~uri)
  | TextDocumentDefinition { textDocument = { uri }; position } ->
      on_program ~default:None store ~uri (Definitions.find ~store ~position ~uri)
  | TextDocumentReferences { textDocument = { uri }; position; _ } ->
      on_program ~default:[] store ~uri (References.find ~store ~position ~uri)
  | TextDocumentHighlight { textDocument = { uri }; position; _ } ->
      on_program ~default:[] store ~uri (Highlights.find ~store ~position)
  | CodeAction { textDocument = { uri }; range; _ } ->
      on_program ~default:[] store ~uri (fun p -> Lint.code_actions store p range)
  | ExecuteCommand { command = "illuaminate/fix"; arguments } -> (
    match arguments with
    | Some [ uri; `Int id ] -> (
        let uri = Uri.t_of_yojson uri in

        match Store.get_file store uri with
        | Some { program = Ok prog; contents = Some contents; _ } -> (
            let fixed = Lint.fix store prog id in
            match fixed with
            | Error msg -> Error { code = ContentModified; message = msg; data = None }
            | Ok edit ->
                make_edit ~uri ~version:(Text_document.version contents) [ edit ]
                |> Ugly_hacks.send_request rpc;
                Ok (store, None) )
        | _ -> Ok (store, None) )
    | _ -> Error { code = InternalError; message = "Invalid arguments"; data = None } )
  | _ ->
      Log.err (fun f -> f "Unknown message (not implemented)");
      Error { code = InternalError; message = "Not implemented"; data = None }

let handle rpc store caps req =
  try worker rpc store caps req
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling request: %s\n%s" e bt);
    Error { code = InternalError; message = e; data = None }
