open IlluaminateCore
open Lsp
module Data = IlluaminateData
module Table = Hashtbl.Make (Uri)
module UriSet = CCHashSet.Make (Uri)

module Filename = struct
  let of_uri uri =
    let name = Uri.to_string uri in
    { Span.name; path = name }

  let to_uri_json { Span.path; _ } = `String path

  let to_uri { Span.path; _ } = Uri.t_of_yojson (`String path)
end

let linter_schema =
  let open IlluaminateLint in
  let open IlluaminateConfig in
  let add k s = Schema.union (Schema.singleton k) s in

  List.fold_left
    (fun s (Linter.Linter l) -> Schema.union s (Schema.singleton l.options))
    Schema.empty Linters.all
  |> add IlluaminateSemantics.Doc.Extract.Config.key

type document =
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : Lsp.Text_document.t option;
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
    mutable file : Data.Programs.Files.id option;
    context : Data.Programs.Context.t
  }

type t =
  { directories : UriSet.t;
    files : document Table.t;
    data : Data.t;
    file_store : Data.Programs.Files.t
  }

let data { data; _ } = data

let create () =
  let files = Table.create 64 in
  let file_store = Data.Programs.Files.create () in
  let data =
    let open Data.Builder in
    empty
    |> Data.Programs.Files.builder file_store
    |> oracle Data.Programs.Context.key (fun name ->
           match Filename.to_uri name |> Table.find_opt files with
           | Some { context; _ } -> context
           | None -> failwith "Unknown file!")
    |> build
  in
  { directories = UriSet.create 4; files; file_store; data }

let lex_file name doc =
  Text_document.text doc |> Lexing.from_string |> IlluaminateParser.program name

let get_file { files; _ } = Table.find_opt files

(** Update the {!Data.Programs.Files.t} store. *)
let sync_file store = function
  | { program = Error _; _ } -> ()
  | { program = Ok p; file = None; _ } as file ->
      let id = Data.Programs.Files.add p store.file_store in
      file.file <- Some id
  | { program = Ok p; file = Some id; _ } -> Data.Programs.Files.update id p

let update_file store (doc : document) contents =
  doc.contents <- Some contents;
  doc.program <- lex_file doc.name contents;
  sync_file store doc

let open_file store contents =
  let uri = Text_document.documentUri contents in
  match get_file store uri with
  | Some doc -> update_file store doc contents; doc
  | None ->
      let name = Filename.of_uri uri in
      let program = lex_file name contents in
      let context =
        { Data.Programs.Context.root = Fpath.v "/";
          config = IlluaminateConfig.Schema.(default linter_schema)
        }
      in
      let doc = { name; uri; contents = Some contents; program; file = None; context } in
      Table.add store.files uri doc; sync_file store doc; doc
