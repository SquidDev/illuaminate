open IlluaminateSemantics
open IlluaminateCore
open Lsp
module Table = Hashtbl.Make (Uri)
module UriSet = CCHashSet.Make (Uri)

module Filename = struct
  type t = Span.filename

  let of_uri uri =
    let name = Uri.to_string uri in
    { Span.name; path = name }

  let to_uri { Span.path; _ } = Uri.t_of_yojson (`String path)

  let of_document d = Text_document.documentUri d |> of_uri
end

type document =
  { name : Span.filename;
    uri : Lsp.Uri.t;
    mutable contents : Lsp.Text_document.t option;
        (** The file's contents, if it is open in an editor. *)
    mutable program : (Syntax.program, IlluaminateParser.Error.t Span.spanned) result;
        (** The result of parsing the file, or nil if not available. *)
    mutable file : Data.Files.id option;  (** The current file's ID *)
    context : Data.context
  }

type t =
  { directories : UriSet.t;  (** The set of open directories. *)
    files : document Table.t;
        (** A collection of files. This includes those which are open, and just in the current
            directory. *)
    mutable file_store : Data.Files.t  (** The main cache of file information. *)
  }

let create () =
  let files = Table.create 64 in
  { directories = UriSet.create 4;
    files;
    file_store =
      Data.Files.create (fun name ->
          match Filename.to_uri name |> Table.find_opt files with
          | Some { context; _ } -> context
          | None -> failwith "TODO: How to handle a missing file?")
  }

let lex_file name doc =
  Text_document.text doc |> Lexing.from_string |> IlluaminateParser.program name

let create_file store contents =
  let uri = Text_document.documentUri contents in
  let name = Filename.of_uri uri in
  let program = lex_file name contents in
  let context = failwith "TODO" in
  let doc = { name; uri; contents = Some contents; program; file = None; context } in
  Table.replace store.files uri doc; doc

let update_file store = function
  | { program = Error _; _ } -> ()
  | { program = Ok p; file = None; _ } as file ->
      let file_store, id = Data.Files.add p store.file_store in
      store.file_store <- file_store;
      file.file <- Some id
  | { program = Ok p; file = Some id; _ } ->
      let file_store = Data.Files.update id p store.file_store in
      store.file_store <- file_store
