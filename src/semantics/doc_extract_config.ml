open IlluaminateCore
open IlluaminateConfig
module D = IlluaminateData

type custom_kind =
  { id : string;
    display : string
  }

type t =
  { module_path : IlluaminatePattern.t list;
    module_kinds : custom_kind list
  }

let workspace = Category.create ~name:"doc" ~comment:"Controls documentation generation." ()

let key =
  let term =
    let open Term in
    let parse_path p = CCString.drop_while (fun x -> x = '/') p |> Result.ok and print_path x = x in
    let+ module_path =
      field ~name:"library-path"
        ~comment:
          "The path(s) where modules are located. This is used for guessing the module name of \
           files, it is ignored when an explicit @module annotation is provided."
        ~default:[]
        Converter.(list (atom ~ty:"path" parse_path print_path))
    and+ module_kinds =
      field ~name:"module-kinds" ~comment:"A list of custom module kinds and their display names."
        ~default:[]
        Converter.(list (pair string string))
    in
    { module_path = List.map IlluaminatePattern.parse module_path;
      module_kinds = List.map (fun (id, display) -> { id; display }) module_kinds
    }
  in
  Category.add term workspace

let guess_module path : D.Programs.Context.t -> string option = function
  | { root = None; _ } -> None
  | { root = Some root; config } ->
      let { module_path; module_kinds = _ } = Schema.get key config in
      if not (Fpath.is_rooted ~root path) then None
      else
        let path = Fpath.relativize ~root path |> Option.get in
        let shortest_module best pat =
          match IlluaminatePattern.match_end path pat with
          | None -> best
          | Some (_, name) -> (
              let name = Fpath.rem_ext name |> Fpath.segs |> String.concat "." in
              match best with
              | Some b when String.length b < String.length name -> best
              | _ -> Some name )
        in
        List.fold_left shortest_module None module_path

let guess_module' filename context =
  match filename.Span.Filename.path with
  | None -> None
  | Some path ->
      let config = D.need context D.Programs.Context.key filename in
      guess_module path config
