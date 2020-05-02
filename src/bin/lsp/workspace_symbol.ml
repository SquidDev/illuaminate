open Lsp.Types
module Doc = IlluaminateSemantics.Doc
module Data = IlluaminateData
module StringMap = Map.Make (String)
open Doc.Syntax

module Trie = struct
  module Map = Map.Make (Char)

  type 'a t =
    { here : 'a list;
      there : 'a t Map.t
    }

  let empty = { here = []; there = Map.empty }

  let add k v =
    let len = String.length k in
    let rec add i { here; there } =
      if i >= len then { here = v :: here; there }
      else
        let there =
          Map.update k.[i]
            (fun x -> Option.value ~default:empty x |> add (i + 1) |> Option.some)
            there
        in
        { here; there }
    in

    add 0

  let find k =
    let len = String.length k in
    let rec find i ({ here = _; there } as self) =
      if i >= len then self
      else
        match Map.find_opt k.[i] there with
        | None -> empty
        | Some v -> find (i + 1) v
    in
    find 0

  let rec iter x { here; there } =
    List.iter x here;
    Map.iter (fun _ -> iter x) there
end

let add_top ~name ?container ~kind d =
  Trie.add name
    (SymbolInformation.create ~name ~kind ?containerName:container
       ~location:(Lsp_convert.location d.definition)
       ?deprecated:(Option.map (fun _ -> true) d.deprecated)
       ())

let add ~container = add_top ~container

let dot = Printf.sprintf "%s.%s"

let rec dump_term ~name ~container ({ descriptor; _ } as self) trie =
  let kind : SymbolKind.t =
    match descriptor with
    | Table _ -> Object
    | Expr { ty = Named (_, "string"); _ } -> String
    | Expr { ty = Named (_, "number"); _ } -> Number
    | Expr { ty = Named (_, "boolean"); _ } -> Boolean
    | Expr _ -> Constant
    | Function _ -> Function
    | Type _ -> Class
    | Unknown | Undefined -> Variable
  in
  trie |> add ~name ~container ~kind self
  |> dump_term_contents ~container:(dot container name) descriptor

and dump_term_contents ~container x trie =
  match x with
  | Table xs -> List.fold_left (fun trie (k, v) -> dump_term ~name:k ~container v trie) trie xs
  | _ -> trie

let dump_type ~container ({ descriptor = { type_members; type_name; _ }; _ } as self) trie =
  let name = dot container type_name in
  List.fold_left
    (fun trie m ->
      match m.member_value.descriptor with
      | Function _ -> add ~name:m.member_name ~container:name ~kind:Method m.member_value trie
      | Unknown | Undefined -> add ~name ~container:name ~kind:Field m.member_value trie
      | _ -> dump_term ~name:m.member_name ~container:name m.member_value trie)
    trie type_members
  |> add ~name:type_name ~container ~kind:Class self

let dump_module ({ descriptor = { mod_types; mod_contents; mod_name = name; _ }; _ } as self) trie =
  List.fold_left (fun trie ty -> dump_type ~container:name ty trie) trie mod_types
  |> dump_term_contents ~container:name mod_contents
  |> add_top ~name ~kind:Module self

type t = SymbolInformation.t Trie.t

let key =
  Data.Key.key ~name:__MODULE__ @@ fun data () ->
  let modules = Data.need data Doc.Extract.get_modules () in
  StringMap.fold (fun _ -> dump_module) modules Trie.empty

let find_modules query trie =
  let xs = ref [] in
  Trie.find query trie |> Trie.iter (fun x -> xs := x :: !xs);
  let rec take i ys = function
    | [] -> ys
    | x :: xs -> if i < 0 then ys else take (i - 1) (x :: ys) xs
  in
  take 100 [] !xs
