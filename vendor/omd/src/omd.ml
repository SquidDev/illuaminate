module Pre = Block.Pre
include Ast

type 'ref doc = (attributes, 'ref) block list

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let parse_inlines (md, defs) =
  let defs =
    let f (def : attributes link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Mapper.map (parse_inline defs)) md

let of_channel ic = parse_inlines (Pre.of_channel ic)

let of_string s = parse_inlines (Pre.of_string s)

let inline_to_html ~ref:f inl =
  let open Html in
  let ref r = function
    | `Raw _ as l -> Raw (f r l)
    | `Desc inl -> Raw (f r (`Desc (to_string inl)))
  in
  to_string (inline ~ref inl)

let to_html ?highlight ~ref:f doc =
  let open Html in
  let highlight =
    Option.map (fun f attrs lang code -> Raw (f attrs lang code)) highlight
  in
  let ref r = function
    | `Raw _ as l -> Raw (f r l)
    | `Desc inl -> Raw (f r (`Desc (to_string inl)))
  in
  to_string (of_doc ?highlight ~ref doc)

let to_sexp ~ref ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ~ref ast)

let to_plain_text = Html.to_plain_text

let headers = Toc.headers

let toc = Toc.toc
