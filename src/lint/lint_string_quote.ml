open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

module Quote = struct
  type t =
    | Single
    | Double

  let prefix = function
    | Single -> '\''
    | Double -> '"'

  let show = function
    | Single -> "single"
    | Double -> "double"

  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      field ~name:"quote" ~comment:"The preferred quote mark (' or \")." ~default:Double
        (Converter.enum ~ty:"quote"
           [ ("single", Single); ("\'", Single); ("double", Double); ("\"", Double) ])
    in
    Category.add term category
end

let tag = Error.Tag.make ~attr:[] ~level:Note "format:string-quote"

let fix =
  let requote quote node =
    let open IlluaminateSemantics.Stringlib.Literal in
    let prefix = Quote.prefix quote in
    let out = Node.contents.get node |> String.length |> Buffer.create in
    Buffer.add_char out prefix;
    (parse node
    |> Option.iter @@ List.iter
       @@ function
       | Segment x | Escape (x, _) -> Buffer.add_string out x
       | Malformed (c, _) | Quote c -> Buffer.add_char out '\\'; Buffer.add_char out c);
    Buffer.add_char out prefix;
    Lens.(Node.contents ^= Buffer.contents out) @@ node
  in
  let fix quote = function
    | String lit -> Ok (String (Lens.(Literal.lit_node %= requote quote) @@ lit))
    | _ -> Error "Expected a string"
  in
  let fix_single = Fixer.fix @@ fix Single and fix_double = Fixer.fix @@ fix Double in
  function
  | Quote.Single -> fix_single
  | Double -> fix_double

let expr quote _ r = function
  | String { lit_node; _ } -> (
    match (Node.contents.get lit_node).[0] with
    | ('\'' | '"') as prefix when prefix <> Quote.prefix quote ->
        r.r ~fix:(fix quote) ~tag "String should use %s quotes (%C)" (Quote.show quote)
          (Quote.prefix quote)
    | _ -> ())
  | _ -> ()

let linter = make ~options:Quote.options ~tags:[ tag ] ~expr ()
