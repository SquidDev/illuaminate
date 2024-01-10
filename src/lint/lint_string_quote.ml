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
  let requote quote str =
    if not (String.contains str quote) then (
      (* If we don't contain the quote already, we're safe to just change the quotes with no further
         changes. *)
      let len = String.length str in
      let b = Buffer.create len in
      Buffer.add_char b quote;
      Buffer.add_substring b str 1 (len - 2);
      Buffer.add_char b quote;
      Buffer.contents b)
    else
      (* Otherwise, parse the string into its components. *)
      let module S = Illuaminate.Syntax.Literal.String in
      match S.parse str with
      | Error () -> str
      | Ok (Long_string _) -> failwith "Impossible"
      | Ok (Short_string components) ->
          let buf = Buffer.create (String.length str) in
          Buffer.add_char buf quote;
          List.iter
            (fun { S.contents; start; length } ->
              match contents with
              | Quote c when c = quote -> Buffer.add_char buf '\\'; Buffer.add_char buf quote
              | _ -> Buffer.add_substring buf str start length)
            components;

          Buffer.add_char buf quote;
          Buffer.contents buf
  in
  let requote quote = Illuaminate.Lens.(Node.contents %= requote (Quote.prefix quote)) in
  let fix quote = function
    | String lit -> Ok (String (requote quote lit))
    | _ -> Error "Expected a string"
  in
  let fix_single = Fixer.fix @@ fix Single and fix_double = Fixer.fix @@ fix Double in
  function
  | Quote.Single -> fix_single
  | Double -> fix_double

let expr quote _ r = function
  | String node -> (
    match (Node.contents.get node).[0] with
    | ('\'' | '"') as prefix when prefix <> Quote.prefix quote ->
        r.r ~fix:(fix quote) ~tag "String should use %s quotes (%C)" (Quote.show quote)
          (Quote.prefix quote)
    | _ -> ())
  | _ -> ()

let linter = make ~options:Quote.options ~tags:[ tag ] ~expr ()
