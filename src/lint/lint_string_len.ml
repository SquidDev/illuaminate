open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve
module G = Global

let string_len = G.parse "string.len"
let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "stdlib:string-len"

let check ~(context : context) ~r ?fix f =
  let resolve = IlluaminateData.need context.data R.key context.file |> Option.get in
  match G.of_expr resolve f with
  | Some g when g = string_len -> r.r ?fix ~tag "Prefer `#` over string.len"
  | _ -> ()

let fix =
  let mk fn arg =
    Ok
      (UnOp
         { unop_op =
             { Node.contents = OpLen;
               leading_trivia = Node.leading_trivia.get (First.expr.get fn);
               trailing_trivia = Illuaminate.IArray.empty;
               span = Spanned.expr fn
             };
           unop_rhs = arg
         })
  in
  Fixer.fix @@ function
  | ECall (Call { fn; args = CallString s; _ }) -> mk fn (String s)
  | ECall (Call { fn; args = CallArgs { open_a; args = Some (Mono a); close_a }; _ }) ->
      let a =
        if Precedence.compare (Precedence.of_expr a) (Op (UnOp.precedence OpLen)) <= 0 then
          Illuaminate.Lens.(Last.expr -| Node.trailing_trivia).over
            (fun x -> Node.join_trivia x (Node.trailing_trivia.get close_a))
            a
        else Parens { paren_open = open_a; paren_expr = a; paren_close = close_a }
      in
      mk fn a
  | _ -> Error "Not a single application"

let expr () context r = function
  | ECall (Call { fn; args = CallString _ }) -> check ~r ~context ~fix fn
  | ECall (Call { fn; args = CallArgs { args = Some (Mono _); _ } }) -> check ~r ~context ~fix fn
  | ECall (Call { fn; _ }) -> check ~r ~context fn
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
