open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter

open struct
  module R = Resolve
  module G = Global
  module Safe = Pure.Safe
end

let linter =
  let string_len = G.parse "pcall" in
  let tag = Error.Tag.make Error.Warning "stdlib:pcall-eta" in

  let safe_args = function
    | CallArgs { args; _ } -> SepList0.for_all Safe.expr args
    | CallTable t -> Safe.table t
    | CallString _ -> true
  in
  let check ~(context : context) ~fix = function
    | Call
        (* Look for `function return f(...) end' and `function() f(...) end'. *)
        { fn;
          args =
            CallArgs
              { args =
                  Some
                    (Mono
                      (Fun
                        { fun_args = { args_args = None; _ };
                          fun_body =
                            [ ( Return { return_vals = Some (Mono (ECall (Call call))); _ }
                              | SCall (Call call) )
                            ];
                          _
                        }));
                _
              }
        } -> (
        let resolve = IlluaminateData.need context.data R.key context.program in
        match G.of_expr resolve fn with
        | Some g when g = string_len && safe_args call.args ->
            [ note ~fix ~tag
                "Prefer passing function arguments to pcall, rather than using a closure."
            ]
        | _ -> [] )
    | _ -> []
  in
  let fix = function
    | Call
        { fn;
          args =
            CallArgs
              { args =
                  Some
                    (Mono
                      (Fun
                        { fun_args = { args_args = None; _ };
                          fun_body =
                            [ ( Return { return_vals = Some (Mono (ECall (Call call))); _ }
                              | SCall (Call call) )
                            ];
                          _
                        }));
                open_a;
                close_a
              }
        }
      when let span = Spanned.call (Call call) in
           Span.(start_line.get span = finish_line.get span) ->
        let args = Helpers.get_call_args call.args in
        let args =
          match args with
          | None -> SepList1.Mono call.fn
          | Some args ->
              let comma =
                match Last.expr.get fn with
                | Node.SimpleNode _ -> Node.SimpleNode { contents = Token.Comma }
                | Node.Node { span; _ } ->
                    Node.Node
                      { contents = Token.Comma;
                        span = Span.finish span;
                        leading_trivia = [];
                        (* TODO: Ideally we wouldn't need to do this, and the formatter would handle
                           this. *)
                        trailing_trivia = [ { span; value = Whitespace " " } ]
                      }
              in
              SepList1.Cons1 (call.fn, comma, args)
        in
        Ok (Call { fn; args = CallArgs { open_a; close_a; args = Some args } })
    | _ -> Error "Not a suitable call"
  in

  let fix_expr =
    FixOne
      (function
      | ECall call -> fix call |> Result.map (fun x -> ECall x)
      | _ -> Error "Not a function call")
  in
  let expr () context = function
    | ECall call -> check ~context ~fix:fix_expr call
    | _ -> []
  in

  let fix_stmt =
    FixOne
      (function
      | SCall call -> fix call |> Result.map (fun x -> SCall x)
      | _ -> Error "Not a function call")
  in
  let stmt () context = function
    | SCall call -> check ~context ~fix:fix_stmt call
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ~stmt ()
