open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve
module G = Global
module F = Stringlib.Format

let tag_format = Error.Tag.make ~attr:[ Default ] ~level:Warning "stdlib:string-format"

let tag_pattern = Error.Tag.make ~attr:[ Default ] ~level:Warning "stdlib:string-pattern"

let plural fmt (n, word) =
  if n = 1 then Format.fprintf fmt "1 %s" word else Format.fprintf fmt "%d %ss" n word

let check_format ~r ~span : F.specifier -> unit = function
  | Eof -> r.r ~span ~tag:tag_format "Expected specifier after '%%'"
  | Unknown c -> r.r ~span ~tag:tag_format "Unknown specifier '%%%c'" c
  | Known ("", _) -> ()
  | Known (_, '%') -> r.r ~span ~tag:tag_format "Redundant flags to format string '%%'"
  | _ -> ()

let check_format_args ~r ~span specs args =
  let rec count n : F.t -> int option = function
    | [] -> Some n
    | (Raw _ | Known (_, '%')) :: specs -> count n specs
    | Known (_, _) :: specs -> count (n + 1) specs
    | (Eof | Unknown _) :: _ -> None
  in

  let rec go i (specs : F.t) (args : expr SepList0.t) =
    match (specs, args) with
    | [], None -> ()
    | [], Some args ->
        r.r
          ~span:(Spanned.(list1 expr) args)
          ~tag:tag_format "Format string takes %a, but given an extra %a." plural (i, "parameter")
          plural
          (SepList1.length args, "argument")
    | (Raw _ | Known (_, '%')) :: specs, args -> (* Skip %% *) go i specs args
    (* Abort on malformed strings. Otherwise we'll give lots of spurious warnings. *)
    | (Eof | Unknown _) :: _, _ -> ()
    | Known _ :: _, None -> (
      match count 0 specs with
      | None ->
          (* Any error message we give will just end up being more confusing. So just skip it. We'll
             be warning on the malformed string anyway. *)
          ()
      | Some arity ->
          r.r ~span ~tag:tag_format "Format string takes %a, but only given %a." plural
            (i + arity, "parameter") plural (i, "argument"))
    (* If this expression has multiple values, there's very little we can infer from it. *)
    | _ :: _, Some (Mono arg) when Helpers.has_var_return arg -> ()
    | Known _ :: specs, Some (Mono _) -> go (i + 1) specs None
    | Known _ :: specs, Some (Cons1 (_, _, args)) -> go (i + 1) specs (Some args)
  in
  go 0 specs args

let string_format = G.parse "string.format"

let check_format ~(r : 'a reporter) call fmt args =
  match Eval.eval fmt with
  | RString value ->
      let specs = F.parse value in
      check_format_args ~r ~span:(Spanned.call call) specs args;
      List.iter (check_format ~r ~span:(Spanned.expr fmt)) specs
  | _ -> ()

let check ~(context : context) ~r = function
  | Call { fn; args } as c -> (
      let resolve = IlluaminateData.need context.data R.key context.program in
      match G.of_expr resolve fn with
      | Some g when g = string_format -> (
        match Helpers.get_call_args args with
        | None -> ()
        | Some (Cons1 (fmt, _, args)) -> check_format ~r c fmt (Some args)
        | Some (Mono fmt) -> check_format ~r c fmt None)
      | _ -> ())
  | Invoke { obj; colon = _; meth; args } as c -> (
    match Node.contents.get meth with
    | "format" -> check_format ~r c obj (Helpers.get_call_args args)
    | _ -> ())

let expr () context r = function
  | ECall call -> check ~context ~r call
  | _ -> ()

let stmt () context r = function
  | SCall call -> check ~context ~r call
  | _ -> ()

let linter = make_no_opt ~tags:[ tag_format; tag_pattern ] ~expr ~stmt ()
