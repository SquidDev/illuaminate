open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter

open struct
  module R = Resolve
  module G = Global
  module F = Stringlib.Format
end

let tag_format = Error.Tag.make Error.Warning "stdlib:string-format"

let tag_pattern = Error.Tag.make Error.Warning "stdlib:string-pattern"

let plural fmt (n, word) =
  if n = 1 then Format.fprintf fmt "1 %s" word else Format.fprintf fmt "%d %ss" n word

let check_format ~span : F.specifier -> 'a note option = function
  | Eof -> Some (note ~span ~tag:tag_format "Expected specifier after '%%'")
  | Unknown c -> Some (note ~span ~tag:tag_format "Unknown specifier '%%%c'" c)
  | Known ("", _) -> None
  | Known (_, '%') -> Some (note ~span ~tag:tag_format "Redundant flags to format string '%%'")
  | _ -> None

let check_format_args ~span specs args =
  let rec count n : F.t -> int option = function
    | [] -> Some n
    | (Raw _ | Known (_, '%')) :: specs -> count n specs
    | Known (_, _) :: specs -> count (n + 1) specs
    | (Eof | Unknown _) :: _ -> None
  in

  let rec go i (specs : F.t) (args : expr SepList0.t) =
    match (specs, args) with
    | [], None -> []
    | [], Some args ->
        [ note
            ~span:(Spanned.(list1 expr) args)
            ~tag:tag_format "Format string takes %a, but given an extra %a." plural (i, "parameter")
            plural
            (SepList1.length args, "argument")
        ]
    | (Raw _ | Known (_, '%')) :: specs, args -> (* Skip %% *) go i specs args
    | (Eof | Unknown _) :: _, _ ->
        (* Abort on malformed strings. Otherwise we'll give lots of spurious warnings. *) []
    | Known _ :: _, None -> (
      match count 0 specs with
      | None ->
          (* Any error message we give will just end up being more confusing. So just skip it. We'll
             be warning on the malformed string anyway. *)
          []
      | Some arity ->
          [ note ~span ~tag:tag_format "Format string takes %a, but only given %a." plural
              (arity, "parameter") plural (i, "argument")
          ] )
    | _ :: _, Some (Mono arg) when Helpers.has_var_return arg ->
        (* If this expression has multiple values, there's very little we can infer from it. *) []
    | Known _ :: specs, Some (Mono _) -> go (i + 1) specs None
    | Known _ :: specs, Some (Cons1 (_, _, args)) -> go (i + 1) specs (Some args)
  in
  go 0 specs args

let linter =
  let string_format = G.parse "string.format" in
  let check_format : 'a. call -> expr -> expr SepList0.t -> 'a note list =
   fun call fmt args ->
    match Eval.eval fmt with
    | RString value ->
        let specs = F.parse value in
        check_format_args ~span:(Spanned.call call) specs args
        @ List.filter_map (check_format ~span:(Spanned.expr fmt)) specs
    | _ -> []
  in

  let check ~(context : context) = function
    | Call { fn; args } as c -> (
        let resolve = Data.get context.program R.key context.data in
        match G.of_expr resolve fn with
        | Some g when g = string_format -> (
          match Helpers.get_call_args args with
          | None -> []
          | Some (Cons1 (fmt, _, args)) -> check_format c fmt (Some args)
          | Some (Mono fmt) -> check_format c fmt None )
        | _ -> [] )
    | Invoke { obj; colon = _; meth; args } as c -> (
      match Node.contents.get meth with
      | "format" -> check_format c obj (Helpers.get_call_args args)
      | _ -> [] )
  in

  let expr () context = function
    | ECall call -> check ~context call
    | _ -> []
  and stmt () context = function
    | SCall call -> check ~context call
    | _ -> []
  in
  make_no_opt ~tags:[ tag_format; tag_pattern ] ~expr ~stmt ()
