open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve
module SSet = Set.Make (String)

module Builtins = struct
  (** A list of names shared across all versions of Lua. *)
  let common =
    (* We split this over multiple lists so ocamlformat doesn't produce something unreadably long. *)
    [ "_G"; "_VERSION"; "arg"; "assert"; "collectgarbage"; "coroutine"; "debug"; "dofile"; "error" ]
    @ [ "getmetatable"; "io"; "ipairs"; "load"; "loadfile"; "math"; "next"; "os"; "package" ]
    @ [ "pairs"; "pcall"; "print"; "rawequal"; "rawget"; "rawset"; "require"; "select" ]
    @ [ "setmetatable"; "string"; "table"; "tonumber"; "tostring"; "type"; "xpcall" ]

  let lua_51 =
    SSet.of_list
    @@ [ "gcinfo"; "getfenv"; "loadstring"; "module"; "newproxy"; "setfenv"; "unpack" ]
    @ common

  let lua_52 = SSet.of_list @@ [ "_ENV"; "bit32"; "rawlen" ] @ common

  let lua_53 = SSet.of_list @@ [ "_ENV"; "bit32"; "rawlen"; "utf8" ] @ common

  let max = List.fold_left SSet.union SSet.empty [ lua_51; lua_52; lua_53 ]

  let parse = function
    | "5.1" | "lua5.1" -> Ok lua_51
    | "5.2" | "lua5.2" -> Ok lua_52
    | "5.3" | "lua5.3" -> Ok lua_53
    | "max" | "lua-max" -> Ok max
    | x -> Error (Printf.sprintf "Unknown collection %S." x)
end

module Global = struct
  type t =
    | Collection of string * SSet.t
    | Name of string

  let show = function
    | Collection (c, _) -> ":" ^ c
    | Name n -> n

  let parse x : (t, string) result =
    match CCString.chop_prefix ~pre:":" x with
    | None -> Ok (Name x)
    | Some name -> Builtins.parse name |> Result.map (fun x -> Collection (name, x))

  let convert = IlluaminateConfig.Term.Converter.atom ~ty:"global" parse show

  let to_set = function
    | Collection (_, c) -> c
    | Name n -> SSet.singleton n
end

let options : SSet.t IlluaminateConfig.Category.key =
  let open IlluaminateConfig in
  let open Term in
  category
  |> Category.add
     @@ let+ f =
          field ~name:"globals"
            ~default:[ Global.Collection ("max", Builtins.max) ]
            ~comment:"List of global variables"
            Converter.(list Global.convert)
        in
        List.to_seq f |> Seq.map Global.to_set |> Seq.fold_left SSet.union SSet.empty

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:unknown-global"

let name known ctx r = function
  | NVar v -> (
    match IlluaminateData.need ctx.data R.key ctx.program |> R.get_var v with
    | { definitions = []; kind = Global; name; _ }
      when (not (SSet.mem name known))
           && not (IlluaminateData.need ctx.data Module_resolve.global_modules () |> SSet.mem name)
      ->
        r.r ~tag "Using unknown global %S" name
    | _ -> () )
  | NDot _ | NLookup _ -> ()

let linter = make ~tags:[ tag ] ~options ~name ()
