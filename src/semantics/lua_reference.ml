(* Effectively lifted from https://www.lua.org/manual/5.1/#index. Ideally we could automate this. *)

type t =
  | Unknown
  | InManual of string
  | Undocumented

let manual_section sec = "https://www.lua.org/manual/5.1/manual.html#" ^ sec

let to_url = function
  | Unknown | Undocumented -> None
  | InManual s -> Some (manual_section s)

let lookup_type = function
  | "coroutine" -> InManual "5.2"
  | "string" -> InManual "5.4"
  | "table" -> InManual "5.5"
  | "nil" | "number" | "integer" | "boolean" | "function" ->
      Undocumented (* Could do 2.2, but seems a little dubious *)
  | _ -> Unknown

let lookup_name = function
  | "coroutine" -> InManual "5.2"
  | "debug" -> InManual "5.9"
  | "io" -> InManual "5.7"
  | "math" -> InManual "5.6"
  | "os" -> InManual "5.8"
  | "package" -> InManual "5.3"
  | "string" -> InManual "5.4"
  | "table" -> InManual "5.5"
  | ( "_G" | "_VERSION" | "assert" | "collectgarbage" | "coroutine.create" | "coroutine.resume"
    | "coroutine.running" | "coroutine.status" | "coroutine.wrap" | "coroutine.yield"
    | "debug.debug" | "debug.getfenv" | "debug.gethook" | "debug.getinfo" | "debug.getlocal"
    | "debug.getmetatable" | "debug.getregistry" | "debug.getupvalue" | "debug.setfenv"
    | "debug.sethook" | "debug.setlocal" | "debug.setmetatable" | "debug.setupvalue"
    | "debug.traceback" | "dofile" | "error" | "file:close" | "file:flush" | "file:lines"
    | "file:read" | "file:seek" | "file:setvbuf" | "file:write" | "getfenv" | "getmetatable"
    | "io.close" | "io.flush" | "io.input" | "io.lines" | "io.open" | "io.output" | "io.popen"
    | "io.read" | "io.stderr" | "io.stdin" | "io.stdout" | "io.tmpfile" | "io.type" | "io.write"
    | "ipairs" | "load" | "loadfile" | "loadstring" | "math.abs" | "math.acos" | "math.asin"
    | "math.atan" | "math.atan2" | "math.ceil" | "math.cos" | "math.cosh" | "math.deg" | "math.exp"
    | "math.floor" | "math.fmod" | "math.frexp" | "math.huge" | "math.ldexp" | "math.log"
    | "math.log10" | "math.max" | "math.min" | "math.modf" | "math.pi" | "math.pow" | "math.rad"
    | "math.random" | "math.randomseed" | "math.sin" | "math.sinh" | "math.sqrt" | "math.tan"
    | "math.tanh" | "module" | "next" | "os.clock" | "os.date" | "os.difftime" | "os.execute"
    | "os.exit" | "os.getenv" | "os.remove" | "os.rename" | "os.setlocale" | "os.time"
    | "os.tmpname" | "package.cpath" | "package.loaded" | "package.loaders" | "package.loadlib"
    | "package.path" | "package.preload" | "package.seeall" | "pairs" | "pcall" | "print"
    | "rawequal" | "rawget" | "rawset" | "require" | "select" | "setfenv" | "setmetatable"
    | "string.byte" | "string.char" | "string.dump" | "string.find" | "string.format"
    | "string.gmatch" | "string.gsub" | "string.len" | "string.lower" | "string.match"
    | "string.rep" | "string.reverse" | "string.sub" | "string.upper" | "table.concat"
    | "table.insert" | "table.maxn" | "table.remove" | "table.sort" | "tonumber" | "tostring"
    | "type" | "unpack" | "xpcall" ) as x ->
      InManual x
  | x -> lookup_type x
