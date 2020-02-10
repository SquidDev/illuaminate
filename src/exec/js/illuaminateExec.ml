open Js_of_ocaml

class type result =
  object
    method out : Js.js_string Js.t Js.readonly_prop

    method code : int Js.readonly_prop
  end

let exec : Js.js_string Js.t -> Js.js_string Js.t Js.js_array Js.t -> result Js.t =
  Js.Unsafe.js_expr
    "function (cmd, args) {\n\
    \  try {\n\n\
    \    return { code: 0, out : require('child_process').execFileSync(cmd, args, { encoding: \
     'UTF-8' }) };\n\
    \  } catch(e) {\n\
    \    return { code: e.errno, out: e.code };\n\
    \  }\n\
    \ }"

let exec cmd args =
  let result =
    Array.init (Array.length args - 1) (fun i -> Js.string args.(i + 1))
    |> Js.array
    |> exec (Js.string cmd)
  in
  let out = Js.to_string result##.out in
  match result##.code with
  | 0 -> Ok out
  | e -> Printf.sprintf "%s exited with %d (%s)" cmd e out |> Result.error
