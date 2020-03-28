{
[@@@coverage exclude_file]

type t = Raw of string | Key of string

let pp b = function
| Raw x -> Format.fprintf b "%s" x
| Key x -> Format.fprintf b "${%s}" x

let with_buffer buffer out =
  match Buffer.contents buffer with
  | "" -> out
  | x-> Buffer.clear buffer; Raw x :: out

}

let ident = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']

rule main buffer out  = parse
| _ as x                         { Buffer.add_char buffer x; main buffer out lexbuf }
| "${" (ident+ as x) "}"         { main buffer (Key x :: with_buffer buffer out) lexbuf }

| "${"                           { Error "Unclosed '${' " }
| eof                            { with_buffer buffer out |> List.rev |> Result.ok }
