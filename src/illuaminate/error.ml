(** The position that an error occurs at. *)
module Position = struct
  type t =
    { file : File_id.t;
      position_map : Position_map.t;
      start : Position_map.pos;
      finish : Position_map.pos
    }

  let v ~file ~position_map start finish = { file; position_map; start; finish }

  let of_lex_pos ~file ~position_map (start : Lexing.position) (fin : Lexing.position) =
    v ~file ~position_map (Pos start.pos_cnum)
      (Pos (if fin.pos_cnum <= start.pos_cnum then start.pos_cnum else fin.pos_cnum - 1))

  let start_pos p = Position_map.position_of p.position_map p.start
  let finish_pos p = Position_map.position_of p.position_map p.finish

  let compare x y =
    match File_id.compare x.file y.file with
    | 0 ->
        let (Position_map.Pos xs) = x.start and (Position_map.Pos ys) = y.start in
        Int.compare xs ys
    | i -> i
end

(** The severity of an error. *)
type severity =
  | Error  (** A error which will prevent code from working as expected. *)
  | Warning  (** A potential problem or bug. *)
  | Note  (** A minor issue. Effectively just a less major warning. *)

(** Additional tags associated with an error.

    These attach some semantic meaning to a specific error, which allows editors to format them in
    more specialised ways. *)
type tag =
  | Unneccessary
      (** This error warns about code which can be deleted, such as an unused variable or
          unreachable code.*)
  | Deprecated  (** This error warns about deprecated code. *)

type t =
  { code : string;  (** The code associated with this error. *)
    severity : severity;  (** The severity of this error. *)
    tags : tag list;  (** Additional tags for this error. *)
    position : Position.t;
    message : unit Fmt.t;  (** The primary error message. *)
    annotations : (Position.t * unit Fmt.t option) list;
        (** Additional messages to attach to other pieces of source code. *)
    trailer : unit Fmt.t option  (** A trailing message to show at the end of the error. *)
  }

(** Create a new error message. *)
let v ~code ~severity ?(tags = []) position message annotations trailer =
  { code; severity; tags; position; message; annotations; trailer }

(** Create a new error message with no annotations or trailer. *)
let simple ~code ~severity ?tags position message =
  v ~code ~severity ?tags position (fun out () -> message (Fmt.pf out)) [] None

let compare_by_position l r = Position.compare l.position r.position
