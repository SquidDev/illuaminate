open IlluaminateCore

type level =
  | Notice
  | Warning
  | Failure

let level_to_yojson : level -> Yojson.Safe.t = function
  | Notice -> `String "notice"
  | Warning -> `String "warning"
  | Failure -> `String "failure"

type status =
  | Queued
  | InProgress
  | Completed

let status_to_yojson : status -> Yojson.Safe.t = function
  | Queued -> `String "queued"
  | InProgress -> `String "in_progress"
  | Completed -> `String "completed"

type conclusion =
  | Success
  | Failure
  | Neutral
  | Cancelled
  | TimedOut
  | ActionRequired

let conclusion_to_yojson : conclusion -> Yojson.Safe.t = function
  | Success -> `String "success"
  | Failure -> `String "failure"
  | Neutral -> `String "neutral"
  | Cancelled -> `String "cancelled"
  | TimedOut -> `String "timed_out"
  | ActionRequired -> `String "action_required"

type annotation =
  { path : string;
        (** The path of the file to add an annotation to. For example, assets/css/main.css. *)
    start_line : int;  (** The start line of the annotation. *)
    end_line : int;  (** The end line of the annotation. *)
    start_column : int option; [@default None]
        (** The start column of the annotation. Annotations only support start_column and end_column
            on the same line. Omit this parameter if start_line and end_line have different values. *)
    end_column : int option; [@default None]
        (** The end column of the annotation. Annotations only support start_column and end_column
            on the same line. Omit this parameter if start_line and end_line have different values. *)
    annotation_level : level;  (** The level of the annotation. *)
    message : string;
        (** A short description of the feedback for these lines of code. The maximum size is 64 KB. *)
    title : string option [@default None]
        (** The title that represents the annotation. The maximum size is 255 characters. *)
  }
[@@deriving to_yojson]

(** A button which shows up below the result of a checks run. *)
type action =
  { label : string;
        (** The text to be displayed on a button in the web UI. The maximum size is 20 characters. *)
    description : string;
        (** A short explanation of what this action would do. The maximum size is 40 characters. *)
    identifier : string
        (** A reference for the action on the integrator's system. The maximum size is 20
            characters. *)
  }
[@@deriving to_yojson]

(** Output of the check run. *)
type output =
  { title : string;  (** The title of the check run. *)
    summary : string;  (** The summary of the check run. This parameter supports Markdown. *)
    annotations : annotation list
  }
[@@deriving to_yojson]

type check_run_create =
  { name : string;  (** The name of the check. For example, "code-coverage". *)
    head_sha : string;  (** The SHA of the commit. *)
    status : status option; [@default None]  (** The current status. *)
    external_id : string option; [@default None]
        (** A reference for the run on the integrator's system. *)
    started_at : string option; [@default None]  (** The time that the check run began. *)
    conclusion : conclusion option; [@default None]
        (** Required if you provide completed_at or a status of completed. The final conclusion of
            the check *)
    output : output option; [@default None]
        (** Check runs can accept a variety of data in the output object, including a title and
            summary and can optionally provide descriptive details about the run. *)
    actions : action list
        (** Displays a button on GitHub that can be clicked to alert your app to do additional
            tasks. *)
  }
[@@deriving to_yojson]

type check_run_update =
  { status : status option; [@default None]  (** The current status. *)
    conclusion : conclusion option; [@default None]
        (** Required if you provide completed_at or a status of completed. The final conclusion of
            the check *)
    output : output option; [@default None]
        (** Check runs can accept a variety of data in the output object, including a title and
            summary and can optionally provide descriptive details about the run. *)
    actions : action list option [@default None]
        (** Displays a button on GitHub that can be clicked to alert your app to do additional
            tasks. *)
  }
[@@deriving to_yojson]

type check_run_response = { id : int } [@@deriving of_yojson { strict = false }]

module Consts = struct
  let name = "illuaminate"

  let title = "Illuaminate"

  let no_actions = []

  let fix_actions =
    [ { label = "Fix and commit";
        description = "Attempt to fix issues and push a commit.";
        identifier = "fix_and_commit"
      }
    ]

  (** Get a skeleton output with a title and description but no annotations. *)
  let get_skeleton_output (errors : Error.Error.t list) : output =
    let errors, warnings =
      List.fold_left
        (fun (errors, warnings) { Error.Error.tag; _ } ->
          match tag.level with
          | Critical | Error -> (errors + 1, warnings)
          | Warning | Note -> (errors, warnings + 1))
        (0, 0) errors
    in
    { title = "Illuaminate";
      summary = Printf.sprintf "Linting finished with %d error(s) and %d warning(s)" errors warnings;
      annotations = []
    }

  let make_create ~sha ~ok output =
    { name;
      head_sha = sha;
      status = Some Completed;
      external_id = None;
      started_at = None;
      conclusion = Some (if ok then Success else Failure);
      output = Some output;
      actions = (if ok then no_actions else fix_actions)
    }

  let make_update output =
    { status = None; conclusion = None; output = Some output; actions = None }

  (** Convert an error into an annotation. *)
  let to_annotation ({ span; message; tag; details } : Error.Error.t) : annotation =
    let same_line = span.start_line = span.finish_line in
    { path = span.filename.name;
      start_line = span.start_line;
      end_line = span.finish_line;
      start_column = (if same_line then Some span.start_col else None);
      end_column = (if same_line then Some span.finish_col else None);
      annotation_level =
        ( match tag.level with
        | Critical | Error -> Failure
        | Warning -> Warning
        | Note -> Notice );
      message =
        ( match details with
        | None -> message
        | Some d -> Format.asprintf "%s\n%t" message d );
      title = None
    }

  (** Convert our error list into batches of 50 annotations. Note that ordering is not preserved,
      though that should not matter as they will be presented the same in the GitHub interface. *)
  let to_annotations =
    let rec batch n xss xs = function
      | [] -> xs :: xss
      | y :: ys ->
          let y = to_annotation y in
          if n >= 50 then batch 1 (xs :: xss) [ y ] ys else batch (n + 1) xss (y :: xs) ys
    in
    batch 0 [] []
end
