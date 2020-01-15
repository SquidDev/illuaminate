(** Provides integration with GitHub and GitHub actions, allowing for tighter integration between CI
    and the GitHub interface. *)

(** Submit a collection of errors to GitHub.

    This uses the {{:https://developer.github.com/v3/checks/runs/} checks API} and
    {{:https://help.github.com/en/actions/automating-your-workflow-with-github-actions/using-environment-variables}
    various environment variables} ([GITHUB_REPOSITORY], [GITHUB_TOKEN], [GITHUB_SHA]) in order to
    determine how the request should be made. *)
val publish_errors : IlluaminateCore.Error.t -> (unit, string) result
