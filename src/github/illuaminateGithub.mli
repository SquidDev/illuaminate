(** Provides integration with GitHub and GitHub actions, allowing for tighter integration between CI
    and the GitHub interface. *)

(** Submit a collection of errors to GitHub.

    This uses the checks API [1] and various environment variables [2] (GITHUB_REPOSITORY,
    GITHUB_TOKEN, GITHUB_SHA) in order to determine how the request should be made.

    + [1]: https://developer.github.com/v3/checks/runs/
    + [2]:
      https://help.github.com/en/actions/automating-your-workflow-with-github-actions/using-environment-variables *)
val publish_errors : IlluaminateCore.Error.t -> (unit, string) result
