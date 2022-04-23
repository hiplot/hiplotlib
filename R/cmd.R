#' Run system commands
#' @param cmd the command to run.
#' @param print_cmd if `TRUE`, also print the command.
#' @export
system_safe <- function(cmd, print_cmd = FALSE) {
  if (print_cmd) {
    flog.info(str_remove_all(cmd, "/cluster/apps/hiplot/userdata|/cluster/apps/hiplot/web/src/scripts"))
  }
  stderr_fn <- tempfile()
  stdout_fn <- tempfile()
  status <- r_safe(
    function(cmd) {
      system(cmd)
    },
    args = list(cmd),
    stderr = stderr_fn,
    stdout = stdout_fn
  )
  if (status != 0) {
    stop(str_replace_all(
      paste0(readLines(stderr_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ))
  } else {
    cat(str_replace_all(
      paste0(readLines(stdout_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ),
    append = TRUE
    )
    cat(str_replace_all(
      paste0(readLines(stderr_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ),
    append = TRUE
    )
  }
}
