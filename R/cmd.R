#' Run system commands
#' @param cmd the command to run.
#' @param print_cmd if `TRUE`, also print the command.
#' @param conda_env set conda env.
#' @export
system_safe <- function(cmd, print_cmd = TRUE, conda_env = NULL) {
  if (print_cmd) {
    flog.info(str_remove_all(cmd,
      "/.*/userdata|/.*/hiplot/data/|/.*/opt|/.*/hiplot|/.*/home|/.*/src/scripts|/home/.*/.cache/R"))
  }
  stderr_fn <- tempfile()
  stdout_fn <- tempfile()
  status <- r_safe(
    function(cmd, conda_env = NULL) {
      if (!is.null(conda_env)) {
        reticulate::use_condaenv(conda_env)
        reticulate::py_config()
      }
      system(cmd)
    },
    args = list(cmd, conda_env),
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
