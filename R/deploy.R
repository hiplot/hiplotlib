#' Deploy Command Line Interface to System Local Path
#'
#' Only should be used in Unix-like system.
#'
#' @return Nothing.
#' @export
deploy <- function() {
  if (.Platform$OS.type != "unix") stop("This is designed for Unix-like system.")

  dir <- system.file(package = "gcap", mustWork = TRUE)

  cmd1 <- glue::glue("ln -sf {dir}/hisub.R  /usr/local/bin/hisub")
  cmd2 <- glue::glue("ln -sf {dir}/hicli.R  /usr/local/bin/hicli")

  message("Linking hisub command")
  system(cmd1)
  message("Linking hicli command")
  system(cmd2)

  message("Done")
  message("Now you shall run hisub and hicli from anywhere.")
}
