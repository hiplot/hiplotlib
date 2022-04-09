#' Functions and utilities related to hiplot task
#' @name task
#' @rdname task
NULL

#' @describeIn task Update task status
#' @param conf Web UI conf.
#' @export
update_task_status <- function(conf) {
  conf <<- conf
  globs_set("conf")
  outfn <- paste0(dirname(opt$outputFilePrefix), "/task.status.json")
  if (!file.exists(paste0(dirname(opt$outputFilePrefix), "/task.retry"))) {
    outfn <- paste0(dirname(dirname(opt$outputFilePrefix)), "/task.status.json")
  }
  jsonlite::write_json(conf$steps, outfn)
}

#' @describeIn task Create a task to run
#' @param id task id
#' @param label task label
#' @param func a function to run
#' @export
new_task_step <- function(id, label, func) {
  log <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), id)
  print(log)
  con <- file(log)
  sink(con, append = TRUE)
  conf$steps$id <- c(conf$steps$id, id)
  conf$steps$label <- c(conf$steps$label, label)
  conf$steps$status <- c(conf$steps$status, 1)
  update_task_status(conf)
  tryCatch(func(), error = function(e) {
    conf$steps$status[length(conf$steps$status)] <- 3
    flog.error(as.character(e))
    update_task_status(conf)
    sink()
    close(con)
    stop(e)
  })
  conf$steps$status[length(conf$steps$status)] <- 2
  update_task_status(conf)
  sink()
  close(con)
}

#' @describeIn task Run a step of a task #TODO What's the difference from above?
#' @export
run_task_step <- function(id, func) {
  log <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), id)
  con <- file(log)
  sink(con, append = TRUE)
  idx <- which(conf$steps$id == id)
  conf$steps$status[idx] <- 1
  update_task_status(conf)
  tryCatch(func(), error = function(e) {
    conf$steps$status[idx] <- 3
    flog.error(as.character(e))
    update_task_status(conf)
    sink()
    close(con)
    stop(e)
  })
  conf$steps$status[idx] <- 2
  update_task_status(conf)
  sink()
  close(con)
}
