pkgs <- c("ezcox")
pacman::p_load(pkgs, character.only = TRUE)

call_ezcox <- function(data,
                       covariates,
                       controls,
                       merge_models,
                       vars_to_show,
                       drop_controls,
                       add_caption) {

  if (ncol(data) < 3) {
    stop("Input data should have at least 3 columns!")
  }

  if (!all(c("time", "status") %in% colnames(data))) {
    cat("WARN: 'time' and 'status' colnames not exist in input data.",
        sep = "\n")
    cat("WARN: rename the first and the second column as 'time' and 'status'.",
        sep = "\n")
    colnames(data)[1:2] <- c("time", "status")
  }

  data$time <- as.numeric(data$time)
  data$status <- as.integer(data$status) # Can only be 0 or 1 here

  # 协变量
  if (covariates == "" || is.null(covariates)) {
    covariates <- setdiff(colnames(data), c("time", "status"))
  }

  # 控制变量
  if (controls == "" || is.null(controls)) {
    controls <- NULL
  }

  # 结果图显示变量
  if (vars_to_show == "" || is.null(vars_to_show)) {
    vars_to_show <- NULL
  }

  ezcox::show_forest(
    data = data,
    covariates = covariates,
    controls = controls,
    merge_models = merge_models,
    vars_to_show = vars_to_show,
    drop_controls = drop_controls,
    add_caption = add_caption
  )
}
