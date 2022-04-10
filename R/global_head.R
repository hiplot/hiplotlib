# Global objects to inherit web UI options/settings
#   conf, opt

#' Mocked global objects to inherit web UI options/settings for plugin developer
#'
#' These provided objects are containers storing default data and information
#' send from web UI. To help develop plugins, we mock them so we can type
#' it in R(Studio) and access the available fields.
#' @rdname conf
#' @references
#' - [Guideline - Data JSON](https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E)
#' - [后台任务代码说明](https://hiplot.com.cn/docs/zh/development-guides/#%E5%90%8E%E5%8F%B0%E4%BB%BB%E5%8A%A1%E4%BB%A3%E7%A0%81%E8%AF%B4%E6%98%8E)
#' @export
.conf <- list(
  data = list(),
  dataArg = list(),
  general = list(
    cmd = "",
    taskname = "",
    imageExportType = c("jpeg", "pdf"),
    plumber = TRUE,
    cliMode = FALSE,
    size = list(width = 4, height = 2.5),
    title = "",
    font = "Arial",
    theme = "default", # "theme_bw",
    transformX = "",
    transformY = "",
    transformG = "",
    transformS = "",
    palette = "lancet",
    palette2 = "",
    paletteCont = "RdBu",
    legendTitle = "",
    legendPos = "right",
    legendDir = "vertical",
    xlab = "",
    ylab = "",
    xbreaks = "",
    ybreaks = "",
    legendTitleSize = 10,
    legendTextSize = 10,
    axisTitleSize = 12,
    axisTextFontSize = 10,
    xAxisTextAngle = 0,
    xAxisHjust = 0.5,
    xAxisVjust = 1,
    fontsizeRow = 10,
    fontsizeCol = 10,
    digets = 2,
    alpha = 1,
    titleSize = 12
  ),
  extra = list(
    background = "#FFFFFF"
  )
)
attr(.conf, which = "reference") <- "https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E"


#' @rdname conf
#' @export
.opt <- list(
  inputFile = "placeholder",
  confFile = "placeholder",
  outputFilePrefix = file.path(tempdir(), uuid::UUIDgenerate()),
  tool = "placeholder",
  module = "basic",
  simple = TRUE,
  enableExample = TRUE
)

# Web UI options list, query and setting ----------------------------------

#' Functions to manage the global setting objects
#' @name globs
#' @rdname globs
#' @examples
#' \dontrun{
#' conf <- globs_get("conf")
#' # Modify conf
#' # xxx
#' # Then reassign
#' globs_set("conf")
#' }
NULL

#' @describeIn globs list available global objects
#' @export
globs_list <- function() {
  c("conf", "opt")
}

#' @describeIn globs get the data from specified global objects
#' @param x the name of the global object, the most common used is "conf".
#' @export
globs_get <- function(x) {
  x <- match.arg(x, choices = globs_list())
  y <- tryCatch(get(x, envir = rlang::global_env()),
                error = function(e) {
                  message("This should only show when developing plugin, if you see this message in run mode, please check")
                  get(paste0(".", x), envir = rlang::global_env())
                })
  stopifnot(!is.null(y))
  y
}

#' @describeIn globs reassign the specified data (with name) to available global object
#' @param name if x is not a valid global object name, then specify it with `name`.
#' This is not recommended.
#' @export
globs_set <- function(x, name = NULL) {
  y <- get(x, envir = rlang::caller_env())
  stopifnot(!is.null(y))
  # Reassign to global setting
  assign(if (is.null(name)) x else name,
    y,
    envir = rlang::global_env()
  )
}


# R running global options ------------------------------------------------

#' Set R global options before starting plugin
#' @export
set_global_options <- function() {
  options(stringsAsFactors = FALSE)
  options(warn = -1)

  suppressWarnings(extrafont::loadfonts(quiet = TRUE))

  assign("upload_dir", "/cluster/apps/hiplot/userdata", envir = rlang::global_env())
  assign("utils_dir", "/cluster/apps/hiplot/web/src/scripts/utils", envir = rlang::global_env())
}

#' Set general R packages before starting plugin
#' @export
set_general_pkgs <- function() {
  pkgs <- c("cowplot", "patchwork", "extrafont", "R.utils")
  sapply(pkgs, function(x) {
    eval(parse(text = "library(x, character.only = TRUE)"))
  })
  message(sprintf(
    "General packages %s are loaded.",
    paste(pkgs, collapse = ", ")
  ))
}

#' Set extra R packages before starting plugin
#' @export
set_extra_pkgs <- function() {
  pkgs <- c(
    "reshape2", "ggplot2", "grid", "ggplotify",
    "Hmisc", "dplyr", "tidyverse", "gplots",
    "ggthemes", "see", "ggcharts", "ggdist",
    "ComplexHeatmap", "genefilter", "pheatmap"
  )
  sapply(pkgs, function(x) {
    eval(parse(text = "library(x, character.only = TRUE)"))
  })
  message(sprintf(
    "Extra packages %s are loaded.",
    paste(pkgs, collapse = ", ")
  ))
}

# Conf update -------------------------------------------------------------

#' Check and update global setting before starting plugin
#' @export
set_global_confs <- function() {
  conf <- globs_get("conf")
  on.exit(globs_set("conf"))

  if (is.null(conf$general$font)) {
    conf$general$font <- "Arial"
  }

  # Update conf from UI to R backend
  ui_extra2general <- c(
    font_family = "font",
    family = "font", # TODO 重复了，能否去除？
    title_size = "titleSize",
    axis_title_size = "axisTitleSize",
    legend_pos = "legendPos",
    legend_dir = "legendDir",
    legend_title_size = "legendTitleSize",
    legendTextSize = "legendTextSize",
    x_axis_angle = "xAxisTextAngle",
    hjust = "xAxisHjust",
    vjust = "xAxisVjust",
    fontsize_row = "fontsizeRow",
    fontsize_col = "fontsizeCol",
    digets = "digets"
  )

  for (i in names(ui_extra2general)) {
    if (i %in% names(conf$extra)) {
      conf$general[[ui_extra2general[i]]] <- conf$extra[[i]]
      conf$extra[[i]] <- NULL
    }
  }

  ui_general_remap <- c(palette_cont = "paletteCont")

  for (i in names(ui_general_remap)) {
    if (i %in% names(conf$general)) {
      if (!is.null(conf$general[[ui_general_remap[i]]])) {
        conf$general[[ui_general_remap[i]]] <- conf$general[[i]]
        conf$general[[i]] <- NULL
      }
    }
  }
}

#' Preprocess the data argument from conf object
#' @export
data_arg_preprocess <- function() {
  conf <- globs_get("conf")
  on.exit(globs_set("conf"))

  if (is.null(conf$dataArg) || length(conf$dataArg) == 0) {
    return(NULL)
  }
  for (i in 1:length(conf$dataArg)) {
    for (j in 1:length(conf$dataArg[[i]])) {
      conf$dataArg[[i]][[j]]$blackItems <- NULL
      conf$dataArg[[i]][[j]]$required <- NULL
      conf$dataArg[[i]][[j]]$individual <- NULL
      conf$dataArg[[i]][[j]]$label <- NULL
      conf$dataArg[[i]][[j]]$value <- unlist(conf$dataArg[[i]][[j]]$value)
      conf$dataArg[[i]][[j]]$value[is.null(conf$dataArg[[i]][[j]]$value)] <- ""
    }
  }
  # assign("conf", conf, envir = .GlobalEnv)
}


# Init opt and conf from global environment --------------------------
# init_global_opts = function() {
#   conf = globs_get("conf")
#   opt = globs_get("opt")
#   assign("conf", conf, envir = rlang::caller_env())
#   assign("opt", opt, envir = rlang::caller_env())
# }


