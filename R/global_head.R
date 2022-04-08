# Global objects to inherit web UI options/settings
#   conf, opt

#' Global objects to inherit web UI options/settings
#'
#' These provided objects are containers storing default data and information
#' send from web UI. To help develop plugins, we mock them so we can type
#' it in R(Studio) and access the available fields.
#'
#' @references
#' - [Guideline - Data JSON](https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E)
#' - [后台任务代码说明](https://hiplot.com.cn/docs/zh/development-guides/#%E5%90%8E%E5%8F%B0%E4%BB%BB%E5%8A%A1%E4%BB%A3%E7%A0%81%E8%AF%B4%E6%98%8E)
#' @export
conf = list(
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
attr(conf, which = "reference") = "https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E"


#' @rdname conf
#' @export
opt = list(
  inputFile = "placeholder",
  confFile = "placeholder",
  outputFilePrefix = "placeholder",
  tool = "placeholder",
  module = "placeholder",
  simple = "placeholder",
  enableExample = "placeholder"
)

# Web UI options list, query and setting ----------------------------------

#' Functions to manage the global setting objects
#' @name globs
#' @rdname globs
#' @examples
#' \dontrun{
#' conf = globs_get("conf")
#' # Modify conf
#' # xxx
#' # Then reassign
#' globs_set("conf")
#' }
NULL

#' @describeIn globs list available global objects
#' @export
globs_list = function() {
  c("conf", "opt")
}

#' @describeIn globs get the data from specified global objects
#' @param x the name of the global object, the most common used is "conf".
#' @export
globs_get = function(x) {
  x = match.arg(x, choices = globs_list())
  y = get(x, envir = .GlobalEnv)
  stopifnot(!is.null(y))
  y
}

#' @describeIn globs reassign the specified data (with name) to available global object
#' @param name if x is not a valid global object name, then specify it with `name`.
#' This is not recommended.
#' @export
globs_set = function(x, name = NULL) {
  if (!is.null(name)) {
    name = match.arg(name, choices = globs_list())
  } else {
    x = match.arg(x, choices = globs_list())
  }
  y = get(x, envir = rlang::caller_env())
  stopifnot(!is.null(y))
  # Reassign to global setting
  assign(if (is.null(name)) x else name,
         y, envir = .GlobalEnv)
}


# R running global options ------------------------------------------------

#' Set R global options before starting plugin
#' @export
set_global_options = function() {
  options(stringsAsFactors = FALSE)
  options(warn = -1)
}


# Conf update -------------------------------------------------------------

#' Check and update global setting before starting plugin
#' @export
set_global_confs = function() {
  conf = globs_get("conf")
  on.exit(globs_set("conf"))

  if (is.null(conf$general$font)) {
    conf$general$font = "Arial"
  }

  # Update conf from UI to R backend
  ui_extra2general = c(
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
      conf$general[[ui_extra2general[i]]] = conf$extra[[i]]
      conf$extra[[i]] = NULL
    }
  }

  ui_general_remap = c(palette_cont = "paletteCont")

  for (i in names(ui_general_remap)) {
    if (i %in% names(conf$general)) {
      if (!is.null(conf$general[[ui_general_remap[i]]])) {
        conf$general[[ui_general_remap[i]]] = conf$general[[i]]
        conf$general[[i]] = NULL
      }
    }
  }

}
