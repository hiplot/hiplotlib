# Global objects to inherit web UI options/settings
#   conf, opt

# https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E
conf = list(
  data = list(),
  dataArg = list(),
  general = list(
    cmd = "",
    imageExportType = c("jpeg", "pdf"),
    size = list(width = 4, height = 2.5),
    theme = "theme_bw",
    palette = "lancet",
    title = "",
    alpha = 1,
    font_family = NULL,
    title_size = NULL,
    axis_title_size = NULL,
    legend_pos = NULL,
    legend_dir = NULL,
    legend_title_size = NULL,
    legendTextSize = NULL,
    x_axis_angle = NULL,
    hjust = NULL,
    vjust = NULL,
    fontsize_row = NULL,
    fontsize_col = NULL,
    digets = NULL,
  ),
  extra = list()
)
attr(conf, which = "reference") = "https://hiplot.com.cn/docs/zh/development-guides/#data-json-%E6%A0%BC%E5%BC%8F%E8%AF%B4%E6%98%8E"

# Web UI options list, query and setting ----------------------------------
# conf = globs_get("conf")
# # Modify conf
# # xxx
# # Then reassign
# globs_set("conf")

globs_list = function() {
  c("conf", "opt")
}

globs_get = function(x) {
  x = match.arg(x, choices = globs_list())
  y = get(x, envir = .GlobalEnv)
  stopifnot(!is.null(y))
  y
}

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

set_global_options = function() {
  options(stringsAsFactors = FALSE)
  options(warn = -1)
}


# Conf update -------------------------------------------------------------

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
