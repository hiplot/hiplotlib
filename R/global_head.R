# Global objects to inherit web UI options/settings
#   conf

# Web UI options list, query and setting ----------------------------------
# conf = globs_get("conf")
# # Modify conf
# # xxx
# # Then reassign
# globs_set("conf")

globs_list = function() {
  list(conf = "conf")
}

globs_get = function(x) {
  y = get(x, envir = .GlobalEnv)
  stopifnot(!is.null(y))
  y
}

globs_set = function(x, name = NULL) {
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
