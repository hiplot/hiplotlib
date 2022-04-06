# This code is the "head" of hiplot r script
# function: data / options / configuration input and preprocess

# global options
options(stringsAsFactors = F)
options(warn = -1)

if (is.null(conf$general$font)) {
  conf$general$font <- "Arial"
}

# convert old config
ref <- c(
  font_family = "font",
  family = "font",
  title_size = "titleSize",
  axis_title_size = "axisTitleSize",
  legend_pos = "legendPos", legend_dir = "legendDir",
  legend_title_size = "legendTitleSize",
  legendTextSize = "legendTextSize",
  x_axis_angle = "xAxisTextAngle",
  hjust = "xAxisHjust",
  vjust = "xAxisVjust",
  fontsize_row = "fontsizeRow",
  fontsize_col = "fontsizeCol",
  digets = "digets"
)

for (i in names(ref)) {
  if (i %in% names(conf$extra)) {
    conf$general[[ref[i]]] <- conf$extra[[i]]
    conf$extra[[i]] <- NULL
  }
}

ref2 <- c(palette_cont = "paletteCont")

for (i in names(ref2)) {
  if (i %in% names(conf$general)) {
    if (!is.null(conf$general[[ref2[i]]])) {
      conf$general[[ref2[i]]] <- conf$general[[i]]
      conf$general[[i]] <- NULL
    }
  }
}
