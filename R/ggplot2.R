
ggplot2_themes <- c(
  "theme_abyss", "theme_blackboard", "theme_lucid", "theme_modern",
  "theme_radar", "theme_radar_dark", "theme_linedraw", "theme_minimal",
  "theme_minimal_grid", "theme_minimal_hgrid", "theme_minimal_vgrid",
  "theme_cowplot", "theme_half_open",
  "theme_void", "theme_bw", "theme_classic", "theme_dark", "theme_gray",
  "theme_grey", "theme_light", "theme_ggdist", "theme_tidybayes", "theme_nightblue",
  "theme_ggcharts", "theme_hermit", "theme_ng", "theme_wsj", "theme_calc",
  "theme_igray", "theme_tufte", "theme_economist_white",
  "theme_fivethirtyeight", "theme_foundation", "theme_par",
  "theme_base", "theme_map", "theme_few", "theme_stata", "theme_solid",
  "theme_excel", "theme_pander", "theme_hc", "theme_solarized",
  "theme_clean", "theme_solarized_2", "theme_economist",
  "theme_excel_new", "theme_gdocs", "theme_nothing", "theme_prism"
)

ggplot2_themes2 <- list(
  hrbrthemes = c(
    "theme_ipsum_pub", "theme_ft_rc", "theme_modern_rc",
    "theme_tinyhand", "theme_ipsum_es", "theme_ipsum_rc",
    "theme_ipsum_ps", "theme_ipsum_tw", "theme_ipsum"
  ),
  ggthemes = c("theme_wsj", "theme_calc", "theme_igray", "theme_tufte", "theme_economist_white", "theme_fivethirtyeight", "theme_foundation", "theme_par", "theme_base", "theme_map", "theme_few", "theme_stata", "theme_solid", "theme_excel", "theme_pander", "theme_hc", "theme_solarized", "theme_clean", "theme_solarized_2", "theme_economist", "theme_excel_new", "theme_gdocs"),
  see = c("theme_radar", "theme_radar_dark", "theme_lucid", "theme_abyss", "theme_modern", "theme_blackboard"),
  ggdist = c("theme_ggdist", "theme_tidybayes"),
  ggcharts = c("theme_nightblue", "theme_hermit", "theme_ggcharts", "theme_ng"),
  ggprism = "theme_prism"
)

## ggplot color theme
choose_ggplot_theme <- function(p, theme = "theme_bw") {
  if (theme %in% ggplot2_themes) {
    cmd <- sprintf("p = p + %s()", theme)
    for (j in names(ggplot2_themes2)) {
      if (theme %in% ggplot2_themes2[[j]]) {
        library(j, character.only = TRUE)
      }
    }
    eval(parse(text = cmd))
  }
  return(p)
}

choose_ggplot_theme2 <- function(theme = "theme_bw") {
  t <- NULL
  if (theme %in% ggplot2_themes) {
    cmd <- sprintf("%s()", theme)
    for (j in names(ggplot2_themes2)) {
      if (theme %in% ggplot2_themes2[[j]]) {
        library(j, character.only = TRUE)
      }
    }
    t <- eval(parse(text = cmd))
  }
  return(t)
}

return_ggplot_theme <- function(theme = "theme_bw") {
  if (theme %in% ggplot2_themes) {
    cmd <- sprintf("theme <- %s()", theme)
    for (j in names(ggplot2_themes2)) {
      if (theme %in% ggplot2_themes2[[j]]) {
        library(j, character.only = TRUE)
      }
    }
    eval(parse(text = cmd))
  }
  return(theme)
}

# return position type
position_type <- function(label) {
  if (label == "dodge") {
    return(position_dodge(0.9))
  } else if (label == "fill") {
    return(position_fill(0.9))
  } else if (label == "stack") {
    return(position_stack(0.9))
  } else {
    stop("Error, the label for postion type is wrong.")
  }
}

set_palette_theme <- function(p, conf) {
  ## add color palette
  p <- p + return_hiplot_palette(
    conf$general$palette,
    conf$general$paletteCustom
  )

  p <- p + return_hiplot_palette_color(
    conf$general$palette,
    conf$general$paletteCustom
  )

  ## add theme
  theme <- conf$general$theme
  p <- choose_ggplot_theme(p, theme)
}

set_complex_general_theme <- function(p) {
  p <- p + theme(
    text = element_text(
      family = conf$general$font
    ),
    plot.title = element_text(
      size = conf$general$titleSize,
      hjust = 0.5
    ),
    axis.title = element_text(
      size = conf$general$axisTitleSize
    ),
    axis.text = element_text(
      size = conf$general$axisTextFontSize
    ),
    axis.text.x = element_text(
      angle = conf$general$xAxisTextAngle,
      hjust = conf$general$xAxisHjust,
      vjust = conf$general$xAxisVjust
    ),
    legend.position = conf$general$legendPos,
    legend.direction = conf$general$legendDir,
    legend.title = element_text(
      size = conf$general$legendTitleSize
    ),
    legend.text = element_text(
      size = conf$general$legendTextSize
    )
  )

  if (!is.null(conf$general$xbreaks) && length(conf$general$xbreaks) > 0) {
    conf$general$xbreaks <- sapply(conf$general$xbreaks, function(x) {
      as.numeric(str_split(x, " |,|;"))
    })
    conf$general$xbreaks <- unlist(conf$general$xbreaks)
    p <- p + scale_x_continuous(
      breaks = conf$general$xbreaks,
      n.breaks = length(conf$general$xbreaks)
    )
    p <- p + xlim(conf$general$xbreaks[1], conf$general$xbreaks[length(conf$general$xbreaks)])
  }
  if (!is.null(conf$general$ybreaks) && length(conf$general$ybreaks) > 0) {
    conf$general$ybreaks <- sapply(conf$general$ybreaks, function(x) {
      as.numeric(str_split(x, " |,|;"))
    })
    conf$general$ybreaks <- unlist(conf$general$ybreaks)
    p <- p + scale_y_continuous(
      breaks = conf$general$ybreaks,
      n.breaks = length(conf$general$xbreaks)
    )
    p <- p + ylim(conf$general$ybreaks[1], conf$general$ybreaks[length(conf$general$ybreaks)])
  }
  return(p)
}


