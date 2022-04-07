# This code is a libray of useful functions in hiplot r scripts.

# general packages
pacman::p_load(cowplot)
pacman::p_load(patchwork)
pacman::p_load(extrafont)
pacman::p_load(R.utils)

library_extra_pkgs <- function() {
  pacman::p_load(reshape2)
  pacman::p_load(ggplot2)
  pacman::p_load(grid)
  pacman::p_load(ggplotify)
  pacman::p_load(Hmisc)
  pacman::p_load(dplyr)
  pacman::p_load(tidyverse)
  pacman::p_load(gplots)
  pacman::p_load(ggthemes)
  pacman::p_load(see)
  pacman::p_load(ggcharts)
  pacman::p_load(ggdist)
  pacman::p_load(ComplexHeatmap)
  pacman::p_load(genefilter)
  pacman::p_load(pheatmap)
}

suppressWarnings(suppressMessages(loadfonts()))
## an update version of brewer.pal in R package RColorBrewer
# This function could generate any number of colors from RColorBrewer package

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

# add alpha to color
add_alpha <- function(col, alpha) {
  rgb_col <- unlist(col2rgb(col))
  col.res <- NULL
  for (i in seq_len(ncol(rgb_col))) {
    tmp <- rgb(rgb_col[1, i], rgb_col[2, i], rgb_col[3, i],
      alpha = alpha * 255, maxColorValue = 255
    )
    col.res <- c(col.res, tmp)
  }

  return(col.res)
}

# create a funciton to get the same characters in two strings
list_same_string_position <- function(a, b, exclude = c("-", "?"),
                                      ignore_case = TRUE, show_excluded = FALSE, only_position = TRUE) {
  if (ignore_case) {
    a <- toupper(a)
    b <- toupper(b)
  }

  split_seqs <- strsplit(c(a, b), split = "")
  only_diff <- split_seqs[[1]] == split_seqs[[2]]
  only_diff[
    (split_seqs[[1]] %in% exclude) |
      (split_seqs[[2]] %in% exclude)
  ] <- NA

  diff_info <- data.frame(
    which(is.na(only_diff) | only_diff),
    split_seqs[[1]][only_diff], split_seqs[[2]][only_diff]
  )
  names(diff_info) <- c("position", "seq.a", "seq.b")

  if (!show_excluded) diff_info <- na.omit(diff_info)
  if (only_position) {
    diff_info$position
  } else {
    diff_info
  }
}

# add data frame colnames to data frame
colname2data <- function(data) {
  colnames(data) <- paste("V", seq_len(ncol(data)), sep = "")
  return(data)
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

set_factors <- function(x, rev = FALSE) {
  if (rev) {
    x <- factor(x, levels = rev(unique(x)))
  } else {
    x <- factor(x, levels = unique(x))
  }
  return(x)
}

capitalize <- function(string) {
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(
    string[capped],
    1, 1
  ))
  return(string)
}
