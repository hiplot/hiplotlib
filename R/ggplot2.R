#' Prestored settings and functions related to ggplot2 or plots
#' @name ggplot2
#' @rdname ggplot2
NULL

#' @describeIn ggplot2 A vector of colors
#' @export
cb_palette <- c(
  "#ed1299", "#246b93", "#cc8e12",
  "#d561dd", "#c93f00",
  "#e86502", "#39ba30", "#8249aa",
  "#e07233", "#3367e0",
  "#ce2523", "#f7aa5d",
  "#03827f", "#931635", "#373bbf",
  "#ef3bb6", "#d66551",
  "#1a918f", "#ff66fc", "#2927c4",
  "#7149af", "#8e3af4", "#f9a270",
  "#22547f", "#db5e92",
  "#6f25e8", "#280f7a",
  "#6373ed", "#5b910f", "#7b34c1",
  "#d80fc1", "#dd27ce", "#07a301",
  "#167275", "#391c82",
  "#2baeb5", "#925bea"
)

#' @describeIn ggplot2 A vector of ggsci palette names
#' @export
ggsci_palette_names <- c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "d3", "locuszoom", "igv", "uchicago", "startrek", "tron", "futurama", "rickandmorty", "simpsons")
#' @describeIn ggplot2 Maximum color number provided by palette of [ggsci_palette_names]
#' @export
ggsci_palette_length <- c(10, 10, 8, 9, 7, 10, 26, 10, 7, 51, 9, 7, 7, 12, 12, 16)
#' @describeIn ggplot2 A vector of RColorBrewer palette names
#' @export
brewer_palette_names <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
#' @describeIn ggplot2 A vector of prism palette names
#' @export
prism_palette_names <- c("autumn_leaves", "beer_and_ales", "black_and_white", "blueprint", "blueprint2", "blueprint3", "candy_bright", "candy_soft", "colorblind_safe", "colors", "diazo", "earth_tones", "evergreen", "fir", "fir2", "fir3", "flames", "flames2", "floral", "floral2", "greenwash", "inferno", "magma", "mustard_field", "mustard_field2", "muted_rainbow", "neon", "ocean", "ocean2", "ocean3", "office", "pastels", "pearl", "pearl2", "plasma", "prism_dark", "prism_dark2", "prism_light", "prism_light2", "purple_passion", "quiet", "quiet2", "shades_of_gray", "spring", "spring2", "stained_glass", "stained_glass2", "starry", "starry2", "summer", "sunny_garden", "sunny_garden2", "sunny_garden3", "the_blues", "viridis", "warm_and_sunny", "warm_pastels", "warm_pastels2", "waves", "waves2", "winter_bright", "winter_soft", "wool_muffler", "wool_muffler2", "wool_muffler3")
#' @describeIn ggplot2 A vector of graf palette names
#' @export
graf_palette_names <- c("grafify", "grafify2", "grafify_c")

#' @describeIn ggplot2 A vector of ggplot2 themes
#' @export
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

#' @describeIn ggplot2 A list of ggplot2 themes
#' @export
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

#' @describeIn ggplot2 Set ggplot2 theme for a ggplot object
#' @param p a ggplot object.
#' @param theme a string specifying theme.
#' @export
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

#' @describeIn ggplot2 Return ggplot2 theme based on string
#' @export
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

# TODO: make sure it is unused by plugin
# choose_ggplot_theme2 <- function(theme = "theme_bw") {
#   t <- NULL
#   if (theme %in% ggplot2_themes) {
#     cmd <- sprintf("%s()", theme)
#     for (j in names(ggplot2_themes2)) {
#       if (theme %in% ggplot2_themes2[[j]]) {
#         library(j, character.only = TRUE)
#       }
#     }
#     t <- eval(parse(text = cmd))
#   }
#   return(t)
# }

#' @describeIn ggplot2 Return position type
#' @param label a position label, one of 'dodge', 'fill' and 'stack'.
#' @export
position_type <- function(label = c("dodge", "fill", "stack")) {
  label <- match.arg(label)
  switch(label,
    dodge = position_dodge(0.9),
    fill = position_fill(0.9),
    stack = position_stack(0.9)
  )
}

#' @describeIn ggplot2 Set palette and theme from conf for a ggplot object
#' @param conf a conf object, see [conf].
#' @export
set_palette_theme <- function(p, conf = globs_get("conf")) {
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

  return(p)
}

#' @describeIn ggplot2 Set complex general theme from conf for a ggplot object
#' @export
set_complex_general_theme <- function(p, conf = globs_get("conf")) {
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

#' @describeIn ggplot2 Return ggplot2 scale of fill palette with predefined or custom colors
#' @param custom a user specified custom color vector.
#' @param u one of a predefined palette name. See [ggsci_palette_names],
#' [brewer_palette_names], [prism_palette_names], etc.
#' @export
return_hiplot_palette <- function(u, custom = NULL) {
  if (u == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    palette <- scale_fill_manual(values = custom)
  } else if (u %in% ggsci_palette_names) {
    palette <- eval(parse(text = sprintf("ggsci::scale_fill_%s()", u)))
  } else if (u %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, u)
    colors <- rev(colorRampPalette(ref)(500))
    palette <- scale_fill_manual(values = colors)
  } else if (u == "cb") {
    palette <- scale_fill_manual(values = cb_palette)
  } else if (u == "random") {
    set.seed(Sys.time())
    palette <- scale_fill_manual(values = randomcoloR::distinctColorPalette(500))
  } else if (u == "rainbow") {
    palette <- scale_fill_manual(values = rainbow(500))
  } else if (u %in% prism_palette_names) {
    palette <- scale_fill_manual(values = ggprism::ggprism_data$colour_palettes[[u]])
  } else if (u %in% graf_palette_names) {
    palette <- eval(parse(text = sprintf("grafify::scale_fill_%s()", u)))
  } else {
    palette <- scale_fill_discrete()
  }
  return(palette)
}

#' @describeIn ggplot2 Return ggplot2 scale of color palette with predefined or custom colors,
#' similar to [return_hiplot_palette]
#' @param custom a user specified custom color vector.
#' @export
return_hiplot_palette_color <- function(u, custom = NULL) {
  if (u == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    palette <- scale_color_manual(values = custom)
  } else if (u %in% ggsci_palette_names) {
    palette <- eval(parse(text = sprintf("ggsci::scale_color_%s()", u)))
  } else if (u %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, u)
    colors <- rev(colorRampPalette(ref)(500))
    palette <- scale_colour_manual(values = colors)
  } else if (u == "cb") {
    palette <- scale_color_manual(values = cb_palette)
  } else if (u == "random") {
    set.seed(Sys.time())
    palette <- scale_color_manual(values = randomcoloR::distinctColorPalette(500))
  } else if (u == "rainbow") {
    palette <- scale_color_manual(values = rainbow(500))
  } else if (u %in% prism_palette_names) {
    palette <- scale_color_manual(values = ggprism::ggprism_data$colour_palettes[[u]])
  } else if (u %in% graf_palette_names) {
    palette <- eval(parse(text = sprintf("grafify::scale_color_%s()", u)))
  } else {
    palette <- scale_color_discrete()
  }
  return(palette)
}

#' @describeIn ggplot2 Get colors from a color palette
#' @param n number of colors.
#' @export
get_hiplot_color <- function(u, n = -1, custom = NULL) {
  # this function extract the colors from ggsci package
  # u: palette
  # n: number of colors

  if (u == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    col <- custom
  } else if (u %in% ggsci_palette_names) {
    plen <- ggsci_palette_length[which(u == ggsci_palette_names)]
    col <- eval(parse(text = sprintf("ggsci::pal_%s()(%s)", u, plen)))
  } else if (u %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, u)
    col <- rev(colorRampPalette(ref)(500))
  } else if (u == "cb") {
    col <- cb_palette
  } else if (u == "random") {
    set.seed(Sys.time())
    if (n == -1) {
      col <- randomcoloR::distinctColorPalette(500)
    } else {
      col <- randomcoloR::distinctColorPalette(n)
    }
  } else if (u == "rainbow") {
    if (n == -1) {
      col <- rainbow(500)
    } else {
      col <- rainbow(n)
    }
  } else if (u %in% prism_palette_names) {
    col <- ggprism::ggprism_data$colour_palettes[[u]]
  } else if (u %in% graf_palette_names) {
    col <- grafify::graf_col_palette()(30)
  } else {
    col <- gg_color_default(100)
  }
  if (!is.na(n) && n == -1) {
    return(col)
  }
  return(rep(col, 100)[1:n])
}


#' @describeIn ggplot2 A color generator for default colors in ggplot
#' @export
gg_color_default <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' @describeIn ggplot2 An update version of brew.pal function
#' @param panel defines the color panel you want
#' @export
brewer_pal_update <- function(n, panel = "Set3") {
  if (n < 3) {
    return(brewer.pal(3, panel)[1:n])
  } else {
    max.color <- brewer.pal.info$maxcolors[rownames(brewer.pal.info) == panel]
    if (n <= max.color) {
      return(brewer.pal(n, panel))
    } else {
      col <- brewer.pal(max.color, panel)

      col.out <- NULL
      for (i in c(1:c(length(col) - 1))) {
        col.tmp <- colorpanel(ceiling(n / (max.color - 1)), col[i], col[i + 1])
        print(i)
        print(col.tmp)
        col.out <- c(col.out, col.tmp)
      }

      if (length(unique(col.out)) < n) {
        col.out <- NULL
        for (i in c(1:c(length(col) - 1))) {
          col.tmp <- colorpanel(
            ceiling(n / (max.color - 1)) + 1,
            col[i], col[i + 1]
          )
          print(i)
          print(col.tmp)
          col.out <- c(col.out, col.tmp)
        }
      }
      col.out <- unique(col.out)[1:n]
      return(col.out)
    }
  }
}


#' @describeIn ggplot2 Transparent colors
#' @param col a color.
#' @param alpha a alpha value.
#' @export
#' @examples
#' transparentColor("red")
transparentColor <- function(col, alpha = 200) {
  # alpha is an integer >= 1 and <= 255

  col.rgb <- as.numeric(col2rgb(col))
  col.rgb.alpha <- rgb(col.rgb[1], col.rgb[2], col.rgb[3], alpha = alpha, maxColorValue = 255)
  return(col.rgb.alpha)
}

#' @describeIn ggplot2 A custom color filter
#' @param cols colors.
#' @export
#' @examples
#' custom_color_filter("AAAAAA, CCCCCC")
custom_color_filter <- function(cols) {
  cols <- str_replace_all(cols, "\"|'|,", " ")
  cols <- unlist(str_split(cols, " |,|;"))
  cols <- cols[!is.na(cols) & cols != ""]
  cols[!str_detect(cols, "#")] <- paste0(
    "#",
    cols[!str_detect(cols, "#")]
  )
  return(cols)
}

#' @describeIn ggplot2 A alter_fun used in ComplexHeatmap
#' @param x,y,w,h,v a set of parameters passing to [grid::grid.rect].
#' @export
alter_fun <- function(x, y, w, h, v) {
  n <- sum(v)
  h <- h * 0.98
  grid.rect(x, y, w - unit(0.29, "mm"), h - unit(0.1, "mm"),
    gp = gpar(fill = "#f4f4f4", col = "#f4f4f4")
  )
  if (any(v[names(cols)], na.rm = TRUE)) {
    grid.rect(x, y - h * 0.5 + 1:n / n * h,
      w * 1, 1 / n * h,
      gp = gpar(fill = cols[names(which(v))], col = NA),
      just = "top"
    )
  }
}

#' @describeIn ggplot2 A color function used in ComplexHeatmap
#' @export
col_fun_cont <- function(x, cols = c(
                           "#196ABD", "#3399FF", "#3399FF", "#f4f4f4", "#f4f4f4",
                           "#f4f4f4", "#FF3333", "#FF3333", "#C20B01"
                         )) {
  circlize::colorRamp2(
    get_quant(x),
    cols
  )
}

#' @describeIn ggplot2 A col_tag used in ComplexHeatmap (TODO: make sure)
#' @export
col_tag <- c("#f4f4f4", "#5a5a5a")

#' @describeIn ggplot2 A mapping between mutation type and colors used in ComplexHeatmap (TODO: make sure)
#' @export
mut_cols <- c(
  "missense" = "#3987CC",
  "frameshift" = "#DB3D3D",
  "frameshift_del" = "#DB3D3D",
  "frameshift_ins" = "#DB3D3D",
  "frameshift_mutation" = "#DB3D3D",
  "stopgain" = "#20D65C",
  "splicing" = "#663FFB",
  "splice" = "#663FFB",
  "nonframeshift" = "#8B00AA",
  "nonframeshift_del" = "#CC66FF",
  "nonframeshift_ins" = "#8B564C",
  "nonsynonymous" = "#3987CC",
  "nonsense" = "#FF7F0E",
  "protein_del" = "#CC66FF",
  "proteindel" = "#CC66FF",
  "proteinDel" = "#CC66FF",
  "protein_ins" = "#8B564C",
  "proteinins" = "#8B564C",
  "proteinIns" = "#8B564C",
  "proteinSub" = "#AACCAA",
  "proteinsub" = "#AACCAA",
  "protein_sub" = "#AACCAA",
  "nonframeshiftSub" = "#AACCAA",
  "nonframeshiftsub" = "#AACCAA",
  "nonframeshift_sub" = "#AACCAA",
  "splice_region" = "#9369bb",
  "one_hit" = "#819981",
  "tow_hit" = "#499E49",
  "three_hit" = "#237023",
  "four_hit" = "#007023"
)

#' @describeIn ggplot2 Draw a Map
#' @param rds a file storing in the format of `.RDS`.
#' @param keyname a keyname to plot.
#' @param filter_names used for filtering.
#' @export
draw_map <- function(rds, keyname, filter_names = NULL) {
  # TODO:  I don't dont if
  # they can work properly.

  script_dir <- get("script_dir", envir = rlang::global_env())
  data[, 2] <- as.numeric(data[, 2])
  colnames(data)[1] <- "region"
  data <- data.table(data)
  dt_map <- readRDS(file.path(script_dir, rds))
  setkey(data, region)
  eval(parse(text = sprintf("setkey(dt_map, cols = %s)", keyname)))
  data2 <- data[dt_map]
  data <- as.data.frame(data)
  data2 <- as.data.frame(data2)
  data2$group <- as.numeric(data2$group)
  data2[, colnames(data)[2]] <- as.numeric(data2[, colnames(data)[2]])
  if (!is.null(filter_names)) {
    data2 <- subset(data2, !region %in% filter_names)
  }
  # scale
  rg <- max(data[, 2], na.rm = TRUE) - min(data[, 2], na.rm = TRUE)
  brks_scale <- seq(
    round(min(data[, 2], na.rm = TRUE)),
    round(max(data[, 2], na.rm = TRUE)),
    round(rg / 7)
  )

  if (is.null(conf$general$paletteCont)) {
    conf$general$paletteCont <- "RdYlBu"
  }
  colors <- get_hiplot_color(conf$general$paletteCont, -1, conf$general$paletteCustom)
  p <- ggplot(data2) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = data2[, colnames(data)[2]]),
      alpha = 0.9, size = 0.5
    ) +
    geom_path(aes(x = long, y = lat, group = group),
      color = "black", size = 0.2
    ) +
    scale_fill_gradientn(
      colours = colors,
      breaks = brks_scale,
      name = conf$general$legendTitle,
      guide = guide_legend(
        direction = "vertical",
        keyheight = unit(7, units = "mm"),
        keywidth = unit(75, units = "mm"),
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5,
        nrow = 1,
        byrow = T,
        reverse = F,
        label.position = "bottom"
      )
    ) +
    theme(
      text = element_text(color = "#3A3F4A"),
      axis.text = element_blank(),
      axis.ticks.length = unit(1, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 26 * 1.5, color = "black"),
      legend.title = element_text(size = 30 * 1.5, color = "black"),
      plot.title = element_text(face = "bold", size = 30 * 1.5, hjust = 0.5, margin = margin(t = 20, b = 75), color = "black"),
      plot.background = element_rect(fill = conf$extra$background, color = conf$extra$background),
      panel.background = element_rect(fill = conf$extra$background, color = NA),
      legend.background = element_rect(fill = conf$extra$background, color = NA),
      plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
    ) +
    labs(
      x = NULL, y = NULL,
      title = conf$general$title
    )
  export_single(p)
  return(p)
}

utils::globalVariables(
  c("region", "long", "lat", "group")
)
