brewer_pal_update <- function(n, panel = "Set3") {
  pacman::p_load(RColorBrewer)
  # an update versin of brew.pal function

  # Argument [n] defines the number of colors generated
  # Argument [panel] defines the color panel you want

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
## a color generator for default colors in ggplot
gg_color_default <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

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

ggsci_palette_names <- c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "d3", "locuszoom", "igv", "uchicago", "startrek", "tron", "futurama", "rickandmorty", "simpsons")
ggsci_palette_length <- c(10, 10, 8, 9, 7, 10, 26, 10, 7, 51, 9, 7, 7, 12, 12, 16)
brewer_palette_names <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
prism_palette_names <- c("autumn_leaves", "beer_and_ales", "black_and_white", "blueprint", "blueprint2", "blueprint3", "candy_bright", "candy_soft", "colorblind_safe", "colors", "diazo", "earth_tones", "evergreen", "fir", "fir2", "fir3", "flames", "flames2", "floral", "floral2", "greenwash", "inferno", "magma", "mustard_field", "mustard_field2", "muted_rainbow", "neon", "ocean", "ocean2", "ocean3", "office", "pastels", "pearl", "pearl2", "plasma", "prism_dark", "prism_dark2", "prism_light", "prism_light2", "purple_passion", "quiet", "quiet2", "shades_of_gray", "spring", "spring2", "stained_glass", "stained_glass2", "starry", "starry2", "summer", "sunny_garden", "sunny_garden2", "sunny_garden3", "the_blues", "viridis", "warm_and_sunny", "warm_pastels", "warm_pastels2", "waves", "waves2", "winter_bright", "winter_soft", "wool_muffler", "wool_muffler2", "wool_muffler3")
graf_palette_names <- c("grafify", "grafify2", "grafify_c")

## transparent colors
transparentColor <- function(col, alpha = 200) {
  # alpha is an integer >= 1 and <= 255

  col.rgb <- as.numeric(col2rgb(col))
  col.rgb.alpha <- rgb(col.rgb[1], col.rgb[2], col.rgb[3], alpha = alpha, maxColorValue = 255)
  return(col.rgb.alpha)
}

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

## return ggsci color palette
return_hiplot_palette <- function(p, custom = NULL) {
  library(ggplot2)
  # return fill color

  # npg,aaas,nejm,lancet,jama,jco,ucscgb,d3,locuszoom,
  # igv,uchicago,startrek,tron,futurama,rickandmorty,simpsons
  if (p == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    palette <- scale_fill_manual(values = custom)
  } else if (p %in% ggsci_palette_names) {
    palette <- eval(parse(text = sprintf("ggsci::scale_fill_%s()", p)))
  } else if (p %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, p)
    colors <- rev(colorRampPalette(ref)(500))
    palette <- scale_fill_manual(values = colors)
  } else if (p == "cb") {
    palette <- scale_fill_manual(values = cb_palette)
  } else if (p == "random") {
    set.seed(Sys.time())
    palette <- scale_fill_manual(values = randomcoloR::distinctColorPalette(500))
  } else if (p == "rainbow") {
    palette <- scale_fill_manual(values = rainbow(500))
  } else if (p %in% prism_palette_names) {
    palette <- scale_fill_manual(values = ggprism::ggprism_data$colour_palettes[[p]])
  } else if (p %in% graf_palette_names) {
    palette <- eval(parse(text = sprintf("grafify::scale_fill_%s()", p)))
  } else {
    palette <- scale_fill_discrete()
  }
  return(palette)
}

return_hiplot_palette_color <- function(p, custom = NULL) {
  library(ggplot2)
  # return color

  # npg,aaas,nejm,lancet,jama,jco,ucscgb,d3,locuszoom,
  # igv,uchicago,startrek,tron,futurama,rickandmorty,simpsons
  if (p == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    palette <- scale_color_manual(values = custom)
  } else if (p %in% ggsci_palette_names) {
    palette <- eval(parse(text = sprintf("ggsci::scale_color_%s()", p)))
  } else if (p %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, p)
    colors <- rev(colorRampPalette(ref)(500))
    palette <- scale_colour_manual(values = colors)
  } else if (p == "cb") {
    palette <- scale_color_manual(values = cb_palette)
  } else if (p == "random") {
    set.seed(Sys.time())
    palette <- scale_color_manual(values = randomcoloR::distinctColorPalette(500))
  } else if (p == "rainbow") {
    palette <- scale_color_manual(values = rainbow(500))
  } else if (p %in% prism_palette_names) {
    palette <- scale_color_manual(values = ggprism::ggprism_data$colour_palettes[[p]])
  } else if (p %in% graf_palette_names) {
    palette <- eval(parse(text = sprintf("grafify::scale_color_%s()", p)))
  } else {
    palette <- scale_color_discrete()
  }
  return(palette)
}
## get ggsci colors
get_hiplot_color <- function(p, n = -1, custom = NULL) {
  # this function extract the colors from ggsci package
  # p: palette
  # n: number of colors

  # npg,aaas,nejm,lancet,jama,jco,ucscgb,d3,locuszoom,
  # igv,uchicago,startrek,tron,futurama,rickandmorty,simpsons
  if (p == "null") {
    return(NULL)
  } else if (!is.null(custom) && length(custom) > 0) {
    custom <- custom_color_filter(custom)
    col <- custom
  } else if (p %in% ggsci_palette_names) {
    plen <- ggsci_palette_length[which(p == ggsci_palette_names)]
    col <- eval(parse(text = sprintf("ggsci::pal_%s()(%s)", p, plen)))
  } else if (p %in% brewer_palette_names) {
    ref <- RColorBrewer::brewer.pal(6, p)
    col <- rev(colorRampPalette(ref)(500))
  } else if (p == "cb") {
    col <- cb_palette
  } else if (p == "random") {
    set.seed(Sys.time())
    if (n == -1) {
      col <- randomcoloR::distinctColorPalette(500)
    } else {
      col <- randomcoloR::distinctColorPalette(n)
    }
  } else if (p == "rainbow") {
    if (n == -1) {
      col <- rainbow(500)
    } else {
      col <- rainbow(n)
    }
  } else if (p %in% prism_palette_names) {
    col <- ggprism::ggprism_data$colour_palettes[[p]]
  } else if (p %in% graf_palette_names) {
    col <- grafify::graf_col_palette()(30)
  } else {
    col <- gg_color_default(100)
  }
  if (!is.na(n) && n == -1) {
    return(col)
  }
  return(rep(col, 100)[1:n])
}

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

col_fun_cont <- function(x, cols = c(
                           "#196ABD", "#3399FF", "#3399FF", "#f4f4f4", "#f4f4f4",
                           "#f4f4f4", "#FF3333", "#FF3333", "#C20B01"
                         )) {
  colorRamp2(
    get_quant(x),
    cols
  )
}

col_tag <- c("#f4f4f4", "#5a5a5a")

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
