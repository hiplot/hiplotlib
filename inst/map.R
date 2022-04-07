draw_map <- function(rds, keyname, filter_names = NULL) {
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
