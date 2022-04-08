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

## an update version of brewer.pal in R package RColorBrewer
# This function could generate any number of colors from RColorBrewer package


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
