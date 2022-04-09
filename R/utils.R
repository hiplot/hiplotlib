#' Add alpha to color
#' @param col a color, e.g., 'red'.
#' @param alpha alpha value, range from 0 to 1.
#' @export
#' @examples
#' add_alpha("red", 0.5)
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

#' Get the same characters in two strings
#' @param a,b a string.
#' @param exclude symbol to exclude from comparison.
#' @param ignore_case if `TRUE`, ignore case.
#' @param show_excluded if `TRUE`, show excluded.
#' @param only_position if `TRUE`, only position vector returned.
#' @export
#' @examples
#' list_same_string_position("abcde", "fbcde")
#' list_same_string_position("abcde", "fbcde", only_position = FALSE)
list_same_string_position <- function(a, b,
                                      exclude = c("-", "?"),
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

#' Reset `colnames` of a data frame
#' @param data a `data.frame`
#' @export
#' @examples
#' colname2data(data.frame(A = 1:3, B = LETTERS[1:3]))
colname2data <- function(data) {
  colnames(data) <- paste("V", seq_len(ncol(data)), sep = "")
  return(data)
}

#' Set a vector to a factor
#' @param x a vector.
#' @param rev if `TRUE`, reverse the factor level.
#' @param sort if `TRUE`, sort the vector before setting factor.
#' @export
#' @examples
#' set_factors(c("B", "B", "A", "C"))
#' set_factors(c("B", "B", "A", "C"), rev = TRUE)
#' set_factors(c("B", "B", "A", "C"), sort = TRUE)
set_factors <- function(x, rev = FALSE, sort = FALSE) {
  if (sort) {
    x_uniq <- sort(unique(x))
  } else {
    x_uniq <- unique(x)
  }
  if (rev) {
    x <- factor(x, levels = rev(x_uniq))
  } else {
    x <- factor(x, levels = x_uniq)
  }
  return(x)
}

# TODO: check this
#' Capitalize a string
#' @inheritParams stringr::case
#' @export
#' @examples
#' capitalize("aBcDeF")
capitalize <- stringr::str_to_title

# capitalize <- function(string) {
#   capped <- grep("^[A-Z]", string, invert = TRUE)
#   substr(string[capped], 1, 1) <- toupper(substr(
#     string[capped],
#     1, 1
#   ))
#   return(string)
# }

# Make sure which version is right
# get_quant <- function(x) {
#   return(
#     c(
#       min(x),
#       quantile(x, 0.03),
#       quantile(x, 0.45),
#       median(x),
#       quantile(x, 0.55),
#       quantile(x, 0.97),
#       max(x)
#     )
#   )
# }

get_quant <- function(x) {
  return(
    c(
      min(x, na.rm = T),
      quantile(x, 0.03, na.rm = T),
      quantile(x, 0.07, na.rm = T),
      quantile(x, 0.45, na.rm = T),
      median(x, na.rm = T),
      quantile(x, 0.55, na.rm = T),
      quantile(x, 0.93, na.rm = T),
      quantile(x, 0.97, na.rm = T),
      max(x, na.rm = T)
    )
  )
}
