#' hiplotlib package
#'
#' @description
#' Infrastructure functions for Hiplot plot system.
#'
#' @seealso
#' Useful links:
#'
#' \url{https://github.com/hiplot/hiplotlib}
#'
#' Report bugs at \url{https://github.com/hiplot/hiplotlib/issues}
#' @docType package
#' @name hiplotlib
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom gplots colorpanel
#' @importFrom grDevices col2rgb rgb colorRampPalette rainbow hcl cairo_pdf
#' @importFrom stats na.omit median quantile
#' @importFrom utils packageDescription head str packageVersion
#' @importFrom stringr fixed str_split str_replace_all str_detect str_length str_remove_all str_sub
#' @importFrom grid grid.rect gpar
#' @importFrom data.table data.table fread setkey
#' @importFrom jsonlite read_json
#' @importFrom futile.logger flog.error flog.info
#' @importFrom openxlsx createWorkbook addWorksheet saveWorkbook writeData
#' @importFrom export graph2doc graph2ppt
#' @importFrom cowplot ggdraw
#' @importFrom plotly ggplotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom callr r_safe
#' @import ggplot2 R.utils optparse patchwork uuid
NULL
