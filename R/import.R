#' Functions and utilities related to data import
#' @name import
#' @rdname import
NULL

#' @describeIn export Read common format data into data frame
#' @param path the file path to data.
#' @param format import data format.
#' @param header if `TRUE`, tread input having a header.
#' @param ... other parameters passing to [data.table::fread] when use
#' this function to import inside.
#' @export
read_data <- function(path,
                      format = c("csv", "excel", "rds", "rdata"),
                      header = TRUE, ...) {
  stopifnot(length(path) == 1, is.character(path))
  format <- match.arg(format)
  if (any(tolower(xfun::file_ext(path)) == "rdata", format == "rdata")) {
    # RData
    load(path)
  } else if (any(tolower(xfun::file_ext(path)) == "rds", format == "rds")) {
    # RDS
    readRDS(path)
  } else if (any(tolower(xfun::file_ext(path)) %in%
    c("xlsx", "xls"), format == "excel")) {
    # Excel
    readxl::read_excel(path, ...) %>%
      as.data.frame()
  } else if (any(tolower(xfun::file_ext(path)) %in%
    c("txt", "csv", "tsv", "csv.gz", "txt.gz"), format == "csv")) {
    # CSV and its variant
    # Here I assume gzipped file are *.tsv.gz or *.csv.gz
    if (tolower(xfun::file_ext(path)) == "csv" ||
      tolower(xfun::file_ext(path)) == "csv.gz") {
      sep <- ","
    } else {
      sep <- "\t"
    }
    fread(path,
      header = header,
      sep = sep,
      data.table = FALSE,
      fill = TRUE, ...
    )
  }
}

#' @describeIn export Parse the true file path from link
#' @param link the file link
#' @export
#' @examples
#' # parse_file_link("hiplot.org/path=xxx") TODO
parse_file_link <- function(link) {
  link <- str_replace_all(link, ".*?path=", "")
  if (isTRUE(getOption("hiplotlib.debug"))) {
    return(link)
  } else {
    x <- paste0(get("upload_dir", envir = .GlobalEnv), "/", link)
    x <- str_replace_all(x, "//", "/")
    return(x)
  }
}
