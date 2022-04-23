#' Functions and utilities to export hiplot output
#' @name export
#' @rdname export
NULL

#' @describeIn export Export a single ggplot object
#' @param x a plot object to export.
#' @export
export_single <- function(x) {
  conf <- globs_get("conf")
  opt <- globs_get("opt")

  asp <- conf$general$size$width / conf$general$size$height
  if ("htmlwidget" %in% class(x)) {
    return(export_htmlwidget(x))
  }
  for (i in conf$general$imageExportType) {
    outfn <- sprintf("%s.%s", opt$outputFilePrefix, i)
    if (i == "pptx" || i == "ppt") {
      export_pptx(x, outfn, conf$general$size$width, conf$general$size$height)
    } else if (i == "docx") {
      graph2doc(x,
        file = outfn, width = conf$general$size$width,
        height = conf$general$size$height, append = TRUE
      )
    } else if (i == "plotly") {
      outfn <- sprintf("%s.plotly.html", opt$outputFilePrefix)
      export_plotly(x, outfn)
    }
  }
  if (any(c("png", "svg", "eps", "jpeg", "svg", "pdf") %in% conf$general$imageExportType)) {
    if (is.null(conf$general$font)) conf$general$font <- "Arial"
    outpdf <- sprintf("%s.pdf", opt$outputFilePrefix)
    cowplot::save_plot(x,
      filename = outpdf,
      base_height = conf$general$size$height,
      base_asp = asp,
      family = conf$general$font,
      device = cairo_pdf,
      limitsize = FALSE
    )
    pdf2image(outpdf)
    if (!"pdf" %in% conf$general$imageExportType) {
      file.remove(outpdf)
    }
  }
}

#' @describeIn export Export a single ggplot object to PPTx
#' @param outfn output file path.
#' @param width width of output pptx.
#' @param height height of output pptx.
#' @export
export_pptx <- function(x, outfn, width, height) {
  step3 <- function(e) {
    warning("Not support PPTX export yet.")
  }
  step2 <- function(e) {
    tryCatch(
      graph2ppt(ggdraw(x),
        file = outfn,
        width = width, height = height,
        append = TRUE
      ),
      error = step3
    )
  }
  tryCatch(
    graph2ppt(x,
      file = outfn, width = width,
      height = height, append = TRUE
    ),
    error = step2
  )
}

#' @describeIn export Export a single ggplot object to plotly
#' @export
export_plotly <- function(x, outfn) {
  tryCatch(
    {
      saveWidget(ggplotly(x), outfn)
    },
    error = function(e) {
      warning("Not support Plotly export yet.")
    }
  )
}

#' @describeIn export Export hiplot plugin output directory to a zipped file
#' @param outdir output directory.
#' @param pdf_tb_subdir directory path storing pdf (figure) files.
#' @param recursive same path as pdf_tb_subdir to specify if run with recursive mode.
#' @param sheetname_replace all .csv files are re-exported into a excel file,
#' if necessary, use this option remove some string as sheet names.
#' @export
export_directory <- function(outdir = "output", pdf_tb_subdir = c("output", "output/results"),
                             recursive = c(FALSE, TRUE),
                             sheetname_replace = "_BayesianNMF|legacy_fitting_|_fitting|.csv") {
  conf <- globs_get("conf")

  basedir <- dirname(opt$outputFilePrefix)
  if (!str_detect(outdir, basedir)) {
    outdir <- sprintf("%s/%s", basedir, outdir)
  }
  pdfs <- c()
  tb <- c()
  for (i in 1:length(pdf_tb_subdir)) {
    if (length(recursive) == 1) {
      recursive_tmp <- recursive
    } else {
      recursive_tmp <- recursive[i]
    }
    pdfs <- c(pdfs, list.files(sprintf("%s/%s", basedir, pdf_tb_subdir[i]),
      recursive = recursive_tmp,
      ".pdf", full.names = TRUE
    ))
    tb <- c(tb, list.files(sprintf("%s/%s", basedir, pdf_tb_subdir[i]),
      recursive = recursive_tmp,
      ".txt$|.csv$|.tsv$", full.names = TRUE
    ))
  }
  if (length(pdfs) > 0) {
    pdfs2image(pdfs)
  }
  if (length(tb) > 0) {
    wb <- createWorkbook()
    sheetnames <- c()
    for (i in 1:length(tb)) {
      sheetname <- sub("\\..[^\\.]*$", "", basename(tb[i]))
      sheetname <- str_replace_all(
        sheetname,
        sheetname_replace, ""
      )
      if (str_length(sheetname) > 31) {
        sheetname_tmp <- str_sub(sheetname, 1, 29)
        if (sheetname_tmp %in% sheetnames) {
          sheetname_tmp <- paste0(sheetname_tmp, tb[i])
        }
        sheetname <- sheetname_tmp
      }
      sheetnames <- c(sheetnames, sheetname)
      addWorksheet(wb, sheetname)
      writeData(wb, sheetname, fread(tb[i]), colNames = TRUE, rowNames = FALSE)
    }
    out_xlsx <- paste(opt$outputFilePrefix, ".xlsx", sep = "")
    saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  }
  if (dir.exists(outdir)) {
    Sys.sleep(2)
    owd <- getwd()
    setwd(outdir)
    system_safe(sprintf(
      "tar -czv * --warning=no-file-changed -f %s.addition.tar.gz",
      opt$outputFilePrefix
    ))
    setwd(owd)
    unlink(outdir, recursive = TRUE)
  }
}

#' @describeIn export Export a single ggplot object to html file
#' @export
export_htmlwidget <- function(x) {
  if ("wordcloud2" %in% class(x)) {
    temp_html <- sprintf("%s.htmlwidget.html", tempfile())
    saveWidget(x, temp_html, selfcontained = FALSE)
    for (i in conf$general$imageExportType) {
      outfn <- sprintf("%s.%s", opt$outputFilePrefix, i)
      webshot::webshot(temp_html, outfn, delay = 5)
    }
  } else {
    temp_html <- sprintf("%s.htmlwidget.html", opt$outputFilePrefix)
    saveWidget(x, temp_html)
  }
}

#' @describeIn export Convert html to pdf
#' @param inhtml input html file.
#' @param outpdf output pdf file.
#' @export
html2pdf <- function(inhtml, outpdf) {
  system(sprintf(
    paste0(
      "xvfb-run google-chrome --no-sandbox",
      " --disable-setuid-sandbox --headless --disable-gpu --no-margins",
      " --print-to-pdf-no-header --print-to-pdf-no-footer",
      " --run-all-compositor-stages-before-draw --print-to-pdf=%s %s"
    ),
    outpdf, inhtml
  ))
}

#' @describeIn export Convert pdf to images
#' @param pdf input pdf file.
#' @param i a postfix added to the output file name.
#' @export
pdf2image <- function(pdf, i = "") {
  for (tp in conf$general$imageExportType) {
    if (tp %in% c("png", "svg", "eps", "jpeg")) {
      cmd <- sprintf(
        paste(
          "unset LD_LIBRARY_PATH; unset LIBRARY_PATH;",
          "pdftocairo %s",
          "-%s '%s' '%s%s%s'"
        ),
        ifelse(tp %in% c("png", "jpeg"), "-r 300", ""),
        tp, pdf, opt$outputFilePrefix,
        ifelse(i != "", paste0("-", i), ""),
        ifelse(tp %in% c("svg", "eps"), paste0(".", tp), "")
      )
      system(cmd)
    }
  }
  if ("tiff" %in% conf$general$imageExportType ||
    "tif" %in% conf$general$imageExportType) {
    cmd <- sprintf(
      paste(
        "unset LD_LIBRARY_PATH; unset LIBRARY_PATH;",
        "pdftocairo -tiff -tiffcompression lzw",
        "-r 300 '%s' '%s%s'"
      ),
      pdf, opt$outputFilePrefix,
      ifelse(i != "", paste("-", i), "")
    )
    system(cmd)
  }
}

#' @describeIn export Convert multiple pdfs to images
#' @param pdfs input pdf files.
#' @export
pdfs2image <- function(pdfs) {
  if ("pdf" %in% conf$general$imageExportType) {
    out_pdf <- paste(opt$outputFilePrefix, ".pdf", sep = "")
    if (length(pdfs) == 1) {
      file.copy(pdfs[1], out_pdf)
    } else {
      merge_pdfs(pdfs, out_pdf)
    }
  }
  for (i in seq_len(length(pdfs))) {
    pdf2image(pdfs[i], i)
  }
}

#' @describeIn export Merge multiple pdf files into one
#' @export
merge_pdfs <- function(pdfs, outpdf) {
  cmds <- sprintf(
    "pdfcpu merge '%s' '%s'",
    outpdf, paste0(pdfs, collapse = "' '")
  )
  system(cmds)
}

#' @describeIn export Split multiple page pdfs to a directory
#' @param pdfs input pdf files.
#' @export
split_pdfs <- function(pdfs, outdir) {
  for (i in pdfs) {
    cmds <- sprintf("pdfcpu split '%s' '%s'", i, outdir)
    system(cmds)
  }
  return(list.files(outdir, ".pdf"))
}

#' @describeIn export Export non-pdf images as pdfs
#' @export
import_images_to_pdf <- function(pdfs) {
  ret <- c()
  all_tmp_pdf <- c()
  for (i in pdfs) {
    if (tolower(xfun::file_ext(i)) != "pdf") {
      tmp_pdf <- paste0(tempfile(), ".pdf")
      all_tmp_pdf <- c(all_tmp_pdf, tmp_pdf)
      system(sprintf(
        "pdfcpu import 'dpi:3000, pos:full' '%s' '%s'",
        tmp_pdf, i
      ))
      ret <- c(ret, tmp_pdf)
    } else {
      ret <- c(ret, i)
    }
  }
  return(list(ret = ret, all_tmp_pdf = all_tmp_pdf))
}
