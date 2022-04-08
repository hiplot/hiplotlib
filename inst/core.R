pacman::p_load(optparse)
pacman::p_load(jsonlite)
pacman::p_load(data.table)
pacman::p_load(export) # tomwenseleers/export
pacman::p_load(openxlsx)
pacman::p_load(xfun)
pacman::p_load(stringr)
pacman::p_load(callr)
pacman::p_load(futile.logger)

transform_val <- function(func_str, val) {
  if (is.character(func_str) && func_str != "") {
    if (any(sapply(c("+", "-", "*", "/", "%"), function(x) {
      str_detect(func_str, fixed(x))
    }))) {
      val <- eval(parse(text = sprintf("val%s", func_str)))
    } else {
      val <- eval(parse(text = sprintf("%s(val)", func_str)))
    }
  }
  return(val)
}

export_single <- function(x) {
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

export_plotly <- function(x, outfn) {
  pacman::p_load(plotly)
  pacman::p_load(htmlwidgets)
  tryCatch(
    {
      saveWidget(ggplotly(x), outfn)
    },
    error = function(e) {
      warning("Not support Plotly export yet.")
    }
  )
}

export_directory <- function(outdir = "output", pdf_tb_subdir = c("output", "output/results"),
                             recursive = c(FALSE, TRUE),
                             sheetname_replace = "_BayesianNMF|legacy_fitting_|_fitting|.csv") {
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
    owd <- getwd()
    setwd(outdir)
    system_safe(sprintf(
      "tar -czv * -f %s.addition.tar.gz",
      opt$outputFilePrefix
    ))
    setwd(owd)
    unlink(outdir, recursive = TRUE)
  }
}


export_htmlwidget <- function(x) {
  pacman::p_load(htmlwidgets)
  pacman::p_load(webshot)
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

parse_file_link <- function(link) {
  link <- str_replace_all(link, ".*?path=", "")
  return(file.path(get("upload_dir", envir = .GlobalEnv), link))
}

get_quant <- function(x) {
  return(
    c(
      min(x),
      quantile(x, 0.03),
      quantile(x, 0.45),
      median(x),
      quantile(x, 0.55),
      quantile(x, 0.97),
      max(x)
    )
  )
}

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
    pacman::p_load(readxl)
    readxl::read_excel(path, ...) %>%
      as.data.frame()
  } else if (any(tolower(xfun::file_ext(path)) %in%
    c("txt", "csv", "tsv", "csv.gz", "txt.gz"), format == "csv")) {
    pacman::p_load(data.table)
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

merge_pdfs <- function(pdfs, outpdf) {
  cmds <- sprintf(
    "pdfcpu merge '%s' '%s'",
    outpdf, paste0(pdfs, collapse = "' '")
  )
  system(cmds)
}

split_pdfs <- function(pdfs, outdir) {
  for (i in pdfs) {
    cmds <- sprintf("pdfcpu split '%s' '%s'", i, outdir)
    system(cmds)
  }
  return(list.files(outdir, ".pdf"))
}

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

system_safe <- function(cmd, print_cmd = FALSE) {
  if (print_cmd) {
    flog.info(str_remove_all(cmd, "/cluster/apps/hiplot/userdata|/cluster/apps/hiplot/web/src/scripts"))
  }
  stderr_fn <- tempfile()
  stdout_fn <- tempfile()
  status <- r_safe(
    function(cmd) {
      system(cmd)
    },
    args = list(cmd),
    stderr = stderr_fn,
    stdout = stdout_fn
  )
  if (status != 0) {
    stop(str_replace_all(
      paste0(readLines(stderr_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ))
  } else {
    cat(str_replace_all(
      paste0(readLines(stdout_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ),
    append = TRUE
    )
    cat(str_replace_all(
      paste0(readLines(stderr_fn), collapse = "\n"),
      get("upload_dir", envir = .GlobalEnv), ""
    ),
    append = TRUE
    )
  }
}

data_arg_preprocess <- function() {
  if (is.null(conf$dataArg) || length(conf$dataArg) == 0) {
    return(NULL)
  }
  for (i in 1:length(conf$dataArg)) {
    for (j in 1:length(conf$dataArg[[i]])) {
      conf$dataArg[[i]][[j]]$blackItems <- NULL
      conf$dataArg[[i]][[j]]$required <- NULL
      conf$dataArg[[i]][[j]]$individual <- NULL
      conf$dataArg[[i]][[j]]$label <- NULL
      conf$dataArg[[i]][[j]]$value <- unlist(conf$dataArg[[i]][[j]]$value)
      conf$dataArg[[i]][[j]]$value[is.null(conf$dataArg[[i]][[j]]$value)] <- ""
    }
  }
  assign("conf", conf, envir = .GlobalEnv)
}

update_task_status <- function(conf) {
  conf <<- conf
  assign("conf", conf, envir = .GlobalEnv)
  outfn <- paste0(dirname(opt$outputFilePrefix), "/task.status.json")
  if (!file.exists(paste0(dirname(opt$outputFilePrefix), "/task.retry"))) {
    outfn <- paste0(dirname(dirname(opt$outputFilePrefix)), "/task.status.json")
  }
  jsonlite::write_json(conf$steps, outfn)
}

new_task_step <- function(id, label, func) {
  log <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), id)
  print(log)
  con <- file(log)
  sink(con, append = TRUE)
  conf$steps$id <- c(conf$steps$id, id)
  conf$steps$label <- c(conf$steps$label, label)
  conf$steps$status <- c(conf$steps$status, 1)
  update_task_status(conf)
  tryCatch(func(), error = function(e) {
    conf$steps$status[length(conf$steps$status)] <- 3
    flog.error(as.character(e))
    update_task_status(conf)
    sink()
    close(con)
    stop(e)
  })
  conf$steps$status[length(conf$steps$status)] <- 2
  update_task_status(conf)
  sink()
  close(con)
}

run_task_step <- function(id, func) {
  log <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), id)
  con <- file(log)
  sink(con, append = TRUE)
  idx <- which(conf$steps$id == id)
  conf$steps$status[idx] <- 1
  update_task_status(conf)
  tryCatch(func(), error = function(e) {
    conf$steps$status[idx] <- 3
    flog.error(as.character(e))
    update_task_status(conf)
    sink()
    close(con)
    stop(e)
  })
  conf$steps$status[idx] <- 2
  update_task_status(conf)
  sink()
  close(con)
}

omitCondition <- function(x) {
  return(all(is.na(x) | x == "" | x == " "))
}

initSteps <- function() {
  # read in configuration file
  if (!is.null(conf$tool)) {
    conf <<- conf$params$config
  }
  id <- c("read-params", "read-data")
  label <- c("en:Parse parameters;zh_cn:解析参数", "en:Read data;zh_cn:读取数据")
  status <- c(0, 0)
  steps <- list(id = id, label = label, status = status)
  if (is.null(conf$steps)) {
    conf$steps <- steps
  } else {
    conf$steps$id <- c(steps$id[1:2], conf$steps$id)
    conf$steps$label <- c(steps$label[1:2], conf$steps$label)
    conf$steps$status <- c(steps$status[1:2], conf$steps$status)
  }
  assign("conf", conf, envir = .GlobalEnv)
}

checkExample <- function() {
  conf_raw <- conf
  if (is.null(conf_raw$exampleData$config)) {
    conf_raw$exampleData <- conf_raw$exampleData[[1]]
  }
  if (is.logical(opt$enableExample) && opt$enableExample) {
    for (i in names(conf_raw$exampleData$config)) {
      for (j in names(conf_raw$exampleData$config[[i]])) {
        conf[[i]][[j]] <<- conf_raw$exampleData$config[[i]][[j]]
      }
    }
    assign("conf", conf, envir = .GlobalEnv)
    update_task_status(conf)
    for (i in 1:length(conf_raw$exampleData$textarea)) {
      tmp <- fread(conf_raw$exampleData$textarea[[i]], data.table = FALSE)
      print(head(tmp))
      if (i == 1) {
        assign("data", tmp, envir = .GlobalEnv)
      } else {
        assign(paste0("data", i), tmp, envir = .GlobalEnv)
      }
    }
  }
}

hiplot_preprocess <- function() {
  run_task_step("read-params", function() {
    flog.info(sprintf("Task started: %s/%s.", opt$module, opt$tool))
    flog.info("Reading parameters......")
    conf_print <- conf
    conf_print$steps <- NULL
    print(str(conf_print))
    update_task_status(conf)
  })

  run_task_step("read-data", function() {
    flog.info("Checking data files...")
    # read in data file1: which is the file with data values
    if (is.character(opt$inputFile)) {
      flog.info("Reading data...")
      if (str_detect(opt$inputFile, ",")) {
        file.list <- str_split(opt$inputFile, ",")[[1]]
      } else {
        file.list <- opt$inputFile
      }
      for (i in seq_len(length(file.list))) {
        if (file.exists(file.list[i]) && file.size(file.list[i]) > 0) {
          tmp <- as.data.frame(read_data(file.list[i]))
          fil <- apply(tmp, 1, function(x) {
            omitCondition(x)
          })
          fil2 <- apply(tmp, 2, function(x) {
            omitCondition(x)
          })
          cnames <- colnames(tmp)
          tmp <- tmp[!fil, ]
          if (!is.data.frame(tmp)) {
            tmp <- as.data.frame(tmp)
            colnames(tmp) <- cnames
          }
          tmp <- tryCatch(
            {
              tmp[!fil2]
            },
            error = function(e) {
              tmp
            }
          )
          print(head(tmp))
          if (i == 1) {
            assign("data", tmp, envir = .GlobalEnv)
          } else {
            assign(paste0("data", i), tmp, envir = .GlobalEnv)
          }
        }
      }
    }
    conf <<- conf
  })
}

print_sessioninfo <- function() {
  session_info <- as.character(sessioninfo::session_info())
  session_info <- sapply(session_info, function(x) {
    str_replace_all(x, "/cluster/home/public/opt|/cluster/apps/hiplot", "")
  })
  cat(session_info, sep = "\n")
}

eval_parse_codes <- function() {
  conf <<- get("conf", envir = .GlobalEnv)
  keep_vars <<- c(
    "pkgs",
    paste0("data", 1:10), paste0("dat", 1:10), paste0("res", 1:10),
    paste0("pobj", 1:10000),
    paste0("p", 1:10000), paste0("wb", 1:10000),
    "conf", "data", "p", "wb", "dat", "cem", "res", "pobj"
  )
  start_task <- function() {
    sourceR <- sprintf("%s/%s/source.R", script_dir, opt$tool)
    loadSourceR <- file.exists(sourceR)
    source(sprintf("%s/head.R", script_dir))
    if (loadSourceR) source(sourceR)
    entry <- sprintf("%s/%s/%s.R", script_dir, opt$tool, c("plot", "start", "entry"))
    entry <- entry[file.exists(entry)]
    sapply(entry, source)
  }
  if (length(conf$steps$id) == 2) {
    new_task_step("core-steps", "en:Analysis/Plotting;zh_cn:分析/绘图", start_task)
  } else {
    start_task()
  }
  source(sprintf("%s/foot.R", script_dir))
  if (!is.null(conf$steps$id) && length(conf$steps$id) > 0) {
    logfile <- file.path(dirname(opt$outputFilePrefix), "task.log")
    if (!file.exists(logfile)) file.create(logfile)
    stepslog <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), conf$steps$id)
    system(sprintf("cat %s >> %s", paste0(stepslog, collapse = " "), logfile))
  }
  tmp_pdfs <- list.files(
    dirname(opt$outputFilePrefix),
    "Rplots[0-9]*.pdf",
    full.names = TRUE
  )
  tmp_pdfs2 <- list.files(
    ".",
    "Rplots[0-9]*.pdf",
    full.names = TRUE
  )
  if (length(tmp_pdfs) > 0) file.remove(tmp_pdfs)
  if (length(tmp_pdfs2) > 0) file.remove(tmp_pdfs2)
  vars <- ls(envir = .GlobalEnv)
  out_prefix <- opt$outputFilePrefix
  vars <- vars[vars %in% keep_vars]
  conf$steps <<- NULL
  save(list = vars, file = paste0(out_prefix, ".Rdata"))
  rm(list = vars, envir = .GlobalEnv)
}

run_hiplot <- function() {
  conf <<- read_json(opt$confFile, simplifyVector = F)
  checkExample()
  initSteps()
  data_arg_preprocess()
  hiplot_preprocess()
  eval_parse_codes()
  return("")
}
