initSteps <- function() {
  # read in configuration file
  conf <- globs_get("conf")
  stopifnot(!is.null(conf))
  if (!is.null(conf$tool)) {
    conf <- conf$params$config
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
  globs_set("conf")
}

checkExample <- function() {
  conf_raw <- conf <- globs_get("conf")
  if (is.null(conf_raw$exampleData$config)) {
    conf_raw$exampleData <- conf_raw$exampleData[[1]]
  }
  if (is.logical(opt$enableExample) && opt$enableExample) {
    for (i in names(conf_raw$exampleData$config)) {
      for (j in names(conf_raw$exampleData$config[[i]])) {
        conf$params$config[[i]][[j]] <- conf_raw$exampleData$config[[i]][[j]]
      }
    }
    globs_set("conf")
    update_task_status(conf)
    for (i in 1:length(conf_raw$exampleData$textarea)) {
      tmp <- fread(conf_raw$exampleData$textarea[[i]], data.table = FALSE)
      print(head(tmp))
      if (i == 1) {
        globs_set("tmp", name = "data")
      } else {
        globs_set("tmp", name = paste0("data", i))
      }
    }
  }
}

format_conf_opt <- function () {
  if (is.null(conf$general$font)) {
    conf$general$font <- "Arial"
  }
  # convert old config
  ref <- c(font_family = "font",
    family = "font",
    digets = "digits",
    title_size = "titleSize",
    axis_title_size = "axisTitleSize",
    legend_pos = "legendPos", legend_dir = "legendDir",
    legend_title_size = "legendTitleSize",
    legendTextSize = "legendTextSize",
    x_axis_angle = "xAxisTextAngle",
    hjust = "xAxisHjust",
    vjust = "xAxisVjust",
    fontsize_row = "fontsizeRow",
    fontsize_col = "fontsizeCol",
    digits = "digits")

  for (i in names(ref)) {
    if (i %in% names(conf$extra)) {
      conf$general[[ref[i]]] <- conf$extra[[i]]
      conf$extra[[i]] <- NULL
    }
  }
  ref2 <- c(palette_cont = "paletteCont")
  for (i in names(ref2)) {
    if (i %in% names(conf$general)) {
      if (!is.null(conf$general[[ref2[i]]])) {
        conf$general[[ref2[i]]] = conf$general[[i]]
        conf$general[[i]] <- NULL
      }
    }
  }
  globs_set("conf")
  globs_set("opt")
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
            globs_set("tmp", "data")
          } else {
            globs_set("tmp", paste0("data", i))
          }
        }
      }
    }
    globs_set("conf")
  })
}

print_sessioninfo <- function() {
  session_info <- as.character(sessioninfo::session_info())
  session_info <- sapply(session_info, function(x) {
    str_replace_all(x, "/cluster/home/public/opt|/cluster/apps/hiplot|/cluster/home", "")
  })
  cat(session_info, sep = "\n")
}

start_task <- function() {
  format_conf_opt()
  sourceR <- sprintf("%s/%s/source.R", script_dir, opt$tool)
  loadSourceR <- file.exists(sourceR)
  if (loadSourceR) source(sourceR)
  entry <- sprintf("%s/%s/%s.R", script_dir,
    opt$tool, c("plot", "start", "entry"))
  entry <- entry[file.exists(entry)]
  if (length(entry) == 0L) stop("No entry found, bad script directory setting!")
  sapply(entry, source)
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
}

eval_parse_codes <- function() {
  script_dir <<- getOption(
    "hiplotlib.script_dir",
    get("script_dir", envir = rlang::global_env())
  )
  conf <<- getOption(
    "hiplotlib.conf",
    get("conf", envir = rlang::global_env())
  )
  if (length(conf$steps$id) == 2) {
    new_task_step("core-steps", "en:Analysis/Plotting;zh_cn:分析/绘图",
      start_task)
  } else {
    start_task()
  }
  new_task_step(
    "done", "en:Sessioninfo (R);zh_cn:运行环境 (R)",
    function() {
      flog.info(sprintf("Task done: %s/%s.", opt$module, opt$tool))
      print_sessioninfo()
    }
  )
  if (!is.null(conf$steps$id) && length(conf$steps$id) > 0) {
    logfile <- file.path(dirname(opt$outputFilePrefix), "task.log")
    if (!file.exists(logfile)) file.create(logfile)
    stepslog <- sprintf("%s/log/%s.log", dirname(opt$outputFilePrefix), conf$steps$id)
    system(sprintf("cat %s >> %s", paste0(stepslog, collapse = " "), logfile))
  }
  vars <- ls(envir = .GlobalEnv)
  out_prefix <- opt$outputFilePrefix
  vars <- vars[vars %in% keep_vars]
  conf$steps <<- NULL
  save(list = vars, file = paste0(out_prefix, ".Rdata"))
}

#' Run hiplot
#' @param opt option list
#' @export
#' @examples
#' \dontrun{
#' basedir = system.file("extdata", "ezcox", package = "hiplotlib")
#' opt = list(inputFile = file.path(basedir, "data.txt"),
#'            confFile = file.path(basedir, "data.json"),
#'            outputFilePrefix = file.path(basedir, "result/test"),
#'            tool = "ezcox",
#'            module = "basic",
#'            simple = FALSE,
#'            enableExample = TRUE,
#'            help = FALSE)
#' dir.create(dirname(opt$outputFilePrefix))
#' dir.create(file.path(dirname(opt$outputFilePrefix), "log"))
#' options(hiplotlib.script_dir = dirname(basedir))
#' run_hiplot()
#' }
run_hiplot <- function(opt = globs_get("opt")) {
  init_vars <<- ls(envir = .GlobalEnv)
  conf <<- read_json(opt$confFile, simplifyVector = F)
  keep_vars <<- c(
    "pkgs",
    paste0("data", 1:10), paste0("dat", 1:10), paste0("res", 1:10),
    paste0("pobj", 1:10000),
    paste0("p", 1:10000), paste0("wb", 1:10000),
    "conf", "data", "p", "wb", "dat", "cem", "res", "pobj"
  )
  tryCatch({
    set_global_options()
    set_general_pkgs()
    checkExample()
    initSteps()
    set_global_confs()
    data_arg_preprocess()
    hiplot_preprocess()
    eval_parse_codes()
  }, error = function(e) {
    vars <- ls(envir = .GlobalEnv)
    vars <- vars[vars != "e"]
    rm(list=vars[!vars %in% init_vars || vars %in% keep_vars])
    stop(e)
  })
  vars <- ls(envir = .GlobalEnv)
  rm(list=vars[!vars %in% init_vars || vars %in% keep_vars])
  gc()
  return("")
}
