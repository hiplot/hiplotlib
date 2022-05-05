#!/usr/bin/env Rscript
# Hiplot 插件提交器
# Copyright @ 2021-2022 Hiplot team
# 工作原理：
# 1. 读入文件
# 2. 移除无关行
# 3. 为每一个 tag 创建一个对应的解析函数
# 4. 解析所有内容并整理输出，为生成 .json 配置提供内容
# 5. 生成 .json (data.json, meta.json, ui.json)
# 6. 基于配置和输入文件生成 plot.R
#
# ./hisub.R examples/helloworld.R test_hello
VERSION <- 0.4

TEMPLATE <- '# @hiplot start
# @appname helloworld
# @alias An-Example
# @apptitle
# Hiplot Hello World
# Hiplot 示例插件
# @target basic
# @tag test dotplot
# @author your name
# @url your project link
# @citation any reference you should link to
# @version 0.1.0
# @release 2021-01-01
# @description
# en: One sentence to describe this plugin.
# zh: 插件一段话简单介绍。
# @main helloworld
# @library ggplot2 readr
# @param data export::data::hiplot-textarea::{"default": "data.txt", "required": true}
# en: Data Table
# zh: 数据表
# @param x export::dataArg::data::{"index":1, "default": "mpg", "required": true}
# en: X Axis Variable
# zh: X 轴变量
# @param y export::dataArg::data::{"index":2, "default": "vs", "blackItems": "carb", "required": false}
# en: X Axis Variable
# zh: Y 轴变量
# @param size export::extra::slider::{"default":2, "min":0.5, "max":5, "step":0.5, "class":"col-12"}
# en: Dot Size
# zh: 点大小
# @param add_line export::extra::switch::{"default": true, "class":"col-12"}
# en: Add Line
# zh: 添加线图
# @return ggplot::["pdf", "png"]::{"cliMode": true, "title": "A test plot", "width":4, "height": 4, "theme": "theme_bw"}
# @data
# # You can write the code to generate the example data
# # "data.txt" described in parameter data, or you can
# # omit this tag and submit prepared data files.
# # File size <100Kb is recommended.
# # 此处可以编写生成示例数据（建议小于 100Kb）的代码
# # 示例数据文件需要跟数据表格参数对应起来
# # 或者忽略该标签，提交已经准备好的示例数据
# library(readr)
# data("mtcars")
# write_tsv(mtcars, "data.txt")
# @hiplot end

library(ggplot2)
helloworld <- function(data, x, y, size = 2, add_line = TRUE) {
  if (y == "") stop("y must be provided!")
  p <- ggplot(data, aes_string(x = x, y = y))
  p <- p + geom_point(size = size)
  if (add_line) {
    p <- p + geom_line()
  }
  # Here export a ggplot object
  # Or the whole main function generate a basic R plot
  return(p)
}
'

message("HiSub version ", VERSION)
message("Copyright (c) 2021 Hiplot (https://hiplot.com.cn/)")
message("========================")

message("Checking dependencies...")
if (!require("pacman")) install.packages("pacman")
suppressMessages(pacman::p_load(readr, dplyr, purrr, jsonlite, styler))
message("Done")

message("Checking input...")
Args <- commandArgs(trailingOnly = TRUE)
# Args <- c("test.R", "test-plugin2")

if (length(Args) == 1) {
  if (Args[1] == "template") {
    message("'template' command detected. Generating template 'source.R'.")
    write_lines(TEMPLATE, file = "source.R")
    message("Done")
    quit("no")
  } else {
    if (file.exists(Args[1])) {
      Args[2] <- getwd()
    } else {
      message("The first argument should be 'template' or a R script file path for generating plugin.")
      quit("no", -1)
    }
  }
}

if (length(Args) == 0) {
  message("No operations detected.")
  message("Usage:")
  message("\t`hisub template` to generate a template.")
  message("\t`hisub source.R [...] [outdir]` to convert R script to Hiplot plugin.")
  message("\nDetails see <https://github.com/hiplot/hiplotlib>")
  quit("no", -1)
}

# 如果传入的不是 2 个参数，中间的文件原样拷贝到插件目录以支持
# 已准备好的数据文件或其他所需脚本
fc <- file_content <- read_lines(Args[1])
if (length(Args) > 2) {
  outdir <- Args[length(Args)]
  flag <- TRUE
} else {
  outdir <- Args[2]
  flag <- FALSE
}

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (flag) {
  message("Copying middle files...")
  file.copy(Args[2:(length(Args) - 1)], outdir)
}
message("Done")

# Preprocessing -----------------------------------------------------------

message("Preprocessing R script ", Args[1])
# 过滤无关行
file_content <- file_content[startsWith(file_content, "#")]
file_content <- file_content[
  (grep("# *@hiplot +start", file_content) + 1):(grep("# *@hiplot +end", file_content) - 1)
]

# 分隔标签内容
# src: https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

tag_list <- splitAt(file_content, grep("# *@", file_content))

message("Done")

# Parsing content ---------------------------------------------------------

message("Parsing Hiplot tags...")

# 针对每一个元素解析标签和内容
tag_name <- map_chr(tag_list, ~ sub("# *@([^ ]+).*", "\\1", .[1]))

parse_tag_value <- function(x) sub("# *@[^ ]+ +([^ ]+).*", "\\1", x[1])
parse_tag_header <- function(x) sub("# *@[^ ]+ +", "", x[1])
parse_tag_appname <- function(x) {
  list(type = "appname", value = parse_tag_value(x))
}
parse_tag_alias <- function(x) {
  list(type = "alias", value = parse_tag_header(x))
}
parse_tag_apptitle <- function(x) {
  list(
    type = "apptitle",
    value = list(
      en = trimws(sub("^# *", "", x[2])),
      zh = trimws(sub("^# *", "", x[3]))
    )
  )
}
parse_tag_target <- function(x) {
  list(type = "target", value = parse_tag_value(x))
}

parse_tag_release <- function(x) {
  list(type = "release", value = parse_tag_value(x))
}

parse_tag_tag <- function(x) {
  list(type = "tag", value = unlist(strsplit(parse_tag_header(x), split = " ")))
}
parse_tag_author <- function(x) {
  list(type = "author", value = parse_tag_header(x))
}
parse_tag_email <- function(x) {
  list(type = "email", value = parse_tag_header(x))
}
parse_tag_url <- function(x) {
  value <- parse_tag_header(x)
  if (sub(" ", "", value) == "NULL") {
    value <- NULL
  }
  list(type = "url", value = value)
}

parse_tag_version <- function(x) {
  list(type = "version", value = parse_tag_value(x))
}
parse_tag_citation <- function(x) {
  x[1] <- parse_tag_header(x[1])
  if (startsWith(x[1], "#")) x[1] <- ""
  if (length(x) > 1) {
    x[-1] <- sub("^# *$", "\n", x[-1])
    x[-1] <- sub("^# *", "", x[-1])
  }
  x <- paste(x[x != ""], collapse = "\n")
  list(type = "citation", value = x)
}

parse_doc <- function(x) {
  if (length(x) > 1) {
    x <- sub("^# *$", "\n", x)
    x <- sub("^# *", "", x)
  }
  x <- x[x != ""]
  idx_en <- grep("^en:", x)
  idx_zh <- grep("^zh:", x)

  if (length(idx_zh) == 0) {
    # ALL records are in English
    if (length(idx_en) > 0) {
      x <- gsub("en: *", "", x)
    }
    x_en <- paste(x, collapse = " ")
    x_zh <- ""
  } else if (length(idx_en) > 0) {
    # Both English and Chinese available
    if (idx_en < idx_zh) {
      x_en <- gsub("en: *", "", paste(x[1:(idx_zh - 1)], collapse = " "))
      x_zh <- gsub("zh: *", "", paste(x[idx_zh:length(x)], collapse = " "))
    } else {
      x_zh <- gsub("zh: *", "", paste(x[1:(idx_en - 1)], collapse = " "))
      x_en <- gsub("en: *", "", paste(x[idx_en:length(x)], collapse = " "))
    }
  } else {
    # Only Chinese available
    x <- gsub("zh: *", "", x)
    x_zh <- paste(x, collapse = " ")
    x_en <- ""
  }
  list(
    en = trimws(x_en, "right"),
    zh = trimws(x_zh, "right")
  )
}

parse_tag_description <- function(x) {
  x[1] <- parse_tag_header(x[1])
  if (startsWith(x[1], "#")) x[1] <- ""

  doc_list <- parse_doc(x)
  x_en <- doc_list$en
  x_zh <- doc_list$zh

  list(type = "description", value = list(
    en = x_en,
    zh = x_zh
  ))
}

parse_tag_main <- function(x) {
  list(type = "main", value = parse_tag_value(x))
}
parse_tag_library <- function(x) {
  x[1] <- parse_tag_header(x[1])
  if (length(x) > 1) {
    x[-1] <- sub("^#", "", x[-1])
  }
  x <- paste(x, collapse = " ")
  x <- unlist(strsplit(x, split = " "))

  list(type = "library", value = x)
}

parse_tag_param <- function(x) {
  param_name <- parse_tag_value(x[1])
  if (!grepl("export::", x[1])) {
    return(NULL) # No returns
  }

  header <- trimws(parse_tag_header(x[1]))
  header <- sub("^.*export::", "", header)
  header <- unlist(strsplit(header, "::"))

  doc_list <- parse_doc(x[-1])
  doc_list <- map(doc_list, ~ sub("  ", "", sub("\n", "", .)))

  list(
    type = "param",
    value = list(
      param_type = header[1],
      param_name = param_name,
      widget_type = header[2],
      default_value = jsonlite::fromJSON(header[3]),
      en = doc_list$en,
      zh = doc_list$zh
    )
  )
}

parse_tag_return <- function(x) {
  if (!grepl("::", x[1])) {
    return(list(
      type = "return",
      value = NULL
    ))
  }
  header <- trimws(parse_tag_header(x[1]))
  header <- unlist(strsplit(header, "::"))
  outfmt <- jsonlite::fromJSON(header[2])

  doc_list <- parse_doc(x[-1])
  list(
    type = "return",
    value = list(
      outtype = header[1],
      outfmt = outfmt,
      outsetting = jsonlite::fromJSON(header[3]),
      en = doc_list$en,
      zh = doc_list$zh
    )
  )
}

parse_tag_data <- function(x) {
  x <- sub("^# *", "", x)
  x <- x[!grepl("^@|#", x)]
  x <- paste(x, collapse = "\n")
  list(type = "data", value = x)
}

parse_tag <- function(x, name) {
  switch(
    name,
    appname = parse_tag_appname(x),
    alias = parse_tag_alias(x),
    apptitle = parse_tag_apptitle(x),
    target = parse_tag_target(x),
    tag = parse_tag_tag(x),
    author = parse_tag_author(x),
    email = parse_tag_email(x),
    url = parse_tag_url(x),
    citation = parse_tag_citation(x),
    version = parse_tag_version(x),
    release = parse_tag_release(x),
    description = parse_tag_description(x),
    main = parse_tag_main(x),
    library = parse_tag_library(x),
    param = parse_tag_param(x),
    return = parse_tag_return(x),
    data = parse_tag_data(x)
  )
}

a <- map2(tag_list, tag_name, parse_tag)
names(a) <- tag_name
a <- compact(a)

# 注意有多个参数在 names 中同名
# print(jsonlite::toJSON(a, auto_unbox = TRUE, pretty = TRUE))


# Generate data files -----------------------------------------------------

if ("data" %in% names(a)) {
  message("Generating example data file...")
  old_wd <- getwd()
  setwd(outdir)
  eval(parse(text = a$data$value))
  setwd(old_wd)
  message("Done")
}

# Generate plugin files ---------------------------------------------------
# 标签、参数、控件的设定匹配和设定有难度

message("Generating plugin files...")
# 参数的收集！参数对应的 ui 控件！
set_widget <- function(w) {
  c(list(
    type = w$widget_type,
    label = list(
      en = w$en,
      zh_cn = w$zh
    )
  ), w$default_value[!names(w$default_value) %in% "default"])
}

set_widget_dataArg <- function(w) {
  c(list(
    label = list(
      en = w$en,
      zh_cn = w$zh
    )
  ), w$default_value[!names(w$default_value) %in% "default"])
}

drop_names <- function(x) {
  for (i in seq_along(x)) {
    names(x[[i]]) <- NULL
  }
  x
}

collect_params <- function(x) {
  all_args <- x[names(x) == "param"]
  # 根据参数类型和控件类型生成 data.json 和 ui.json 所需数据
  # 参数类型：data, dataArg(暂时不实现),
  # general(通过return实现，以避免设置函数参数，只设定主题和图片大小), extra
  # 这里实现 data 和 extra 即可
  # 控件类型：hiplot-textarea, select, switch, slider, text-field, ...

  # data.json 需要生成的是默认参数值
  # 一处在 params 里，一处在 exampleData 里
  #
  # ui.json 需要生成的是参数的 ui 配置信息
  # 注释中 default 传入到 data.json，其他参数传入到 ui.json

  params_textarea <- list()
  params_data <- list()
  params_dataArg <- list()
  params_extra <- list()

  example_textarea <- list()
  example_data <- list()
  example_dataArg <- list()
  # example_extra <- list()

  ui_data <- list()
  ui_dataArg <- list()
  ui_extra <- list()

  # 为数据表添加前缀，对应的 dataArg 也需要更改
  j <- 1
  for (i in seq_along(all_args)) {
    if (all_args[[i]]$value$param_type == "data") {
      for (k in seq_along(all_args)) {
        # widget_type
        if (all_args[[k]]$value$param_type == "dataArg") {
          if (all_args[[k]]$value$widget_type == all_args[[i]]$value$param_name) {
            all_args[[k]]$value$widget_type <- paste0(j, "-", all_args[[i]]$value$param_name)
          }
        }
      }
      all_args[[i]]$value$param_name <- paste0(j, "-", all_args[[i]]$value$param_name)
      j <- j + 1
    }
  }

  map(all_args, function(y) {
    y <- y$value
    if (y$param_type == "data") {
      # 如果不是表格数据，就不需要 hiplot-textarea
      if (y$widget_type == "hiplot-textarea") {
        params_textarea[[y$param_name]] <<- ""
        if (!is.null(y$default_value$default)) {
          if (is.list(y$default_value$default)) {
            # 如果用户传入的是 default 字典，提取其 example 项
            fpath <- y$default_value$default$example
          } else {
            fpath <- y$default_value$default
          }
          if (length(fpath) > 0) {
            edata <- read_lines(file.path(outdir, fpath))
            esize <- file.info(file.path(outdir, fpath))$size / 1024 # Kb
            elen <- length(edata) - 1
            message("\tSet example data")
            message("\t data rows: ", elen)
            message("\t esize: ", esize)
            if (esize > 500 & elen > 1) {
              edata <- edata[c(1, sort(sample(2:(elen + 1), round(250 * elen / esize))))]
              message("\t reset esize")
            }
            example_textarea[[y$param_name]] <<- paste(edata, collapse = "\n")
          }
        }
        params_data[[y$param_name]] <<- y$default_value$default[!names(y$default_value$default) %in% "example"]
      } else {
        params_data[[y$param_name]] <<- y$default_value$default
      }

      example_data[[y$param_name]] <<- params_data[[y$param_name]]
      ui_data[[y$param_name]] <<- set_widget(y)
    } else if (y$param_type == "extra") {
      params_extra[[y$param_name]] <<- y$default_value$default
      ui_extra[[y$param_name]] <<- set_widget(y)
    } else if (y$param_type == "dataArg") {
      params_dataArg[[y$widget_type]][[y$param_name]][["value"]] <<- list()
      example_dataArg[[y$widget_type]][[y$param_name]][["value"]] <<- if (is.null(y$default_value$default)) {
        list()
      } else {
        y$default_value$default
      }
      ui_dataArg[[y$widget_type]][[y$param_name]] <<- set_widget_dataArg(y)
    }
    NULL
  })

  # example_dataArg 和 ui_dataArg 可能需要排序
  for (i in names(ui_dataArg)) {
    iord <- order(map_int(ui_dataArg[[i]], "index"))
    ui_dataArg[[i]] <- ui_dataArg[[i]][iord]
    for (j in seq_along(ui_dataArg[[i]])) {
      ui_dataArg[[i]][[j]]$index <- NULL
    }

    if (i %in% names(example_dataArg)) {
      example_dataArg[[i]] <- example_dataArg[[i]][iord]
    }
  }

  list(
    params_textarea = params_textarea,
    params_data = params_data,
    params_dataArg = drop_names(params_dataArg),
    params_extra = params_extra,
    example_textarea = example_textarea,
    example_data = example_data,
    example_dataArg = drop_names(example_dataArg),
    ui_data = ui_data,
    ui_dataArg = drop_names(ui_dataArg),
    ui_extra = ui_extra
  )
}

a$params <- collect_params(a)
# toJSON(list(list(value= list()), list(value=list())), auto_unbox = T)

# meta.json
# Metadata for the plugin
json_meta <- list(
  name = list(zh_cn = a$apptitle$value$zh, en = a$apptitle$value$en),
  alias = if (is.null(a$alias$value)) list(a$appname$value) else list(a$alias$value),
  intro = list(zh_cn = a$description$value$zh, en = a$description$value$en),
  src = "",
  href = paste0("/", a$target$value, "/", a$appname$value),
  tag = c("vue", a$tag$value),
  meta = list(
    score = 3, # default score, change by team member after accept
    author = a$author$value,
    email = if (!is.null(a$email$value)) a$email$value else "",
    issues = if (!is.null(a$url$value)) a$url$value else "",
    releaseDate = if ("release" %in% names(a)) {
      a$release$value
    } else {
      as.character(Sys.Date())
    },
    updateDate = as.character(Sys.Date()),
    citation = if (!is.null(a$citation$value)) a$citation$value else ""
  )
)

message("  - meta.json")
# jsonlite::toJSON(json_meta, auto_unbox = TRUE, pretty = TRUE)
write_json(json_meta, file.path(outdir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)

# data.json
json_data <- list(
  module = a$target$value,
  tool = a$appname$value,
  params = list(
    config = list(
      dataArg = a$params$params_dataArg,
      general = c(
        list(
          cmd = ""
        ),
        a$return$value$outsetting[!names(a$return$value$outsetting) %in% c("width", "height")]
      ),
      # Common extra parameter setting
      extra = a$params$params_extra
    )
  ),
  exampleData = list(
    config = list(
      # general = list(),
      # extra = list()
      dataArg = a$params$example_dataArg
    )
  )
)

shifter <- function(x, n = -1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}


outfmt <- c("pdf", "png", "tiff", "plotly", "pptx")
if (any(a$return$value$outfmt %in% outfmt)) {
  json_data$params$config$general$imageExportType <- a$return$value$outfmt[a$return$value$outfmt %in% outfmt]
}

if (a$return$value$outtype != "directory") {
  json_data$params$config$general$size <- list(
    width = if (length(a$return$value$outsetting$width) == 1) a$return$value$outsetting$width else 6,
    height = if (length(a$return$value$outsetting$height) == 1) a$return$value$outsetting$height else 4
  )
}

if (length(unlist(a$params$params_data)) > 0) {
  json_data$params$config$data <- a$params$params_data
  json_data$params$config <- shifter(json_data$params$config)
}
if (length(unlist(a$params$params_textarea)) > 0) {
  json_data$params$textarea <- a$params$params_textarea
  json_data$params <- shifter(json_data$params)
}

if (length(unlist(a$params$example_data)) > 0) {
  json_data$exampleData$data <- a$params$example_data
  json_data$exampleData <- shifter(json_data$exampleData)
}
if (length(a$params$example_textarea) > 0) json_data$exampleData$textarea <- a$params$example_textarea

message("  - data.json")
# jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)
write_json(json_data, file.path(outdir, "data.json"),
           null = "list", auto_unbox = TRUE, pretty = TRUE
)

# ui.json

json_ui <- list(
  data = a$params$ui_data,
  dataArg = a$params$ui_dataArg,
  # general = list(
  #
  # ),
  extra = a$params$ui_extra
)

message("  - ui.json")
# json_ui <- jsonlite::toJSON(json_ui, auto_unbox = TRUE, pretty = TRUE)
write_json(json_ui, file.path(outdir, "ui.json"),
           null = "list", auto_unbox = TRUE, pretty = TRUE
)

# plot.R
# 保留输入脚本
message("  - plot.R")
if (outdir != ".") write_lines(fc, file.path(outdir, "plot.R"))
# 生成 plot.R 进行调用
args_pairs <- map(
  a[names(a) == "param"],
  ~ c(
    .$value$param_type,
    .$value$param_name,
    .$value$widget_type,
    .$value$default_value$index
  )
)

# 确定 data 的匹配
# 如果开发时数据表使用 data, data2, data3 没有问题
# 但如果用户自定义数据表名，这里只能
# 按顺序生成给 data, data2, ...
# !!后续文档要描述该情况，推荐按函数设定顺序写参数说明
data_idx <- 1
dataArg_idx <- 1 # 如果只有第 2 个数据表格有选择列功能，对应索引应当为 1
args_pairs2 <- c()
for (i in seq_along(args_pairs)) {
  if (args_pairs[[i]][1] == "data") {
    if (data_idx == 1) {
      dat_label <- 'if (exists("data") && is.data.frame(data)) data else ""'
    } else {
      dat_label <- paste0("data", data_idx)
      dat_label <- sprintf('if (exists("%s") && is.data.frame(%s)) %s else ""', dat_label, dat_label, dat_label)
    }
    z <- paste(args_pairs[[i]][2], "=", paste0(dat_label, ","))

    # 补充对应的 dataArg
    idx <- map_chr(args_pairs, 1) == "dataArg" & map_chr(args_pairs, 3) == args_pairs[[i]][2]
    if (any(idx)) {
      z2 <- paste(
        map_chr(args_pairs[idx], 2), "=",
        paste0(
          "conf$dataArg[[",
          dataArg_idx, "]][[",
          map_chr(args_pairs[idx], 4),
          "]]$value,"
        )
      )
      z <- c(z, z2)
      dataArg_idx <- dataArg_idx + 1
    }

    data_idx <- data_idx + 1
  } else if (args_pairs[[i]][1] == "extra") {
    z <- paste(args_pairs[[i]][2], "=", paste0("conf$extra$", args_pairs[[i]][2], ","))
  } else {
    z <- c()
  }
  args_pairs2 <- c(args_pairs2, z)
}
args_pairs2[length(args_pairs2)] <- sub(",", "", args_pairs2[length(args_pairs2)])

plot_r <- c(
  "\n# ====================== Plugin Caller ======================\n",
  paste(
    paste0(a$main$value, "("),
    paste(args_pairs2, collapse = "\n"),
    ")",
    sep = "\n"
  )
)

# 处理非 ggplot 图
if (a$return$value$outtype %in% c("plot", "basic", "grid")) {
  plot_r[2] <- paste(
    "p <- as.ggplot(~",
    plot_r[2],
    ")",
    sep = ""
  )
} else if (a$return$value$outtype == "ggplot") {
  plot_r[2] <- paste0(
    "p <- ",
    plot_r[2]
  )
}

# 增加对图片的配置
if (a$return$value$outtype %in% c("ggplot", "plot")) {
  if (any(c("font", "theme", "palette") %in% names(a$return$value$outsetting))) {
    plot_r <- c(
      plot_r,
      "\np <- set_complex_general_theme(set_palette_theme(p, conf))"
    )
  }

  plot_r <- c(
    plot_r,
    "\nexport_single(p)"
  )

} else if (a$return$value$outtype == "directory") {
  plot_r <- c(
    plot_r,
    "\nexport_directory()"
  )
}

write_lines(plot_r, file.path(outdir, "plot.R"), append = outdir != ".")

style_file(file.path(outdir, "plot.R"))

message("ALL operations done. Check output directory for generated plugin.")
