suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(hiplotlib))

# initial_options = c("/Library/Frameworks/R.framework/Resources/bin/exec/R", "--no-echo",
#                     "--no-restore", "--file=run.R", "--args", "-c", "extdata/ezcox/data.json",
#                     "-i", "extdata/ezcox/data.txt", "-o", "extdata/ezcox/test", "-t",
#                     "extdata/ezcox", "--enableExample")
initial_options <- commandArgs(trailingOnly = FALSE)

file_arg_name <- "--file="
script_name <- sub(
  file_arg_name, "",
  initial_options[grep(file_arg_name, initial_options)]
)
script_dir <<- dirname(script_name)

ow <- getwd()
renv_dir <- file.path(script_dir, "../../") # TODO 这个用一个option去控制是否更好
if (file.exists(file.path(renv_dir, ".Rprofile"))) {
  setwd(renv_dir)
  source(".Rprofile")
}
setwd(ow)

# options
option_list <- list(
  make_option(c("-i", "--inputFile"),
    default = "",
    help = "input data"
  ),
  make_option(c("-c", "--confFile"),
    default = "",
    help = "configuration file"
  ),
  make_option(c("-o", "--outputFilePrefix"),
    default = "",
    help = "prefix of output plots"
  ),
  make_option(c("-t", "--tool"),
    default = "heatmap",
    help = "configuration file (e.g. heatmap))"
  ),
  make_option(c("-m", "--module"),
    default = "basic",
    help = "module name (e.g. basic)"
  ),
  make_option(c("-s", "--simple"),
    default = FALSE,
    action = "store_true",
    help = "only source core.R"
  ),
  make_option(c("-e", "--enableExample"),
    default = FALSE,
    action = "store_true",
    help = "enable auto load example"
  )
)

opt <- parse_args(OptionParser(option_list = option_list))
# opt = list(inputFile = "inst/extdata/ezcox/data.txt", confFile = "inst/extdata/ezcox/data.json",
#            outputFilePrefix = "inst/extdata/ezcox/test", tool = "inst/extdata/ezcox",
#            module = "basic", simple = FALSE, enableExample = TRUE, help = FALSE)

if (!opt$simple) {
  #source(file.path(script_dir, "lib.R"))
  set_extra_pkgs()
}

outlog_dir <- file.path(dirname(opt$outputFilePrefix), "log")
if (!dir.exists(outlog_dir)) {
  dir.create(outlog_dir)
}
run_hiplot()
