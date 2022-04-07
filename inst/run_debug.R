initial_options <- commandArgs(trailingOnly = FALSE)
file_arg_name <- "--file="
script_name <- sub(
  file_arg_name, "",
  initial_options[grep(file_arg_name, initial_options)]
)
script_dir <<- dirname(script_name)
ow <- getwd()
renv_dir <- file.path(script_dir, "../../")
if (file.exists(file.path(renv_dir, ".Rprofile"))) {
  setwd(renv_dir)
  source(".Rprofile")
}
setwd(ow)
source(file.path(script_dir, "core.R"))
source(file.path(script_dir, "color.R"))
require(optparse)
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

opt <<- parse_args(OptionParser(option_list = option_list))

if (!opt$simple) {
  source(file.path(script_dir, "lib.R"))
  library_extra_pkgs()
}

outlog_dir <- file.path(dirname(opt$outputFilePrefix), "log")
if (!dir.exists(outlog_dir)) {
  dir.create(outlog_dir)
}
run_hiplot()
