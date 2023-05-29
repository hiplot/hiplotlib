options(warn = -1)
host_port <- commandArgs(TRUE)
ver <- ""
if (length(host_port) == 1) {
  host <- "127.0.0.1"
  port <- host_port
} else if (length(host_port) == 2) {
  host = host_port[2]
  port = host_port[1]
} else if (length(host_port) == 3) {
  host = host_port[2]
  port = host_port[1]
  ver = host_port[3]
}
if (ver != "") {
  ver <- paste0('.', ver)
}
print(host)
print(port)
print(ver)
library(plumber)
library(hiplotlib)
source("../basic/lib.R")
library_extra_pkgs()

pid <- Sys.getpid()
wd <- getwd()
print(wd)
pool <- sprintf("/app/hiplot/data/plumber/active.pool%s", ver)
print(pool)
if (file.exists(pool)) {
  active_pool_raw <- readLines(pool)
  active_pool <- stringr::str_split(active_pool_raw, ":")
  pre_port <- sapply(active_pool, function(x) x[3])
  if (port %in% pre_port) {
    idx <- which(port == sapply(active_pool, function(x) x[3]))
    file.remove(sprintf("/app/hiplot/data/plumber/locks/%s", sapply(active_pool, function(x) x[1])[idx]))
    active_pool_raw[idx] <- sprintf("%s:%s:%s", pid, host, port)
    writeLines(active_pool_raw, pool)
  } else {
    cat(sprintf("%s:%s:%s", pid, host, port),
      file = pool,
      append = TRUE, sep = "\n"
    )
  }
} else {
  cat(sprintf("%s:%s:%s", pid, host, port),
    file = pool,
    append = TRUE, sep = "\n"
  )
}

# 'plumber.R' is the location of the file shown above
pr("plumber.R") %>%
  pr_run(port = as.numeric(port), host = "0.0.0.0")
  
