pkgs <- readLines("pkgs")

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("hiplot/hiplotlib", Ncpus = 1)

pkgs2 <- readLines("pkgs2")
remotes::install_url(pkgs2)

options(repos=c(getOption("repos"), "https://ccb.nki.nl/software/discover/repos/r"))
install.packages("discover")

start=0
#start=300
#start=600
#start=900
BiocManager::install(pkgs[(1 + start):(100+start)], Ncpus = 1)
BiocManager::install(pkgs[(101+start):(200+start)], Ncpus = 1)
BiocManager::install(pkgs[(201+start):(300+start)], Ncpus = 1)

install.packages("numDeriv")
remotes::install_github("ShixiangWang/DoAbsolute")
path_to_file = system.file("extdata", "ABSOLUTE_1.0.6.tar.gz", package = "DoAbsolute", mustWork = T)
install.packages(path_to_file, repos = NULL, type="source")
