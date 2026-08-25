pkgs <- readLines("pkgs")

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
#BiocManager::install("hfang-bristol/supraHex", Ncpus = 3)
#BiocManager::install("hfang-bristol/dnet", Ncpus = 3)
BiocManager::install("hiplot/hiplotlib", Ncpus = 3)

#pkgs2 <- readLines("pkgs2")
#remotes::install_url(pkgs2)
#install.packages("numDeriv")

#options(repos=c(getOption("repos"), "https://ccb.nki.nl/software/discover/repos/r"))
#install.packages("discover")

for (start in c(0, 300, 600, 900, 1200)) {
#for (start in c(800)) {
BiocManager::install(pkgs[(1 + start):(50+start)], Ncpus = 3)
BiocManager::install(pkgs[(51+start):(100+start)], Ncpus = 3)
BiocManager::install(pkgs[(101+start):(150+start)], Ncpus = 3)
BiocManager::install(pkgs[(151+start):(200+start)], Ncpus = 3)
BiocManager::install(pkgs[(201+start):(250+start)], Ncpus = 3)
BiocManager::install(pkgs[(251+start):(300+start)], Ncpus = 3)
}

remotes::install_github("ShixiangWang/DoAbsolute")
path_to_file = system.file("extdata", "ABSOLUTE_1.0.6.tar.gz", package = "DoAbsolute", mustWork = T)
install.packages(path_to_file, repos = NULL, type="source")
