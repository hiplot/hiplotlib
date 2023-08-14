# hiplotlib

[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fhiplot%2Fhiplotlib&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=hits&edge_flat=false)](https://hits.seeyoufarm.com)

Infrastructure for Hiplot native plugin system.

## Installation

Install from GitHub:

```r
remotes::install_github("hiplot/hiplotlib")
```

## Docs

- Check R package documentation for usage of functions.
- Check [hisub_doc.md](hisub_doc.md) for documentation of HiSub to develop and submit a Hiplot native plugin.

## Docker

Hiplot Doker image is now available to use `hisub` and `hicli` to build and run hiplot plugin, It is built based on [rocker/r-ver:4.3](https://hub.docker.com/r/rocker/r-ver/tags)

**Quick Start （base on vrsion`0.2.1-1`）: **

* Build Images : `make docker_build` It takes 20-40 minutes to build, and then you can use `docker images hiplotlib` to check it was success
* Run a new Containers : `docker run -itd -n hiplotlib hiplotlib:0.2.1-1 ` It will start a new containers named hiplotlib in the background, if you nedd to run your own code in this containers, you can use params `-v /yourCode/filepath:/app/containerPath` to mapping your host file into container 
* Entering the container: `docker exec -it hiplotlib bash`, now you are in the interior of the container, you can try to use  `hicli` and `hisub` command

## Maintainer

Hiplot team

## Copyright

&copy; 2020-2023 Hiplot team
