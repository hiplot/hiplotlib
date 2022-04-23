# hiplotlib

Infrastructure for Hiplot native plugin system.

## Usage

### Examples

Plugin examples to check the package.

- [x] ezcox

```r
basedir = system.file("extdata", "ezcox", package = "hiplotlib")
opt = list(inputFile = file.path(basedir, "data.txt"),
           confFile = file.path(basedir, "data.json"),
           outputFilePrefix = file.path(basedir, "result/test"),
           tool = "ezcox",
           module = "basic",
           simple = FALSE,
           enableExample = TRUE,
           help = FALSE)
dir.create(dirname(opt$outputFilePrefix))
dir.create(file.path(dirname(opt$outputFilePrefix), "log"))
options(hiplotlib.script_dir = dirname(basedir))
run_hiplot()
```

### CLI

CLI Programs:

- [hicli.R](inst/hicli.R) - run hiplot native plugin.
- [hisub.R](inst/hisub.R) - generate hiplot native plugin template and do the conversion.

Deploy the CLIs in R console:

```r
deploy()
```

Check usage in Shell terminal:

```sh
▶ /usr/local/bin/hisub
Using library: /Users/wsx/Library/R
HiSub version 0.3
Copyright (c) 2021 Hiplot (https://hiplot.com.cn/)
========================
Checking dependencies...
Loading required package: pacman
Done
Checking input...
No operations detected.
Usage:
	`hisub template` to generate a template.
	`hisub source.R [...] [outdir]` to convert R script to Hiplot plugin.

Details see <https://github.com/hiplot/hiplot-plugin-generator>                                                                                                                 ⍉
▶ hicli -h
Using library: /Users/wsx/Library/R
Usage: /usr/local/bin/hicli [options]


Options:
	-i INPUTFILE, --inputFile=INPUTFILE
		input data

	-c CONFFILE, --confFile=CONFFILE
		configuration file

	-o OUTPUTFILEPREFIX, --outputFilePrefix=OUTPUTFILEPREFIX
		prefix of output plots

	-t TOOL, --tool=TOOL
		configuration file (e.g. heatmap))

	-m MODULE, --module=MODULE
		module name (e.g. basic)

	-s, --simple
		only source core.R

	-e, --enableExample
		enable auto load example

	-h, --help
		Show this help message and exit
```

## Maintainer

Hiplot team
