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

## Maintainer

Hiplot team
