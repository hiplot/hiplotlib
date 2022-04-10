.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("You are using ", pkgname, " version ", version)
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
}
