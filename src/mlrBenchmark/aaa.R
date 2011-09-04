roxygen <- function() NULL

#' @importFrom utils packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Loading package mlrBenchmark. Version: ", packageDescription("mlrBenchmark", fields="Version"))
}