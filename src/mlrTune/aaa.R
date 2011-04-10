roxygen <- function() NULL

#' @importFrom utils packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Loading package mlrTune. Version: ", packageDescription("mlrTune", fields="Version"))
}