roxygen <- function() NULL

#' @importFrom utils packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Loading package mlrEnsemble. Version: ", packageDescription("mlrEnsemble", fields="Version"))
}