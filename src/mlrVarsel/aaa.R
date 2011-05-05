roxygen <- function() NULL

#' @importFrom utils packageDescription
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Loading package mlrVarsel. Version: ", packageDescription("mlrVarsel", fields="Version"))
}