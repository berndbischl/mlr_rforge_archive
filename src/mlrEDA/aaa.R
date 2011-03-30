roxygen <- function() NULL

#' @importFrom utils packageDescription

.onLoad <- function(libname, pkgname) {
	packageStartupMessage("Loading package mlrEDA. Version: ", packageDescription("mlrEDA", fields="Version"))
}