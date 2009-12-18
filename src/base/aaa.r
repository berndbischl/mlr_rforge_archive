


.mlr.local <- new.env()
roxygen <- function() NULL

#' @export .mlr.local
#' @importFrom utils packageDescription

.onLoad <- function(libname, pkgname) {
	logger.setup(level="warn")
	parallel.setup(mode="local")
	packageStartupMessage("Loading package mlr. Version: ", packageDescription("mlr", fields="Version"))
}