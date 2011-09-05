


.mlr.local <- new.env()
.mlr.export <- new.env()

roxygen <- function() NULL

#' @export .mlr.local
#' @importFrom utils packageDescription

.onLoad <- function(libname, pkgname) {
	errorhandler.setup()
  setupLogger(level="info")
	parallel.setup(mode="local")
	packageStartupMessage("Loading package mlr. Version: ", packageDescription("mlr", fields="Version"))
}