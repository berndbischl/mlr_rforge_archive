


.mlr.local <- new.env()
roxygen <- function() NULL

#' @export .mlr.local
.onLoad <- function(libname, pkgname) {
	logger.setup(level="warn")
	parallel.setup(mode="local")
}