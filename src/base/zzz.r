


.mlr.local <- new.env()

#' @export .mlr.local
.onLoad <- function(libname, pkgname) {
	logger.setup(level="warn")
	parallel.setup(mode="local")
}