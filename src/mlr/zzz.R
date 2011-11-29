.mlr.local <- new.env()
.mlr.export <- new.env()

roxygen <- function() NULL

#' @export .mlr.local

.onAttach <- function(libname, pkgname) {
	setupErrorHandler()
  setupLogger(level="info")
	setupParallel(mode="local")
}