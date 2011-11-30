.mlr.conf <- new.env()
.mlr.export <- new.env()

roxygen <- function() NULL

#' @export .mlr.conf

.onAttach <- function(libname, pkgname) {
	setupErrorHandler()
  setupLogger(level="info")
	setupParallel(mode="local")
}