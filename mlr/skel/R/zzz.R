.mlr.conf <- new.env()
.mlr.conf$logger.setup$show.learner.output = TRUE

.onLoad <- function(libname, pkgname) {
	setupErrorHandler()
  #setupLogger(level="info")
	#setupParallel(mode="local")
}