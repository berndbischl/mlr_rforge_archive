

logger.def <- list()
.parallel.setup <- list() 

.onLoad <- function(libname, pkgname) {
	logger.define(level="warn")
	parallel.setup(mode="local")
}