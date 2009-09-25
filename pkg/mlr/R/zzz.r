
.mlr.local <- new.env()


.onLoad <- function(libname, pkgname) {
	logger.define(level="warn")
	parallel.setup(mode="local")
}