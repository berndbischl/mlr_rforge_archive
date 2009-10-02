# todo: can we log where the current log was generated, like in which method (automatically)?

logger.errorhandler <- function() {
	s <- geterrmessage()
	logger.error(s)
}

#' @export
logger.setup <- function(console=TRUE, file=NULL, level) {
	options(warn=1)
	options(error=logger.errorhandler)
	logger.setup <- list()
	logger.setup$console <- console
	logger.setup$file <- file
	logger.setup$global.level <- level

	.mlr.local$logger.setup <- logger.setup
	
	if (!is.null(file)) 
		unlink(file)
	return(logger.setup)
}

logger.print.stuff <- function(prefix, ...) {
	args = list(...)
	if (length(args)>1) {
		cat(prefix, ..., "\n")
	} else {
		co = capture.output(print(...))
		if (length(co) > 1) {
			cat(prefix, "\n")
			co = paste(prefix, co, collapse = "\n")
			co = paste(co, collapse = "\n")
			cat(co, "\n")
		} else {
			cat(prefix, ..., "\n")      
		}
	}
	#cas = lapply(args, function(a) paste(capture.output(a), collapse="\n"))
	#x = paste(cas, sep="", collapse="")
	#  cat(paste(prefix,x, "\n"))
	#}
	#else {
	#  
	#}
}

logger.print <- function(level, ...) {
	prefix = paste("[", level, "]", sep="")
	level <- switch(level,
			error = 4,
			warn = 3,
			info = 2,
			debug = 1)
	
	#cat("level: ", level, "\n")
	#cat("global.level: ", logger.setup$global.level, "\n")
	logger.setup <- .mlr.local$logger.setup
	global.level <- switch(logger.setup$global.level,
			error = 4,
			warn = 3,
			info = 2,
			debug = 1)
	
	
	if (level >= global.level) {
		if (!is.null(logger.setup$file)) { 
			sink(file=logger.setup$file, append=TRUE)
			logger.print.stuff(prefix, ...)  
			sink()
		}
		logger.print.stuff(prefix, ...)
		if (level == "warn") {
			warning(...)
		}
	}
}


logger.error <- function(...) {
	logger.print(level="error", ...)
}

logger.warn <- function(...) {
	logger.print(level="warn", ...)
}

logger.info <- function(...) {
	logger.print(level="info", ...)
}

logger.debug <- function(...) {
	logger.print(level="debug", ...)
}


