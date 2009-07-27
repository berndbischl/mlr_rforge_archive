# todo: can we log where the current log was generated, like in which method (automatically)?

#' @importFrom utils assignInNamespace


logger.errorhandler <- function() {
	s <- geterrmessage()
	logger.error(s)
}

#' @export
logger.define <- function(console=TRUE, file=NULL, level, global=FALSE) {
	options(warn=1)
	options(error=logger.errorhandler)
	logger.def <- list()
	logger.def$console <- console
	logger.def$file <- file
	logger.def$global.level <- level
	# i dont know why i need to do this. otherwise R CMD check wont accept the example code
	if (global)
		assign("logger.def", logger.def, envir=.GlobalEnv)
	else
		assignInNamespace("logger.def", logger.def, ns="mlr")
	if (!is.null(file)) 
		unlink(file)
	return(logger.def)
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
	#cat("global.level: ", logger.def$global.level, "\n")
	global.level <- switch(logger.def$global.level,
			error = 4,
			warn = 3,
			info = 2,
			debug = 1)
	
	
	if (level >= global.level) {
		if (!is.null(logger.def$file)) { 
			sink(file=logger.def$file, append=TRUE)
			logger.print.stuff(prefix, ...)  
			sink()
		}
		logger.print.stuff(prefix, ...)
		if (level == "warn") {
			warning(...)
		}
	}
}


#' @export
logger.error <- function(...) {
	logger.print(level="error", ...)
}

#' @export
logger.warn <- function(...) {
	logger.print(level="warn", ...)
}

#' @export
logger.info <- function(...) {
	logger.print(level="info", ...)
}

#' @export
logger.debug <- function(...) {
	logger.print(level="debug", ...)
}


