
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
		if (!is.na(logger.setup$file)) { 
			sink(file=logger.setup$file, append=TRUE)
			logger.print.stuff(prefix, ...)  
			sink()
		}
		logger.print.stuff(prefix, ...)
	}
}


#logger.error <- function(...) {
#	logger.print(level="error", ...)
#}

logger.info <- function(..., level=NA) {
	logger.print(level="info", ...)
}

logger.debug <- function(..., level=NA) {
	logger.print(level="debug", ...)
}


