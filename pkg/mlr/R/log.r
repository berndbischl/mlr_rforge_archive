# todo: can we log where the current log was generated, like in which method (automatically)?

#logger.errorhandler <- function() {
#	s <- geterrmessage()
#	logger.error(s)
#}

logger.setup <- function(console=TRUE, file=NA, level, sublevel=NA) {
	if (level=="error") {
		options(warn=-1)
	} else {
		options(warn=1)
	}
		
	#options(error=logger.errorhandler)
	logger.setup <- list()
	logger.setup$console <- console
	logger.setup$file <- file
	logger.setup$global.level <- level
	logger.setup$sublevel <- sublevel
	
	.mlr.local$logger.setup <- logger.setup
		
	if (!(is.na(file))) 
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

logger.print <- function(level, sublevel=NA, ...) {
	prefix = paste("[", level, ifelse(is.na(sublevel), "", paste(":", sublevel, sep="")), "]", sep="")
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
	
	if (level >= global.level && ( is.na(sublevel) || sublevel %in% logger.setup$sublevel)) {
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

logger.info <- function(...) {
	logger.print(level="info", sublevel=NA, ...)
}

logger.debug <- function(..., level=NA) {
	logger.print(level="debug", sublevel=level, ...)
}


