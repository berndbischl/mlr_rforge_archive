setClass(
		"varsel.control",
		contains = c("opt.control"),
		representation = representation(
				compare = "character",
				max.vars = "integer", 
				maxit = "integer"
		)
)


setMethod(
		f = "initialize",
		signature = signature("varsel.control"),
		def = function(.Object, method, minimize, tune.threshold, thresholds, maxit, max.vars) {
			if (missing(method))
				return(.Object)
			.Object@compare = "diff" 			
			.Object@max.vars = as.integer(max.vars) 			
			.Object@maxit = as.integer(maxit) 		
			.Object = callNextMethod(.Object, method, minimize, tune.threshold, thresholds)
			return(.Object)
		}
)

