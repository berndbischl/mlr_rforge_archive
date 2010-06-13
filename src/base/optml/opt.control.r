

setClass(
		"opt.control",
		contains = c("object"),
		representation = representation(
				method = "character",
				minimize = "logical",
				tune.threshold = "logical", 
				thresholds = "numeric"
		)
)


setMethod(
		f = "initialize",
		signature = signature("opt.control"),
		def = function(.Object, method, minimize, tune.threshold, thresholds) {
			if (missing(method))
				return(.Object)
			.Object@method = method 			
			.Object@minimize = minimize
			.Object@tune.threshold = tune.threshold 			
			.Object@thresholds = thresholds 			
			return(.Object)
		}
)
