

setClass(
		"opt.control",
		contains = c("object"),
		representation = representation(
				minimize = "logical",
				tune.threshold = "logical", 
				thresholds = "numeric",
				path = "logical"
		)
)


setMethod(
		f = "initialize",
		signature = signature("opt.control"),
		def = function(.Object, minimize, tune.threshold, thresholds) {
			if (missing(minimize))
				return(.Object)
			.Object@minimize = minimize
			.Object@tune.threshold = tune.threshold 			
			.Object@thresholds = thresholds 
			.Object@path = FALSE
			return(.Object)
		}
)


#' @rdname opt.control-class

setMethod(
		f = "[",
		signature = signature("opt.control"),
		def = function(x,i,j,...,drop) {
			if (i == "opt.type"){
				if (is(x, "tune.control"))
					return("tune")
				else
					return("varsel")
			}
			callNextMethod()
		}
)


