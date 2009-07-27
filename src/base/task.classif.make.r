#' @include task.classif.r
roxygen()

setGeneric(
		name = "make.classif.task",
		def = function(learner, formula, data, weights, type) {
			if (missing(weights))
				weights <- rep(1, nrow(data))
			if (missing(type))
				type <- "class"
			standardGeneric("make.classif.task")
		}
)

#' @export
setMethod(
		f = "make.classif.task",
		signature = c(
				learner = "character", 
				formula = "formula", 
				data = "data.frame", 
				weights = "numeric", 
				type = "character"
		),
		
		def = function(learner, formula, data, weights, type) {
			wl <- new(learner)
			ct <- new("classif.task", wrapped.learner=wl, formula=formula, data=data, weights=weights, type=type)
			return(ct)
		}
)
