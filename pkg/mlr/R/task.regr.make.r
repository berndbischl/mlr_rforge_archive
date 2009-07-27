#' @include task.regr.r

setGeneric(
		name = "make.regr.task",
		def = function(learner, formula, data, weights) {
			if (missing(weights))
				weights <- rep(1, nrow(data))
			standardGeneric("make.regr.task")
		}
)

#' @export
setMethod(
		f = "make.regr.task",
		signature = c(
				learner = "character", 
				formula = "formula", 
				data = "data.frame", 
				weights = "numeric" 
		),
		
		def = function(learner, formula, data, weights) {
			wl <- new(learner)
			rt <- new("regr.task", wrapped.learner=wl, formula=formula, data=data, weights=weights)
			return(rt)
		}
)
