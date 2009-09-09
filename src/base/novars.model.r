#' Description class for cross-validation.
#' @exportClass cv.desc
#' @title cv.desc
#' @seealso \code{\link{make.cv.desc}}

setClass("novars.model", 
		representation = representation(
				targets = "ANY"
		)
)                                                     

#' Create description object for cross-validation.
#' @param iters Number of iterations

setMethod(
		f = "initialize",
		signature = signature("novars.model.classif"),
		def = function(.Object, targets) {
			.Object@targets <- targets 
			return(.Object)
		}
)

setClass("novars.model.classif", 
		contains = c("novars.model")
)                                                     


setMethod(
		f = "predict",
		signature = signature(object="novars.model.classif"),
		def = function(object, newdata, type="default") {
			probs <- as.numeric(table(object@targets)) / length(object@targets)
			sample(levels(object@targets), nrow(newdata), replace=TRUE, prob=probs)
		}
)



