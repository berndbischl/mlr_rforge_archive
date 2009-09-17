#' @include task.learn.r
#' @include resample.instance.r
roxygen()

#' \code{resample.result} contains the results of a resampling process, mainly the list of predicitions and - 
#' if saved - the list of fitted models. 
#' 
#' @slot ri.class	Class of resample.instance
#' @slot ri.name	Name of resample.instance
#' @slot preds		List of predictions, i.e. list of factors or list of matrices for classification 
#' 					respectivly regression.
#' @slot models		List of the fitted models.
#' 
#' @exportClass resample.result
#' @seealso \code{\linkS4class{resample.desc}}, \code{\linkS4class{resample.instance}}, 
#' 			\code{\link{make.cv.instance}}, \code{\link{make.bs.instance}}, 
#' 			\code{\link{make.subsample.instance}}, \code{\link{resample.fit}}
#' @title resample.result

setClass(
		"resample.result",
		representation = representation(
				ri.class="character", 
				ri.name="character", 
				preds="list", 
				models="list"
		)
)

#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("resample.result"),
		def = function(x) {
			return(
					paste(
							"Resampling result for ", x@ri.name, " with ", x["iters"], " iterations\n",
							"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)

#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("resample.result"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("resample.result"),
		def = function(object) {
			cat(as.character(object))
		}
)







#----------------- getter ---------------------------------------------------------


setMethod(
		f = "[",
		signature = signature("resample.result"),
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(length(x@preds))
			
			if (i == "fitted") {
				if (missing(j)) {
					return(x["fitted", 1:x["iters"]])
				} else if(length(j) == 1) {
					return(x@preds[[j]])
				}
				else {
					return(lapply(j, function(k) x["fitted", k]))
				}
			}
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)


