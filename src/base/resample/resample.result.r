#' @include task.learn.r
#' @include resample.instance.r
roxygen()

#' \code{resample.result} contains the results of a resampling process, mainly the list of predicitions and - 
#' if saved - the list of fitted models. 
#' 
#' @slot instance	Resample instance used to produce the result
#' @slot preds		List of predictions, i.e. list of factors or list of matrices for classification respectivly regression.
#' @slot extracted	List of extracted informations from the models. Could also be the complete fitted models. 
#' 
#' @exportClass resample.result
#' @seealso \code{\linkS4class{resample.desc}}, \code{\linkS4class{resample.instance}}, 
#' 			\code{\link{make.cv.instance}}, \code{\link{make.bs.instance}}, 
#' 			\code{\link{make.subsample.instance}}, \code{\link{resample.fit}}
#' @title resample.result

setClass(
		"resample.result",
		representation = representation(
				instance="resample.instance", 
				preds="list", 
				extracted="list"
		)
)

#' Conversion to string.
setMethod(
		f = "to.string",
		signature = signature("resample.result"),
		def = function(x) {
			return(
					paste(
							"Resampling result for ", x@instance["name"], " with ", x["iters"], " iterations\n",
							#"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
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
			cat(to.string(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("resample.result"),
		def = function(object) {
			cat(to.string(object))
		}
)


#' Getter.
#' @param x resample.result object
#' @param i [character]
#' \describe{
#'   \item{iters}{Number of predictions.}
#'   \item{fitted}{If j is missing all fitted values are returned. Otherwise they are indexed by j.}
#' }
#' @param j [integer] \cr See above, i == "fitted".
#' 
#' @rdname getter,resample.result-method
#' @aliases resample.result.getter getter,resample.result-method
#' @title Getter for resample.result

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


