#' @include LearnTask.R
roxygen()


#' A regression task.  
#' Use \code{\link{makeRegrTask}} to create it.   
#' 
#' @exportClass RegrTask
#' @title Regression task.
#' @seealso \code{\link{makeRegrTask}}

setClass(
		"RegrTask",
		contains = c("LearnTask")
)


#' Constructor.
#' @title RegrTask constructor

setMethod(
		f = "initialize",
		signature = signature("RegrTask"),
		def = function(.Object, id, data, weights, blocking, target) {
				
			if (missing(data))
        return(make.empty(.Object))
      
      td = new("TaskDesc", data, target, "regr", id, 
        length(weights) > 0, length(blocking) > 0, as.character(NA))      
      
			callNextMethod(.Object, data=data, weights=weights, blocking=blocking, task.desc=td)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("RegrTask"),
		def = function(x) {
      td = x@desc
      data = getData(x)
      feat = paste(capture.output(x@desc@n.feat), collapse="\n")
      return(
					paste(
							"Regression problem ", td@id, "\n",
              "Features:\n", feat, "\n", 
              "Observations: ", td@size , "\n",
							"Missings: ", td@has.missing, "\n", 
              "Infinites: ", td@has.inf, "\n", 
              "Target: ", td@target, "\n", 
              "Has weights: ", td@has.weights, "\n", 
              "Has blocking: ", td@has.blocking, "\n",
              sep=""
					)
			)
		}
)


