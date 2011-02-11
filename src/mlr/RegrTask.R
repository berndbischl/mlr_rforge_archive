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
		def = function(.Object, id, data, weights, blocking, target, control) {
				
			if (missing(data))
        return(make.empty(.Object))
      
      td = new("task.desc", data, target, "RegrTask", id, 
        length(weights) > 0, length(blocking) > 0, matrix(0,0,0), as.character(NA))      
      
			callNextMethod(.Object, data=data, weights=weights, blocking=blocking, control=control, task.desc=td)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("RegrTask"),
		def = function(x) {
      
      rwm = sum(apply(x["data"], 1, function(x) any(is.na(x))))
      cwm = sum(apply(x["data"], 2, function(x) any(is.na(x))))
      rwi = sum(apply(x["data"], 1, function(x) any(is.infinite(x))))
      cwi = sum(apply(x["data"], 2, function(x) any(is.infinite(x))))
      feat = paste(capture.output(x["n.feat"]), collapse="\n")
      return(
					paste(
							"Regression problem ", x@desc@id, "\n",
              "Features:\n", feat, "\n", 
              "Observations: ", x["size"] , "\n",
							"Missings: ", x["has.missing"], "\n", 
							ifelse(x["has.missing"], paste("in", rwm, "observations and", cwm, "features\n"), ""), 
              "Infinites: ", x["has.inf"], "\n", 
              ifelse(x["has.inf"], paste("in", rwi, "observations and", cwi, "features\n"), ""), 
              "Target: ", x["target"], "\n", 
              "Has weights: ", x["has.weights"], "\n", 
              "Has blocking: ", x["has.blocking"], "\n",
              sep=""
					)
			)
		}
)


