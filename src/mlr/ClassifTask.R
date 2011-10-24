#' @include LearnTask.R
roxygen()

#' A classification task.  
#' Use \code{\link{makeClassifTask}} to create it.   
#' 
#' @exportClass ClassifTask
#' @title Classification task.
#' @seealso \code{\link{makeClassifTask}}


setClass(
		"ClassifTask",
		contains = c("LearnTask")
)



#' Constructor.
#' @title ClassifTask constructor

setMethod(
		f = "initialize",
		signature = signature("ClassifTask"),
		def = function(.Object, id, target, data, weights, blocking, positive) {
			if (missing(data))
				return(make.empty(.Object))
			
      td = new("TaskDesc", data, target, "classif", id, 
        length(weights) > 0, length(blocking) > 0, positive)      

			callNextMethod(.Object, data=data, weights=weights, blocking=blocking, task.desc=td)
		}
)


setMethod("show", "ClassifTask", function(object) {
  td = object@desc
  di = table(getTargets(object)) 
  di = paste(capture.output(di)[-1], collapse="\n")
  m = length(td@class.levels)
  data = getData(object)
  feat = paste(capture.output(td@n.feat), collapse="\n")
  cat(
    "Classification problem ", td@id, "\n",
    "Features:\n", feat, "\n", 
    "Observations: ", td@size , "\n",
    "Missings: ", td@has.missing, "\n", 
    "Infinites: ", td@has.inf, "\n", 
    "Target: ", td@target, "\n", 
    "Classes: ", m, "\n",
    di, "\n",
    ifelse(m == 2, paste("Positive class:", td@positive, "\n"), ""),
    "Has weights: ", td@has.weights, "\n", 
    "Has blocking: ", td@has.blocking, "\n",
    sep = ""
  )
})


