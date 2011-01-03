#' @include task.learn.r
roxygen()


#' General description object for a regression task.  
#' Use \code{\link{make.task}} to create it.   
#' 
#' @exportClass regr.task
#' @title Regression task.
#' @seealso \code{\link{make.task}}

setClass(
		"regr.task",
		contains = c("learn.task")
)


#' Constructor.
#' @title regr.task constructor

setMethod(
		f = "initialize",
		signature = signature("regr.task"),
		def = function(.Object, id, data, weights, blocking, target, exclude, control) {
				
			if (missing(data))
        return(make.empty(.Object))
      
      td = new("task.desc", data, target, exclude, "regr.task", id, 
        length(weights) > 0, length(blocking) > 0, matrix(0,0,0), as.character(NA))      
      
			callNextMethod(.Object, data=data, weights=weights, blocking=blocking, control=control, task.desc=td)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("regr.task"),
		def = function(x) {
      
      rwm = sum(apply(x["data"], 1, function(x) any(is.na(x))))
      cwm = sum(apply(x["data"], 2, function(x) any(is.na(x))))
      rwi = sum(apply(x["data"], 1, function(x) any(is.infinite(x))))
      cwi = sum(apply(x["data"], 2, function(x) any(is.infinite(x))))
      
			return(
					paste(
							"Regression problem ", x["id"], "\n",
              "Features Nums:", x["n.feat"]["num"], " Factors:", x["n.feat"]["fact"], 
              " Ints:", x["n.feat"]["int"], " Chars:", x["n.feat"]["char"], "\n",
              "Exclude: ", x["exclude"], "\n",
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


