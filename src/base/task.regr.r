#' @include task.learn.r
roxygen()


#' General description object for a regression task.  
#' Use \code{\link{make.task} to create it.   
#' 
#' @exportClass regr.task
#' @title Regression task.
#' @seealso \code{\link{make.task}}

setClass(
		"regr.task",
		contains = c("learn.task")
)



#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title regr.task constructor

setMethod(
		f = "initialize",
		signature = signature("regr.task"),
		def = function(.Object, id, data, weights, blocking, target, exclude) {
				
			if (missing(data))
				return(.Object)
			
			prep.ctrl = new("prepare.control")
			data = prep.data(FALSE, data, target, exclude, prep.ctrl)			
			dd = new("data.desc", data=data, target=target, exclude=exclude, prepare.control=prep.ctrl)
			hw = length(weights) > 0
			hb = length(blocking) > 0
			td = new("task.desc", task.class="regr.task", id=id, has.weights=hw, has.blocking=hb, 
					costs=matrix(0,0,0), positive=as.character(NA), negative=as.character(NA)) 
			
			callNextMethod(.Object, data=data, weights=weights, blocking=blocking,
					data.desc=dd, task.desc=td)
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
							"Features Nums:", x["n.num"], " Factors:", x["n.fact"], "\n",
							"Observations: ", x["size"] , "\n",
							"Missings: ", x["has.missing"], "\n", 
							ifelse(x["has.missing"], paste("in", rwm, "observations and", cwm, "features\n"), ""), 
              "Infinites: ", x["has.inf"], "\n", 
              ifelse(x["has.inf"], paste("in", rwi, "observations and", cwi, "features\n"), ""), 
              sep=""
					)
			)
		}
)


