#' @include task.learn.r
roxygen()


#' General description object for a regression experiment.  
#' Instantiate it by using its factory method.
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
		def = function(.Object, id, label, data, weights=rep(1, nrow(data)), target, excluded) {
				
			if (missing(data))
				return(.Object)
			
			prep.ctrl = new("prepare.control")
			data = prep.data(FALSE, data, target, excluded, prep.ctrl)			
			dd = new("data.desc", data=data, target=target, excluded=excluded, prepare.control=prep.ctrl)
			hw = length(weights) > 0
			td = new("task.desc", task.class="regr.task", id=id, label=label, has.weights=hw, 
					costs=matrix(0,0,0), positive=as.character(NA), negative=as.character(NA)) 
			
			callNextMethod(.Object, data=data, weights=weights, 
					data.desc=dd, task.desc=td)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("regr.task"),
		def = function(x) {
			return(
					paste(
							"Regression problem ", x["id"], "\n",
							"Features Nums:", x["n.num"], " Factors:", x["n.fact"], " Chars:", x["n.char"], "\n",
							"Observations: ", x["obs"] , "\n",
							"Missings: ", x["has.missing"], "\n", 
							ifelse(x["has.missing"], paste("in", x["rows.with.missings"], "observations and", x["cols.with.missings"], "features\n"), ""), 
							sep=""
					)
			)
		}
)


