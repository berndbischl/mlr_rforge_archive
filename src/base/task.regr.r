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
			
			
			data = prep.data(data, target, excluded)			
			dd = new("data.desc", data=data, target=target, excluded=excluded)
			td = new("task.desc", task.class="regr.task", id=id, label=label, target=target, positive=as.character(NA), negative=as.character(NA), 
					excluded=excluded, costs=matrix(0,0,0))			
			
			callNextMethod(.Object, data=data, weights=weights, data.desc=dd, task.desc=td)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("regr.task"),
		def = function(x) {
			return(
					paste(
							"Regression problem ", x["name"], "\n",
							to.string(x@data.desc), "\n",
							sep=""
					)
			)
		}
)


