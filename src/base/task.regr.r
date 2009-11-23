#' @include task.learn.r
roxygen()


#' General description object for a regression experiment.  
#' Instantiate it by using its factory method.
#' 
#' @exportClass classif.task
#' @title classif.task
#' @seealso learn.task make.regr.task

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
		def = function(.Object, data, weights=rep(1, nrow(data)), target) {
				
			if (missing(data))
				return(.Object)
			callNextMethod(.Object,	data=data, weights=weights,	target=target)
		}
)

#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("regr.task"),
		def = function(x) {
			return(
					paste(
							"Regression problem\n",
							as.character(x@data.desc), "\n",
							sep=""
					)
			)
		}
)


