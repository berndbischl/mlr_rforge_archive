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
		def = function(.Object, wrapped.learner, data, weights=rep(1, nrow(data)), target) {
				
			if (missing(wrapped.learner))
				return(.Object)
			callNextMethod(.Object, 
					check.function = function(x) list(msg="", data=x@data),
					wrapped.learner = wrapped.learner,
					data=data,	
					weights=weights,
					target=target
			)
		}
)

#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("regr.task"),
		def = function(x) {
			wl <- x@wrapped.learner
			return(
					paste(
							"Regression task for ", wl@learner.name, " from package ", wl@learner.pack, "\n\n",
							as.character(x@data.desc), "\n",
							as.character(wl@learner.props), sep=""
					)
			)
		}
)


