#' @include task.learn.r
roxygen()



#' @export
setClass(
		"regr.task",
		contains = c("learn.task")
)



#---------------- constructor---- -----------------------------------------------------


setMethod(
		f = "initialize",
		signature = signature("regr.task"),
		def = function(.Object, wrapped.learner, data, weights=rep(1, nrow(data)), formula) {
				
			if (missing(wrapped.learner))
				return(.Object)
			callNextMethod(.Object, 
					check.function = function(x) list(msg="", data=x@data),
					wrapped.learner = wrapped.learner,
					data=data,	
					weights=weights,
					formula=formula
			)
		}
)


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


