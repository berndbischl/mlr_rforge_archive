#' @include task.learn.r
roxygen()



setClass(
		"classif.task",
		contains = "learn.task",
		representation(
				type = "character",
				trafo.for.classes = "function",
				trafo.for.probs = "function",
				train.par.for.classes = "list",
				train.par.for.probs = "list",
				predict.par.for.classes = "list",
				predict.par.for.probs = "list"
		)
)



#---------------- constructor---- -----------------------------------------------------


setMethod(
		f = "initialize",
		signature = "classif.task",
		def = function(.Object, wrapped.learner, data, weights=rep(1, nrow(data)), formula, type = "class") {
			
			
			#todo: check for classif. learner
			
			if (missing(wrapped.learner))
				return(.Object)
			
			.Object@type <- type

			callNextMethod(.Object, 
					check.function = check.task.classif, 
					wrapped.learner = wrapped.learner, 
					data=data,	
					weights=weights,
					formula=formula
			)
		}
)


setMethod(
		f = "[",
		signature = "classif.task",
		def = function(x,i,j,...,drop) {

			if (i == "class.levels") {
				return(levels(x["targets"]))
			}
			if (i == "class.nr") {
				return(length(levels(x["targets"])))
			}
			callNextMethod()
		}
)



setMethod(
		f = "as.character",
		signature = "classif.task",
		def = function(x) {
			wl <- x@wrapped.learner
			return(
					paste(
							"Classification task for ", wl@learner.name, " from package ", wl@learner.pack, "\n\n",
							as.character(x@data.desc), "\n",
							as.character(wl@learner.props), sep=""
					)
			)
		}
)


