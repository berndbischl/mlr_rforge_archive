#' @include task.learn.r
roxygen()



setClass(
		"classif.task",
		contains = c("learn.task"),
		representation = representation(
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

#' Constructor.
#' @title classif.task constructor

setMethod(
		f = "initialize",
		signature = signature("classif.task"),
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
		signature = signature("classif.task"),
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


#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("classif.task"),
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


