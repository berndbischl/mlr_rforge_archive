#' @include object.r
roxygen()
#' @include wrapped.learner.r
roxygen()


#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the used hyperparameters, the IDs of the used subset and the used variables.    
#' 
#' @title Induced model of learner.
 
setClass(
		"wrapped.model",
		contains = c("object"),
		representation = representation(
				wrapped.learner = "wrapped.learner",
				learner.model = "ANY",
				data.desc = "data.desc",
				task.desc = "task.desc",
				subset = "numeric",
				vars = "character",
				parset = "list",
				time = "numeric"
		)
)

setClass(
		"wrapped.model.classif",
		contains = c("wrapped.model")
)

setClass(
		"wrapped.model.regr",
		contains = c("wrapped.model")
)





#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("wrapped.model"),
		def = function(x) {
			ps <- paste(names(x@parset), x@parset, sep="=", collapse=" ")
			f = x["fail"]
			f = ifelse(is.null(f), "", paste("Training failed:", f))
			tp = x["tuned.par"]
			tp = ifelse(is.null(tp), "", paste("Tuned:", paste(names(tp), tp, sep="=", collapse=" "), "\n"))
			
			return(
					paste(
							"Learner model for ", x@wrapped.learner@label, "\n",  
							"Trained on obs: ", length(x@subset), "\n",
							"Hyperparameters: ", ps, "\n",
							tp,
							f,
							sep=""
					)
			)
		}
)


#' Getter.
#' @param x wrapped.model object
#' @param i [\code{\link{character}}]
#' \describe{
#'	 \item{<slot>}{A slot of the class.}
#' 	 \item{fail}{Generally NULL but if the training failed, the error message of the underlying train function.}
#' }
#' 
#' @rdname wrapped.model-class

setMethod(
		f = "[",
		signature = signature("wrapped.model"),
		def = function(x,i,j,...,drop) {
			if (i == "fail"){
				if (is(x@learner.model, "learner.failure"))
					return(x@learner.model@msg)
				else
					return(NULL)
			}
			if (i == "opt"){
				if (is(x@wrapped.learner, "tune.wrapper"))
					return(attr(x["learner.model"], "opt"))
				else
					return(NULL)
			}
			if (i == "path"){
				if (is(x@wrapped.learner, "tune.wrapper"))
					return(attr(x["learner.model"], "path"))
				else
					return(NULL)
			}
			if (i == "tuned.par"){
				return(x["opt"]$par)
			}
			if (i == "tuned.perf"){
				return(x["opt"]$perf)
			}
			y = x@task.desc[i]
			if (!is.null(y))
				return(y)
			y = x@data.desc[i]
			if (!is.null(y))
				return(y)
			
			callNextMethod()
		}
)













