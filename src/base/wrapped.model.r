#' @include object.r
roxygen()
#' @include data.desc.r
roxygen()
#' @include task.desc.r
roxygen()
#' @include wrapped.learner.r
roxygen()


#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the IDs of the subset used for thraining, variables used for training and    
#' information about second-level optimization like tuned hyperparameters or selected variables. 
#' 
#' Getter.\cr
#' 
#' \describe{
#'	\item{wrapped.learner [{\linkS4class{wrapped.learner}}]}{Wrapped learner that was used to fit the model.}
#'	\item{learner model [any]}{Undelying model from used R package.}
#'	\item{subset [integer]}{Subset used for training.}
#'	\item{vars [character]}{Variables used for training.}
#'	\item{parset [list]}{Hyperparameters used for training.}
#'	\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
#'	\item{opt [path.element]}{Optimum of second-level optimization.}
#'	\item{path [list of path.elements]}{Path of second-level optimization.}
#'	\item{tuned.par [list]}	{If tuning was performed, best found set of hyperparameters.}
#'	\item{tuned.perf [numeric]}{If tuning was performed, performance of best found set of hyperparameters.}
#'	\item{sel.vars [character]}{If variable selection was performed, best found set of variables.}
#'	\item{sel.perf [numeric]}{If variable selection was performed, performance of best found set of variables.}
#' }
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
				if (is(x@wrapped.learner, "opt.wrapper"))
					return(attr(x["learner.model"], "opt"))
				else
					return(NULL)
			}
			if (i == "path"){
				if (is(x@wrapped.learner, "opt.wrapper"))
					return(attr(x["learner.model"], "path"))
				else
					return(NULL)
			}
			if (i == "tuned.par"){
				if (is(x@wrapped.learner, "opt.wrapper") && x@wrapped.learner@type == "tune")
					return(x["opt"]$par)
				else
					return(NULL)
			}
			if (i == "tuned.perf"){
				if (is(x@wrapped.learner, "opt.wrapper") && x@wrapped.learner@type == "tune")
					return(x["opt"]$perf)
				else
					return(NULL)
			}
			if (i == "sel.var"){
				if (is(x@wrapped.learner, "opt.wrapper") && x@wrapped.learner@type == "varsel")
					return(x["opt"]$par)
				else
					return(NULL)
			}
			if (i == "sel.perf"){
				if (is(x@wrapped.learner, "opt.wrapper") && x@wrapped.learner@type == "varsel")
					return(x["opt"]$perf)
				else
					return(NULL)
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













