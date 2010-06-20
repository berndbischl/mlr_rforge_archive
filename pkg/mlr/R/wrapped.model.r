# todo: wrapped.model becomes base class, have a normal subclass + one for optimized models???

#' @include object.r
roxygen()
#' @include data.desc.r
roxygen()
#' @include task.desc.r
roxygen()
#' @include learner.r
roxygen()


#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the IDs of the subset used for training, variables used for training and    
#' information about second-level optimization like tuned hyperparameters or selected variables. 
#' 
#' Getter.\cr
#' 
#' \describe{
#'	\item{learner [{\linkS4class{learner}}]}{Learner that was used to fit the model.}
#'	\item{learner model [any]}{Underlying model from used R package.}
#'	\item{subset [integer]}{Subset used for training.}
#'	\item{vars [character]}{Variables used for training.}
#' 	\item{hyper.pars [list]}{List of fixed hyperparameters and respective values for this model.}
#' 	\item{hyper.names [character]}{Names of used hyperparameters.}
#' 	\item{hyper.types [character]}{For which step in the model building process the respective hyperparameters used? Named character vector.}
#'	\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
#'	\item{opt [path.element]}{Optimum of second-level optimization.}
#'	\item{path [list of path.elements]}{Path of second-level optimization.}
#'	\item{tuned.par [list]}{If tuning was performed, best found set of hyperparameters.}
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
				learner = "learner",
				learner.model = "ANY",
				data.desc = "data.desc",
				task.desc = "task.desc",
				subset = "numeric",
				vars = "character",
				hyper.pars = "list",
				hyper.types = "character",
				time = "numeric"
		)
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("wrapped.model"),
		def = function(x) {
			ps <- paste(names(x["hyper.pars"]), x["hyper.pars"], sep="=", collapse=" ")
			f = x["fail"]
			f = ifelse(is.null(f), "", paste("Training failed:", f))
			tp = x["tuned.par"]
			tp = ifelse(is.null(tp), "", paste("Tuned:", paste(names(tp), tp, sep="=", collapse=" "), "\n"))
			
			return(
					paste(
							"Learner model for ", x@learner["id"], "\n",  
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
			args = list(...)
			type = args$type
			if (is.null(type))
				ps = seq(length=length(x@hyper.pars))
			else
				ps = which(x@hyper.types %in% type)
			if (i == "hyper.pars") 
				return(x@hyper.pars[ps])
			if (i == "hyper.names") 
				return(names(x@hyper.pars)[ps])
			if (i == "hyper.types") 
				return(x@hyper.types)
			
			if (i == "fail"){
				if (is(x@learner.model, "learner.failure"))
					return(x@learner.model@msg)
				else
					return(NULL)
			}
			if (i == "opt.result"){
				if (is(x@learner, "opt.wrapper"))
					return(attr(x["learner.model"], "opt.result"))
				else
					return(NULL)
			}
			if (i == "opt"){
				if (is(x@learner, "opt.wrapper"))
					return(x["opt.result"]["opt"])
				else
					return(NULL)
			}
			if (i == "opt.par"){
				if (is(x@learner, "opt.wrapper"))
					return(x["opt"]$par)
				else
					return(NULL)
			}
			if (i == "tuned.par"){
				if (is(x@learner, "opt.wrapper") && x@learner["opt.type"] == "tune")
					return(x["opt.par"])
				else
					return(NULL)
			}	
			if (i == "sel.var"){
				if (is(x@learner, "opt.wrapper") && x@learner["opt.type"] == "varsel")
					return(x["opt.par"])
				else
					return(NULL)
			}	
			if (i == "opt.perf"){
				if (is(x@learner, "opt.wrapper"))
					return(x["opt"]$perf)
				else
					return(NULL)
			}
			if (i == "path"){
				if (is(x@learner, "opt.wrapper"))
					return(x["opt.result"]["path"])
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













