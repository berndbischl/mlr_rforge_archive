#' @include object.r
roxygen()

# todo supports getters
# hyperpars getter, read all getters

#' Abstract base class for learning algorithms.
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{is.classif [boolean]}{Is this learner for classification tasks?}
#'  \item{is.regr [boolean]}{Is this learner for regression tasks?}
#'  \item{id [string]}{Id string of learner.}
#'	\item{label [string]}{Label string of learner.}
#' 	\item{pack [string]}{Package were underlying learner is implemented.}
#'	\item{props [{\linkS4class{learner.props}}]}{Properties object to describe functionality of the learner.}
#'  \item{supports.probs [boolean]}{Can probabilities be predicted?}
#' }
#' @title Base class for inducers. 

setClass(
		"learner",
		contains = c("object"),
		representation = representation(
				hyper.pars = "list",
				hyper.types = "character"
		)		
)

#' Getter.
#' @rdname learner-class

setMethod(
		f = "[",
		signature = signature("learner"),
		def = function(x,i,j,...,drop) {
			if (i == "pack") 
				return("mlr")
			
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
			callNextMethod()
		}
)



 
#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("learner"),
		def = function(x) {
			hps = x["hyper.pars"]
			hps = paste(names(hps), hps, sep="=", collapse=" ")
			type = switch(x["is.classif"], "Classification", "Regression")
			return(paste(
							#todo regression. also check when applied to task!!
							type, " learner ", x["id"], " from package ", x["pack"], "\n\n",					
							to.string(x["props"]), "\n",
							"Hyperparameters: ", hps, "\n",
							sep =""					
					))
		}
)



