#' @include object.r
roxygen()

# todo supports getters
# hyperpars getter, read all getters

#' Abstract base class for learning algorithms.
#'  
#' Getter.\cr
#' Note that all getters of \code{\linkS4class{learner.desc}} can also be used. 
#' 
#' \describe{
#'  \item{is.classif [boolean]}{Is this learner for classification tasks?}
#'  \item{is.regr [boolean]}{Is this learner for regression tasks?}
#'  \item{id [string]}{Id string of learner.}
#'	\item{label [string]}{Label string of learner.}
#' 	\item{pack [string]}{Package were underlying learner is implemented.}
#'	\item{desc [{\linkS4class{learner.desc}}]}{Properties object to describe functionality of the learner.}
#' 	\item{par.vals [list]}{List of fixed hyperparameters and respective values for this learner.}
#' 	\item{par.vals.name [character]}{Names of currently fixed hyperparameters.}
#' 	\item{par.descs [list]}{Named list of \code{\linkS4class{par.desc}} description objects for all possible hyperparameters for this learner.}
#' 	\item{par.descs.name [character]}{Names of all hyperparameters for which description objects exist.}
#' 	\item{par.descs.when [character]}{Named character vector. Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
#'  \item{predict.type [character]}{What should be predicted: 'response', 'prob' or 'decision'.}
#'  \item{predict.threshold [character]}{Threshold to produce class labels if type is not "response".} 
#' }
#' @exportClass learner
#' @title Base class for inducers. 

setClass(
		"learner",
		contains = c("object"),
		representation = representation(
				par.descs = "list",
				par.vals = "list"
		)		
)

#' Getter.
#' @rdname learner-class

setMethod(
		f = "[",
		signature = signature("learner"),
		def = function(x,i,j,...,drop) {
			check.getter.args(x, c("par.when", "par.top.wrapper.only"), j, ...)
			if (i == "pack") 
				return("mlr")	
			if (i == "par.descs.name") 
				return(sapply(x@par.descs, function(y) y@par.name))
			if (i == "par.descs.when") {
				w=sapply(x@par.descs, function(y) y@when)
				names(w) = x["par.descs.name", ...]
				return(w)
			}
			
			args = list(...)
			par.when = args$par.when
			if(is.null(par.when)) par.when = c("train", "predict", "both")
			ps = x@par.vals
			ns = names(ps)
			w = x["par.descs.when"]

			if (i == "par.vals")  {
				return( ps[ (w[ns] %in% par.when) | (w[ns] == "both") ] ) 
			}			
			if (i == "par.vals.name")  {
				return(names(x["par.vals", ...]))
			}
			callNextMethod()
		}
)


#' @rdname to.string
setMethod(f = "to.string",
          signature = signature("learner"),
          def = function(x) {
            hps = x["par.vals"]
            hps = paste(names(hps), hps, sep="=", collapse=" ")
            is.classif = x["is.classif"]
            type = if (is.null(is.classif))
              "Unknown"
            else if (is.classif)
              "Classification"
            else
              "Regression"
            return(paste(
                         ##todo regression. also check when applied to task!!
                         type, " learner ", x["id"], " from package ", x["pack"], "\n\n",
                         "Supported features Nums:", x["numerics"],
                         " Factors:", x["factors"],
                         " Chars:", x["characters"], "\n",
                         "Supports missings: ", x["missings"], "\n", 
                         "Supports weights: ", x["weights"], "\n", 
                         "Supports multiclass: ", x["multiclass"], "\n",
                         "Supports probabilities: ", x["probs"], "\n", 
                         "Supports decision values: ", x["decision"], "\n", 
                         "Supports costs: ", x["costs"], "\n", 
                         "Hyperparameters: ", hps, "\n",
                         sep =""					
                         ))
          })
