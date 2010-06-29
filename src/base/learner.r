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
#'	\item{props [{\linkS4class{learner.desc}}]}{Properties object to describe functionality of the learner.}
#' 	\item{hyper.pars [list]}{List of fixed hyperparameters and respective values for this learner.}
#' 	\item{hyper.names [character]}{Names of currently used hyperparameters.}
#' 	\item{hyper.types [character]}{For which step in the model building process are the respective hyperparameters used? Named character vector.}
#'  \item{numerics [boolean]}{Can numeric inputs be processed?}
#'  \item{factors [boolean]}{Can factor inputs be processed?}
#'  \item{characters [boolean]}{Can character inputs be processed?}
#'  \item{missings [boolean]}{Can missing values be processed?}
#'  \item{multiclass [boolean]}{Can probabilities be predicted?}
#'  \item{costs [boolean]}{Can misclassification costs be directly used during training?}
#'  \item{probs [boolean]}{Can probabilities be predicted?}
#'  \item{decision [boolean]}{Can probabilities be predicted?}
#'  \item{weights [boolean]}{Can case weights be used?}
#' }
#' @exportClass learner
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
setMethod(f = "to.string",
          signature = signature("learner"),
          def = function(x) {
            hps = x["hyper.pars"]
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
