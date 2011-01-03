#' @include object.r
roxygen()

#' Abstract base class for learning algorithms.
#'  
#' How to change object later on: Look at setters.
#' 
#' Tresholds for class labels: If you set \code{predict.type} to "prob" or "decision", the label with the maximum value is selected.
#' You can change labels of a prediction object later by using the function \code{\link{set.threshold}} or find optimal, 
#' non-default thresholds by using \code{\link{make.et.wrapper}}.
#' 
#' How to add further functionality to a learner: Look at subclasses of \code{\linkS4class{base.wrapper}}.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{is.classif [boolean]}{Is this learner for classification tasks?}
#'  \item{is.regr [boolean]}{Is this learner for regression tasks?}
#'  \item{id [string]}{Id string of learner.}
#' 	\item{pack [char]}{Package(s) required for underlying learner.}
#'  \item{doubles [boolean]}{Can real-valued inputs be processed?}
#'  \item{factors [boolean]}{Can factor inputs be processed?}
#'  \item{missings [boolean]}{Can missing values be processed?}
#'  \item{weights [boolean]}{Can case weights be used?}
#' 	\item{par.vals [list]}{List of fixed hyperparameters and respective values for this learner.}
#' 	\item{par.descs [list]}{Named list of \code{\linkS4class{par.desc}} description objects for all possible hyperparameters for this learner.}
#' 	\item{par.descs.when [character]}{Named character vector. Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
#' }
#' 
#' Further getters for classifiers.\cr
#' 
#' \describe{
#'  \item{oneclass [boolean]}{Can oneclass problems be handled?}
#'  \item{twoclass [boolean]}{Can twoclass problems be handled?}
#'  \item{multiclass [boolean]}{Can multiclass problems be handled?}
#'  \item{costs [boolean]}{Can misclassification costs be directly used during training?}
#'  \item{prob [boolean]}{Can probabilities be predicted?}
#'  \item{decision [boolean]}{Can probabilities be predicted?}
#'  \item{predict.type [character]}{What should be predicted: 'response', 'prob' or 'decision'.}
#' }
#' 
#' Setters: \code{\link{set.id}}, \code{\link{set.hyper.pars}}, \code{\link{set.predict.type}}  
#' 
#' @exportClass learner
#' @title Base class for inducers. 

setClass(
		"learner",
		contains = c("object"),
		representation = representation(
				id = "character",
				pack = "character",
				par.descs = "list",
				par.vals = "list"
		)		
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("learner"),
		def = function(.Object, id, pack, par.descs, par.vals) {			
			if (missing(id))
				return(make.empty(.Object))
			.Object@id = id
			.Object@pack = pack
			.Object@predict.type = "response"
			require.packs(pack, for.string=paste("learner", id))
			if (missing(par.descs))
				par.descs = list()
			.Object@par.descs = par.descs
			callNextMethod(.Object)
			if (!missing(par.vals))
				.Object = set.hyper.pars(.Object, par.vals=par.vals)
			return(.Object)
		}
)


#' Getter.
#' @rdname learner-class

setMethod(
		f = "[",
		signature = signature("learner"),
		def = function(x,i,j,...,drop) {
			check.getter.args(x, c("par.when", "par.top.wrapper.only"), j, ...)
      
      if (i == "par.descs") {
        pds = x@par.descs
        names(pds) = sapply(pds, function(y) y@par.name)
        return(pds)
      } 
			if (i == "par.descs.when") {
				w=sapply(x@par.descs, function(y) y@when)
				names(w) = names(x["par.descs", ...])
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
			callNextMethod()
		}
)


