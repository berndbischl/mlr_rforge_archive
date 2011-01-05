#' @include object.r
roxygen()

#' Abstract base class for learning algorithms.
#'  
#' How to change object later on: Look at setters.
#' 
#' Tresholds for class labels: If you set \code{predict.type} to "prob" or "decision", the label with the maximum value is selected.
#' You can change labels of a prediction object later by using the function \code{\link{set.threshold}} or find optimal, 
#' non-default thresholds by using \code{\link{make.probth.wrapper}} and tuning it.
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
#' 	\item{par.vals [named list]}{List of set hyperparameters.}
#'  \item{par.train [named list]}{List of set hyperparameters which are passed to train function.}
#'  \item{par.predict [named list]}{List of set hyperparameters which are passed to predict function.}
#' 	\item{par.descs [named list]}{Named list of \code{\linkS4class{par.desc}} description objects for all possible hyperparameters.}
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
				par.vals = "list",
        predict.type = "character"
		)		
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("learner"),
		def = function(.Object, id, pack, par.descs=list(), par.vals=list()) {			
			if (missing(id))
				return(make.empty(.Object))
			.Object@id = id
			.Object@pack = pack
			require.packs(pack, for.string=paste("learner", id))
			.Object@par.descs = par.descs
      .Object@predict.type = "response"
      set.hyper.pars(.Object, par.vals=par.vals)
		}
)


#' Getter.
#' @rdname learner-class

setMethod(
		f = "[",
		signature = signature("learner"),
		def = function(x,i,j,...,drop) {
      if (i == "par.descs")  {
        pds = x@par.descs
        names(pds) = sapply(pds, function(y) y@par.name)
        return(pds)
      }     
      if (i == "par.train")  {
        ns = names(Filter(function(y) y@when %in% c("train", "both"), x["par.descs"]))
        ns = intersect(ns, names(x@par.vals))
        return(x["par.vals"][ns])
      }     
      if (i == "par.predict")  {
        ns = names(Filter(function(y) y@when %in% c("predict", "both"), x["par.descs"]))
        ns = intersect(ns, names(x@par.vals))
        return(x["par.vals"][ns])
      }     
      if (i == "par.vals.string") {
        pv = x["par.vals"]
        ns = names(pv)
        pv = Map(function(n, v) hyper.par.val.to.name(n,v,x), ns, pv)
        # print class for complex values
        pv = lapply(pv, function(x) 
            if(is.vector(x) && length(x) == 1)
              x
            else
              paste("<", class(x), ">", sep="")
        )
        return(paste(ns, pv, sep="=", collapse=","))
      }
			callNextMethod()
		}
)


