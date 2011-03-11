# todo desc needs to be here. plus we need a constructor

#' @include object.r
roxygen()
#' @include ParameterSet.R
roxygen()
#' @include LearnerDesc.R
roxygen()

#' Abstract base class for learning algorithms.
#'  
#' How to change object later on: Look at setters.
#' 
#' Tresholds for class labels: If you set \code{predict.type} to "prob" or "decision", the label with the maximum value is selected.
#' You can change labels of a \code{\linkS4class{Prediction}} object later by using the function \code{\link{setThreshold}} or find optimal, 
#' non-default thresholds by using \code{\link{makeProbthWrapper}} and tuning it.
#' 
#' How to add further functionality to a learner: Look at subclasses of \code{\linkS4class{BaseWrapper}}.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{is.classif [boolean]}{Is this learner for classification tasks?}
#'  \item{is.regr [boolean]}{Is this learner for regression tasks?}
#'  \item{id [character(1)]}{Id string of learner.}
#' 	\item{pack [char]}{Package(s) required for underlying learner.}
#'  \item{doubles [boolean]}{Can real-valued inputs be processed?}
#'  \item{factors [boolean]}{Can factor inputs be processed?}
#'  \item{missings [boolean]}{Can missing values be processed?}
#'  \item{weights [boolean]}{Can case weights be used?}
#' 	\item{par.vals [named list]}{List of set hyperparameters.}
#' 	\item{par.set [named list]}{Named list of \code{\linkS4class{Parameter}} description objects for all possible hyperparameters.}
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
#' Setters: \code{\link{setId}}, \code{\link{setHyperPars}}, \code{\link{setPredictType}}  
#' 
#' @exportClass Learner
#' @title Base class for inducers. 

setClass(
		"Learner",
		contains = c("object"),
		representation = representation(
				desc = "LearnerDesc",
				pack = "character",
				par.set = "ParameterSet",
				par.vals = "list",
        predict.type = "character"
		)		
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("Learner"),
		def = function(.Object, id, pack, par.set=makeParameterSet(), par.vals=list()) {			
			if (missing(id))
				return(make.empty(.Object))
      .Object@desc = new("LearnerDesc")
      .Object@desc@id = id
			.Object@pack = pack
			require.packs(pack, for.string=paste("learner", id))
      if(any(sapply(par.set@pars, function(x) !is(x, "LearnerParameter"))))
        stop("All par.set parameters in learner of class ", class(.Object), " must be of class 'LearnerParameter'!")        
			.Object@par.set = par.set
      .Object@predict.type = "response"
      setHyperPars(.Object, par.vals=par.vals)
		}
)




#' Get all possible paramter settings for a learner. 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner.   
#' @return [\code{\linkS4class{ParameterSet}}]
#' @rdname getParameterSet
#' @exportMethod getParameterSet
setGeneric(name = "getParameterSet", def = function(learner) standardGeneric("getParameterSet"))
#' @rdname getParameterSet
setMethod(
  f = "getParameterSet",
  signature = signature(learner="Learner"), 
  def = function(learner) {
    learner@par.set
  } 
)

 
getParameterValues = function(learner, for.fun="both") {
  wh = switch(for.fun, 
     train=c("train", "both"), 
     predict=c("predict", "both"),
     both=c("train", "predict", "both")
  )
  pv = learner@par.vals
  ns = names(Filter(function(y) y@when %in% wh, learner@par.set@pars))
  ns = intersect(ns, names(learner@par.vals))
  pv = pv[ns]
  
  if (is(learner, "BaseWrapper")) 
    c(getParameterValues(learner@learner, for.fun), pv)
  else 
    return(pv)
}

getParameterValuesString = function(learner) {
  valToString(getParameterSet(learner), getParameterValues(learner, "both"))
}

getLeafLearner = function(learner) {
  if (is(learner, "BaseWrapper"))
    return(getLeafLearner(learner@learner))
  else 
    return(learner)
}
