#' @include ParameterSet.R
roxygen()

#' Abstract base class for learning algorithms.
#'  
#' How to change object later on: Look at setters.
#' 
#' Tresholds for class labels: If you set \code{predict.type} to "prob", the label with the maximum value is selected.
#' You can change labels of a \code{\linkS4class{Prediction}} object later by using the function \code{\link{setThreshold}}.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{is.classif [\code{logical(1)}]}{Is this learner for classification tasks?}
#'  \item{is.regr [\code{logical(1)}]}{Is this learner for regression tasks?}
#'  \item{id [\code{character(1)}]}{Id string of learner.}
#' 	\item{pack [char]}{Package(s) required for underlying learner.}
#'  \item{doubles [\code{logical(1)}]}{Can real-valued inputs be processed?}
#'  \item{factors [\code{logical(1)}]}{Can factor inputs be processed?}
#'  \item{missings [\code{logical(1)}]}{Can missing values be processed?}
#'  \item{weights [\code{logical(1)}]}{Can case weights be used?}
#' 	\item{par.vals [named list]}{List of set hyperparameters.}
#' 	\item{par.set [named list]}{Named list of \code{\linkS4class{Parameter}} description objects for all possible hyperparameters.}
#' }
#' 
#' Further getters for classifiers.\cr
#' 
#' \describe{
#'  \item{oneclass [\code{logical(1)}]}{Can oneclass problems be handled?}
#'  \item{twoclass [\code{logical(1)}]}{Can twoclass problems be handled?}
#'  \item{multiclass [\code{logical(1)}]}{Can multiclass problems be handled?}
#'  \item{prob [\code{logical(1)}]}{Can probabilities be predicted?}
#'  \item{predict.type [character]}{What should be predicted: \dQuote{response} or \dQuote{prob}}
#' }
#' 
#' Setters: \code{\link{setId}}, \code{\link{setHyperPars}}, \code{\link{setPredictType}}  
#' 
#' @exportClass Learner
#' @title Base class for inducers. 

setClass(
		"Learner",
		representation = representation(
        id = "character",
        pack = "character",
        properties = "list",
				par.set = "ParameterSet",
				par.vals = "list",
        predict.type = "character"
		)		
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("Learner"),
		def = function(.Object, pack, par.set=makeParameterSet(), par.vals=list()) {			
			if (missing(pack))
				return(make.empty(.Object))
      cc = as.character(class(.Object))[1]
      if (is(.Object, "rlearner.classif"))
        .Object@properties["type"] = "classif"
      else if(is(.Object, "rlearner.regr"))
        .Object@properties["type"] = "regr"
      .Object@id = cc 
      .Object@properties["numerics"] = FALSE
      .Object@properties["factors"] = FALSE
      .Object@properties[["weights"]] = FALSE  
      .Object@properties[["missings"]] = FALSE
      .Object@properties[["oneclass"]] = FALSE
      .Object@properties[["twoclass"]] = FALSE
      .Object@properties[["multiclass"]] = FALSE
      .Object@properties[["prob"]] = FALSE
			.Object@pack = pack
			requirePackages(pack, paste("learner", .Object@id))
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


#' Get current parameter settings for a learner. 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner.   
#' @param for.fun [\code{character(1)}]\cr 
#'   The values corresponding to what aspect of the learner should be returned: 'train', 'predict', or 'both'.
#'   Default is 'both'.    
#' @return A named list of values.
#' @rdname getHyperPars
#' @exportMethod getHyperPars
setGeneric(name = "getHyperPars", 
  def = function(learner, for.fun) {
    if (missing(for.fun))
      for.fun = "both"      
    checkArg(for.fun, "character", 1, c("train", "predict", "both"))
    standardGeneric("getHyperPars")
  }
)  
#' @rdname getHyperPars
setMethod(
  f = "getHyperPars",
  signature = signature(learner="Learner", for.fun="character"), 
  def = function(learner, for.fun) {
    getHyperParsTop(learner, for.fun)
  } 
)


getHyperParsTop = function(learner, for.fun) {
  wh = switch(for.fun, 
    train=c("train", "both"), 
    predict=c("predict", "both"),
    both=c("train", "predict", "both")
  )
  pv = learner@par.vals
  ns = names(Filter(function(y) y@when %in% wh, learner@par.set@pars))
  ns = intersect(ns, names(learner@par.vals))
  pv[ns]
}

getHyperParsString = function(learner) {
  valToString(getParameterSet(learner), getHyperPars(learner, "both"))
}

getLeafLearner = function(learner) {
  if (is(learner, "BaseWrapper"))
    return(getLeafLearner(learner@learner))
  else 
    return(learner)
}

