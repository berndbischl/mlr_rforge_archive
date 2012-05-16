#' Create learner object.
#' 
#' For a classification learner the \code{predict.type} can be set
#' to \dQuote{prob} to predict probabilities and the maximum
#' value selects the label. The threshold used to assign the label can later be changed using the
#' \code{\link{setThreshold}} function.
#'
#' For a regression learner the \code{predict.type} can be set
#' to \dQuote{se} to standard errors in addition to the mean response.
#' 
#' @param class [\code{character(1)}] \cr
#'   Class of learner to create. By convention, all classification learners
#'   start with \dQuote{classif.} and all regression learners with
#'   \dQuote{regr.}. A list of all learners is available on the
#'   \code{\link{learners}} help page.
#' @param id [\code{character(1)}] \cr 
#'   Id string for object. Used to select the object from a named list, etc.
#' @param predict.type [\code{character(1)}] \cr
#'   Classification: \dQuote{response} or \dQuote{prob}.
#'   Regression: \dQuote{response} or \dQuote{se}.
#'   Default is \dQuote{response}.
#' @param ... [any] \cr
#'   Optional named (hyper)parameters. Alternatively these can be given
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}] \cr
#'   Optional list of named (hyper)parameters. The arguments in
#'   \code{...} take precedence over values in this list. We strongly
#'   encourage you to use one or the other to pass (hyper)parameters
#'   to the learner but not both.
#' @return [\code{\linkS4class{Learner}}].
#' @export
#' @example
#' makeLearner("classif.logreg")
#' makeLearner("regr.lm")
makeLearner = function(class, id, predict.type="response", ..., par.vals=list()) {
  if (class == "")
    stop("Cannot create learner from empty string!")	
  wl = new(class)
  if (!is(wl, "rlearner"))
    stop("Learner must be a basic rlearner!")
  if (!missing(id))
    wl@id = id
  pds = wl@par.set$pars
  ## pass defaults
  pv = list()
  for (j in seq(length=length(pds))) {
    pd = pds[[j]]
    if (pd$pass.default) {
      pv[[length(pv)+1]] = pd$default
      names(pv)[length(pv)] = pd$id
    }
  }
  pv = insert(pv, par.vals)
  wl = setHyperPars(wl, ..., par.vals=pv)
  if (predict.type != "response")
    wl = setPredictType(wl, predict.type)
  return(wl)
}


#' Abstract base class for learning algorithms.
#'  
#' How to change object later on: Look at setters.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{is.classif [\code{logical(1)}]}{Is this learner for classification tasks?}
#'  \item{is.regr [\code{logical(1)}]}{Is this learner for regression tasks?}
#'  \item{id [\code{character(1)}]}{Id string of learner.}
#'  \item{pack [char]}{Package(s) required for underlying learner.}
#'  \item{doubles [\code{logical(1)}]}{Can real-valued inputs be processed?}
#'  \item{factors [\code{logical(1)}]}{Can factor inputs be processed?}
#'  \item{missings [\code{logical(1)}]}{Can missing values be processed?}
#'  \item{weights [\code{logical(1)}]}{Can case weights be used?}
#'  \item{par.vals [named list]}{List of set hyperparameters.}
#'  \item{par.set [named list]}{Named list of \code{\link[ParamHelpers]{LearnerParam}} description objects for all possible hyperparameters.}
#' }
#' 
#' Further getters for classifiers.\cr
#' 
#' \describe{
#'  \item{oneclass [\code{logical(1)}]}{Can oneclass problems be handled?}
#'  \item{twoclass [\code{logical(1)}]}{Can twoclass problems be handled?}
#'  \item{multiclass [\code{logical(1)}]}{Can multiclass problems be handled?}
#'  \item{prob [\code{logical(1)}]}{Can probabilities be predicted?}
#'  \item{predict.type [character]}{What should be predicted: \dQuote{response}, \dQuote{prob} or \dQuote{se}}
#' }
#' 
#' Setters: \code{\link{setId}}, \code{\link{setHyperPars}}, \code{\link{setPredictType}}  
#' 
#' @exportClass Learner
#' @title Base class for inducers. 

setMethod(
  f = "initialize",
  signature = signature("Learner"),
  def = function(.Object, pack, par.set=makeParamSet(), par.vals=list()) {      
    checkArg(par.set, "ParamSet")  
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
    .Object@properties[["se"]] = FALSE
    .Object@pack = pack
    requirePackages(pack, paste("learner", .Object@id))
    if(any(sapply(par.set$pars, function(x) !is(x, "LearnerParam"))))
      stop("All par.set parameters in learner of class ", class(.Object), " must be of class 'LearnerParam'!")
    .Object@par.set = par.set
    .Object@predict.type = "response"
    setHyperPars(.Object, par.vals=par.vals)
  }
)

