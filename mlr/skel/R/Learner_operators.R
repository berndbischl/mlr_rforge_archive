#' Get all possible paramter settings for a learner. 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner.   
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @rdname getParamSet
#' @exportMethod getParamSet
setGeneric(name = "getParamSet", def = function(learner) standardGeneric("getParamSet"))
#' @rdname getParamSet
setMethod(
  f = "getParamSet",
  signature = signature(learner="Learner"), 
  def = function(learner) {
    learner@par.set
  } 
)

#' Get current parameter settings for a learner. 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner.   
#' @param for.fun [\code{character(1)}]\cr 
#'   The values corresponding to what aspect of the learner should be returned: 
#'   \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \dQuote{both}.    
#' @return [\code{list}]. A named list of values.
#' @export
getHyperPars = function(learner, for.fun) {
  checkArg(learner, "Learner")
  if (missing(for.fun))
    for.fun = "both"
  else
    checkArg(for.fun, choices=c("train", "predict", "both"))
  getHyperParsTop(learner, for.fun)
}  

getHyperParsTop = function(learner, for.fun) {
  wh = switch(for.fun, 
    train=c("train", "both"), 
    predict=c("predict", "both"),
    both=c("train", "predict", "both")
  )
  pv = learner@par.vals
  ns = names(Filter(function(y) y$when %in% wh, learner@par.set$pars))
  ns = intersect(ns, names(learner@par.vals))
  pv[ns]
}

getHyperParsString = function(learner) {
  hps = getHyperPars(learner, "both")
  ns = names(hps)
  # only use pars ones where we have hyper par values for
  pars = getParamSet(learner)$pars[ns]
  s = Map(paramValueToString, pars, hps)
  paste(ns, s, sep = "=", collapse = ",")
}

getLeafLearner = function(learner) {
  if (is(learner, "BaseWrapper"))
    return(getLeafLearner(learner@learner))
  else 
    return(learner)
}

