#' Get a description of all possible parameter settings for a learner. 
#' 
#' @param learner [\code{\link{Learner}}]\cr 
#'   The learner.   
#' @return [\code{\link[ParamHelpers]{ParamSet}}].
#' @export
getParamSet = function(learner) {
  # FIXME checkArgs
  UseMethod("getParamSet")
} 

#'@S3method getParamSet Learner
getParamSet.Learner = function(learner) {
  # FIXME checkArgs
  learner$par.set
} 

#' Get current parameter settings for a learner. 
#' 
#' @param learner [\code{\link{Learner}}]\cr 
#'   The learner.   
#' @param for.fun [\code{character(1)}]\cr 
#'   Restrict the returned settings to hyperparameters corresponding to: 
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

#' Set the hyperparameters of a learner object.
#' 
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param ... [any]\cr
#'   Named (hyper)parameters with new setting. Alternatively these can be passed
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'    Optional list of named (hyper)parameter settings. The arguments in
#'    \code{...} take precedence over values in this list. We strongly
#'    encourage you to use one or the other to pass (hyper)parameters
#'    to the learner but not both.
#' @return [\code{\link{Learner}}] with changed hyperparameters.
#' @export
#' @seealso See \code{\link{getHyperPars}} for a function to retrieve
#'   the currently set hyperparameters. To get a list of all hyperparameters of
#'   a learner, see the \code{par.set} slot of the \code{\link{Learner}}
#'   object.
#' @examples
#' cl1 <- makeLearner("classif.ksvm", sigma=1)
#' cl2 <- setHyperPars(cl1, sigma=10, par.vals=list(C=2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals) {
  # FIXME checkArgs
  UseMethod("setHyperPars")
} 

#'@S3method setHyperPars Learner
setHyperPars.Learner = function(learner, ..., par.vals) {
  ns = names(par.vals)
  pars = learner$par.set$pars
  for (i in seq_along(par.vals)) {
    n = ns[i]
    p = par.vals[[i]]
    pd = pars[[n]]
    if (is.null(pd)) {
      # no description: stop warn or quiet
      msg = sprintf("%s: Setting parameter %s without available description object!", learner$id, n)
      opwd = getOption("mlr.on.par.without.desc")
      if (opwd == "stop")
        stop(msg)
      if (opwd == "warn")
        warning(msg)
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id=n)
      learner$par.vals[[n]] = p
    } else {
      if (!isFeasible(pd, p))
        # FIXME what if strange value class?
        stopf("%s is not a feasible parameter setting!", p)
      # if valname of discrete par was used, transform it to real value
      # FIXME: is type ordered still there? reason for this code?
      if ((pd$type == "discrete" || pd$type == "ordered") 
        && is.character(p) && length(p) == 1 && p %in% names(pd$values))
        p = pd$values[[p]]
      learner$par.vals[[n]] = p
    }
  }
  return(learner)
} 


getHyperParsTop = function(learner, for.fun) {
  wh = switch(for.fun, 
    train=c("train", "both"), 
    predict=c("predict", "both"),
    both=c("train", "predict", "both")
  )
  pv = learner$par.vals
  ns = names(Filter(function(y) y$when %in% wh, learner$par.set$pars))
  ns = intersect(ns, names(learner$par.vals))
  pv[ns]
}

# FIXME what if hyper pars are of complx type?
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
    return(getLeafLearner(learner$learner))
  else 
    return(learner)
}

