#' Get a description of all possible parameter settings for a learner. 
#' @param learner [\code{\link{Learner}}]\cr 
#'   Learner.   
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(learner) {
  UseMethod("getParamSet")
} 

#'@S3method getParamSet Learner
getParamSet.Learner = function(learner) {
  learner$par.set
} 

#' Get current parameter settings for a learner. 
#' @param learner [\code{\link{Learner}}]\cr 
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

#' Set the hyperparameters of learner objects.
#' 
#' @param learner [\code{\link{Learner}}] \cr
#'   Learner.
#' @param ... [any] \cr
#'   Optional named (hyper)parameters. Alternatively these can be given
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}] \cr
#'    Optional list of named (hyper)parameters. The arguments in
#'    \code{...} take precedence over values in this list. We strongly
#'    encourage you to use one or the other to pass (hyper)parameters
#'    to the learner but not both.
#' @return \code{\link{Learner}} with changed hyperparameters.
#' @export
#' @seealso See \code{\link{getHyperPars}} for a function to retrieve
#'   the currently set hyper parameters. To get a list of all (hyper)parameters of
#'   a learner, see the \code{par.set} slot of the \code{\link{Learner}}
#'   object.
#' @examples
#' cl1 <- makeLearner("classif.ksvm", sigma=1)
#' cl2 <- setHyperPars(cl1, sigma=10, par.vals=list(C=2))
#' print(cl1)
#' # note the now set and altered hyperparameters:
#' print(cl2)
setHyperPars = function(learner, ..., par.vals) {
  UseMethod("setHyperPars")
} 

#'@S3method setHyperPars Learner
setHyperPars.Learner = function(learner, ..., par.vals) {
  ns = names(par.vals)
  pars = learner$par.set$pars
  for (i in seq(length=length(par.vals))) {
    n = ns[i]
    p = par.vals[[i]]
    pd = pars[[n]]
    if (is.null(pd)) {
      # no description: stop warn or quiet
      msg = paste(class(learner), ": Setting par ", n, " without description!", sep="")
      if (.mlr.conf$errorhandler.setup$on.par.without.desc == "stop")
        stop(msg)
      if (.mlr.conf$errorhandler.setup$on.par.without.desc == "warn")
        warning(msg)
      learner$par.set$pars[[n]] = makeUntypedLearnerParam(id=n)
      learner$par.vals[[n]] = p
    } else {
      isf = isFeasible(pd, p)
      if (length(isf) != 1 || !isf)
        stop("'", n, "' must be a feasible parameter setting.")
      # if valname of discrete par was used, transform it to real value
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

