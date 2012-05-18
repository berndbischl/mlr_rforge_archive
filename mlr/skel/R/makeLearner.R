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
#' @param cl [\code{character(1)}]\cr
#'   Class of learner to create. By convention, all classification learners
#'   start with \dQuote{classif.} and all regression learners with
#'   \dQuote{regr.}. A list of all learners is available on the
#'   \code{\link{learners}} help page.
#' @param id [\code{character(1)}]\cr 
#'   Id string for object. Used to select the object from a named list, etc.
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} or \dQuote{prob}.
#'   Regression: \dQuote{response} or \dQuote{se}.
#'   Default is \dQuote{response}.
#' @param ... [any]\cr
#'   Optional named (hyper)parameters. Alternatively these can be given
#'   using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameters. The arguments in
#'   \code{...} take precedence over values in this list. We strongly
#'   encourage you to use one or the other to pass (hyper)parameters
#'   to the learner but not both.
#' @return [\code{\link{Learner}}].
#' @export
#' @example
#' makeLearner("classif.logreg")
#' makeLearner("regr.lm")
makeLearner = function(cl, id, predict.type="response", ..., par.vals=list()) {
  if (cl == "")
    stop("Cannot create learner from empty string!")	
  wl = do.call(sprintf("makeRLearner.%s", cl), list())
  if (!is(wl, "RLearner"))
    stop("Learner must be a basic RLearner!")
  if (!missing(id))
    wl$id = id
  pds = wl$par.set$pars
  # pass defaults
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
