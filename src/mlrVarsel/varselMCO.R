#' Optimizes the variables for a classification or regression problem by doing multi-criteria optimization.
#' Currently the SMS-EMOA is used to maximize the hypervolume (S-metric) with binary operators on 
#' the bit string representation.
#' 
#' The algorithm operates on a 0-1-bit encoding of candidate solutions. Per default a single bit corresponds
#' to a single feature, but you are able to change this by using the arguments \code{bit.names} 
#' and \code{bits.to.features}. Thus allowing you to switch on whole groups of features with a single bit.  
#' 
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'   Learning task.   
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate feature sets. If you pass a description, 
#'   it is instantiated once at the beginning by default, so all feature sets are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at the control object.  
#' @param control [see \code{\link{VarselControl}}]
#'   Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to optimize.   
#' @param bit.names [character]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.    
#' @param bits.to.features [function(x, task)]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features. 
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.      
#' @param log.fun [function(learner, task, resampling, measure, par.set, control, opt.path, x, y)]\cr
#'   Called after every hyperparameter evaluation. Default is to print performance via mlr logger. 
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#' @seealso \code{\link{makeVarselWrapper}} 
#' @title Variable selection.

varselMCO = function(learner, task, resampling, measures, control) {
  if (is.character(learner))
    learner <- makeLearner(learner)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  if (length(measures) < 2)
    stop("Please pass ate least 2 measures for MCO!")
  sapply(measures, function(m) if (length(m["aggr"]) != 1) 
    stop("Please set only one aggr. function for: ", m["id"]))
  if (missing(bits.to.features))
    bits.to.features = function(x, task) binary.to.vars(x, getFeatureNames(task)) 
  if (missing(log.fun))
    log.fun = log.fun.varsel
  
  n = length(measures)
  f = function(x) {
    if (any(is.na(x)))
      return(rep(NA, n+1))    
    y = lookup_cache(x)
    if (!is.null(y))
      return(y)
    else {  
      vars = bitstring_to_features(x)
      y = mlr:::eval.rf(learner, task, resampling, measures, control, par=vars)
      y = c(y, sum(x)/90)
      add_cache(x, y)
      return(y)
    }
  }
  
  or = sms_ga(f, 90, control=list(
      maxeval=control$maxeval,
      ref=c(1, 1, 1),
      logger=varsel_logger(message),               
      mu=control$mu,
      crossover=ubx_operator(0.25),
      mutate=ubm_operator(0.05))
  )
  rownames(or$value) = rownames(or$Y) = c(sapply(measures, function(m) m["id"]), "bits")
  p = or$par
  return(or)
}

