#todo: maxit is cirrently used fdor maxevals, bad name as "iter" is different concept as "eval",
# also maybe we want to construct the ga control object directly and simply reuse is?
# 2) sparse format for or$value?=
# 3) get dob and eol from olaf resul. how?

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

varselMCO = function(learner, task, resampling, measures, bit.names, bits.to.features, control) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  if (length(measures) < 2)
    stop("Please pass ate least 2 measures for MCO!")
  sapply(measures, function(m) if (length(m["aggr"]) != 1) 
    stop("Please set only one aggr. function for: ", m["id"]))
  if (missing(bit.names))
    bit.names = getFeatureNames(task)
  if (missing(bits.to.features))
    bits.to.features = function(x, task) binary.to.vars(x, getFeatureNames(task)) 
  
  n = length(bit.names)
  m = length(measures)
  print(c(n, m))
  # td: scale all measures to 0,1 and always use ref(1,...,1)  
  # td: put this in varselMOCControl
  
  ga.control = list(mu=control@mu, maxeval=control@maxit, 
    crossover=ubx_operator(control@cross.prob), mutate=ubm_operator(control@mut.prob))
  
  f = function(x) {
    if (any(is.na(x)))
      return(rep(NA, m))
    y = mlr:::eval.rf(learner, task, resampling, measures, NULL, bits.to.features, control, x)
    return(y)
  }
  
  or = smsVarselGA(f, n, control=ga.control)
  oo <<- or
  opt.path = makeOptPathDFFromMeasures(bit.names, measures)
  x = as.list(as.data.frame(or$X))
  y = as.list(as.data.frame(or$Y))
  pp = Map(function(a,b) list(x=a, y=b), x,y)
  print(str(pp))
  opt.path@env$path = pp 
  opt.path@env$dob = rep(NA, control@maxit) 
  opt.path@env$eol = rep(NA, control@maxit) 
  return(opt.path)
}

