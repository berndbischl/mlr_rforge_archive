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
  
  crossover = function(x, y) {
    mapply(function(a,b) sample(c(a,b), 1), x, y)
  }
  
  mutate <- function(x) {
    mut = sample(0:1, n, replace=TRUE, prob=c(prob.mut, 1-prob.mut))
    x + mut %% 2    
  }  
  
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
  
  #or = smsVarselGA(f, n, control=ga.control)
  opt.path = makeOptPathDFFromMeasures(bit.names, measures)

  mu = control@mu
  gen = 0L
  print(mu)
  prob.init = 0.5
  prob.mut = 0.5
  states = lapply(1:mu, function(i) rbinom(length(bit.names), 1, prob.init))
  eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states, dob=gen)
  active = 1:mu    ## Indices of individuals that are in the current pop.
  
  # check that ebuff evals are possible if lambda > 1
  
  while(getLength(opt.path) < control@maxit) {
    gen = gen + 1L
    ## Variation:
    parents = sample(active, 2)
    p1 = unlist(getPathElement(opt.path, parents[1])$x)
    p2 = unlist(getPathElement(opt.path, parents[2])$x)
    child = crossover(p1, p2)
    child = mutate(child)
    states = list(child)
    eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states, dob=gen)
    
    active = c(active, getLength(opt.path))
    print(active)
    Y = as.data.frame(opt.path)[, opt.path@y.names]
    Y = t(as.matrix(Y[active,]))
    ## Selection:
    i = nds_hv_selection(Y)
    print(i)
    
    ## Remove the i-th active individual:
    opt.path@env$eol[active[i]] = gen  
    active = active[-i]
  }
  return(opt.path)
}

