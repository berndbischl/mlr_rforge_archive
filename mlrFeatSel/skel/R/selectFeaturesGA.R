#' selectFeaturesGA
#' 
#' A genetic algorithm for feature selection. Note that this is a (mu, lambda)-algorithm, i.e. it iteratively selects
#' the lambda fittest elements out of a population consisting of mu parents.
#' 
#' @param control [\code{control}]\cr 
#'   A control object, which includes the maximal number of iterations (\dQuote{maxit}), the rates for mutation
#'   (\dQuote{mutation}) and crossover combinations (\dQuote{crossoverRate}, i.e. the probability of choosing an element
#'   from the first parent), the size of the parent population (\dQuote{mu}) and the number of childrens to be selected
#'   (\dQuote{lambda}).
#' @return [\code{\link{selectFeaturesGA}}].
#' @name selectFeaturesGA
#' @rdname selectFeaturesGA
#' @aliases selectFeaturesGA
NULL

selectFeaturesGA = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  fit = mlr:::measureAggrName(measures[[1]])
  states = lapply(1:control$mu, 
	  function(i) rbinom(length(getTaskFeatureNames(task)), 1, 0.5))
  evalOptimizationStates(learner, task, resampling, measures, 
	  bits.to.features, control, opt.path, show.info, states, 0L, as.integer(NA))  
  
  if("mutationRate" %in% names(control)) {
    if(!("crossoverRate" %in% names(control))) {
      control$crossoverRate = 1
    }
  } else {
    control$mutationRate = 0
  }
  
  for(i in 1:control$maxit) {
    parents = as.data.frame(getOptPathEl(opt.path, which(is.na(as.data.frame(opt.path)[,"eol"])))$x)
    kids = lapply(1:control$lambda, function(z) 
          generateKids(parents = parents[sample(1:nrow(parents), 2),], 
                       crossoverRate = control$crossoverRate, mutationRate = control$mutationRate))
    evalOptimizationStates(learner, task, resampling, measures, 
                           bits.to.features, control, opt.path, show.info, states = kids, i, as.integer(NA))
    index = order(as.data.frame(opt.path)[[fit]])[1:control$mu]
    setOptPathElEOL(opt.path, setdiff(which(is.na(as.data.frame(opt.path)[,"eol"])), index), i)
  }
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
	makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, opt.path)
}


generateKids = function(parents, crossoverRate, mutationRate){
  kid = crossover(as.integer(parents[1,]), as.integer(parents[2,]), crossoverRate)
  return(mutateBits(kid, mutationRate))
}