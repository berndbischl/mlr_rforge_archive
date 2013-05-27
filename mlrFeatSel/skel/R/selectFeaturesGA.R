#' selectFeaturesGA
#' 
#' A genetic algorithm for feature selection. Note that this is a (mu, lambda)-algorithm, i.e. it iteratively selects
#' the lambda fittest elements out of a population consisting of mu parents.
#' 
#' @param control [\code{control}]\cr 
#'   A control object, which includes the maximal number of iterations (\dQuote{maxit}), the rates for mutation
#'   (\dQuote{mutation.rate}) and crossover combinations (\dQuote{crossover.rate}, i.e. the probability of choosing an element
#'   from the first parent), the size of the parent population (\dQuote{mu}) and the number of childrens to be selected
#'   (\dQuote{lambda}).
#' @return [\code{\link{selectFeaturesGA}}].
#' @name selectFeaturesGA
#' @rdname selectFeaturesGA
#' @aliases selectFeaturesGA
NULL

selectFeaturesGA = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  fit = mlr:::measureAggrName(measures[[1]])
  states = lapply(1:control$extra.args$mu, 
	  function(i) rbinom(length(bit.names), 1, 0.5))
  if(!is.na(control$max.features)){
    foo = function(i){
      while(sum(i) > control$max.features) {
        i = rbinom(length(bit.names), 1, 0.5)
      }
      return(i)
    }
    states = lapply(states, function(z) foo(z))
  }
  evalOptimizationStates(learner, task, resampling, measures, 
	  bits.to.features, control, opt.path, show.info, states, 0L, as.integer(NA))  
  if("mutation.rate" %in% names(control)) {
    if(!("crossover.rate" %in% names(control))) {
      control$extra.args$crossover.rate = 1
    }
  } else {
    control$extra.args$mutation.rate = 0
  }
  for(i in 1:control$maxit) {
    parents = as.data.frame(getOptPathEl(opt.path, which(is.na(as.data.frame(opt.path)[,"eol"])))$x)
    kids = lapply(1:control$extra.args$lambda, function(z) 
          newStates(parents = parents[sample(1:nrow(parents), 2),], 
                       crossover.rate = control$extra.args$crossover.rate, mutation.rate = control$extra.args$mutation.rate, 
                       max.features = control$max.features))
    evalOptimizationStates(learner, task, resampling, measures, 
                           bits.to.features, control, opt.path, show.info, states = kids, i, as.integer(NA))
    index = order(as.data.frame(opt.path)[[fit]])[1:control$extra.args$mu]
    setOptPathElEOL(opt.path, setdiff(which(is.na(as.data.frame(opt.path)[,"eol"])), index), i)
  }
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, opt.path)
}


newStates = function(parents, crossover.rate, mutation.rate, max.features){
  if(is.na(max.features)) {
    kid = crossover(as.integer(parents[1,]), as.integer(parents[2,]), crossover.rate)
    return(mutateBits(kid, mutation.rate))    
  }
  run.loop = TRUE
  while(run.loop) {
    kid = crossover(as.integer(parents[1,]), as.integer(parents[2,]), crossover.rate)
    kid = mutateBits(kid, mutation.rate)
    run.loop = (sum(kid) > max.features)
  }
  return(kid)
}