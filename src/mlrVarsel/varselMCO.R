#todo: maxit is cirrently used fdor maxevals, bad name as "iter" is different concept as "eval",
# 2) sparse format for opt.path

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
#' @title Variable selection.

varselMCO = function(learners, task, resampling, measures, bit.names, bits.to.features, control, measure.max.vals=NULL, multi.starts=1, par.sets) {
  requirePackages("emoa", "varselMCO")
  bag = makeLearnerBag(learners)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    if (length(measures) < 2)
      stop("Please pass ate least 2 measures for MCO!")
  sapply(measures, function(m) if (length(m@aggr) != 1) 
        stop("Please set only one aggr. function for: ", m@id))
  if (missing(bit.names))
    bit.names = getFeatureNames(task)
  if (missing(bits.to.features))
    bits.to.features = function(x, task) binary.to.vars(x, getFeatureNames(task)) 
  if (!is.null(measure.max.vals) && !setequal(names(measure.max.vals), sapply(measures, function(x) x@id)))
    stop("Measure ids and names of 'measure.max.vals' sets must coincide!")
  learner.ns = names(bag@learners)
  if (!setequal(names(par.sets), learner.ns)) 
    stop("Learner ids and names of parameter sets must coincide!")
  ops = replicate(multi.starts,  simplify=FALSE, varselMCO2(bag, task, resampling, measures, 
      bit.names, bits.to.features, control, measure.max.vals, par.sets, learner.ns))
}


initPopulation = function(learner.ns, par.sets, bits, control) {
  pop = list()
  for (i in 1:control@mu) {
    learner = sample(learner.ns, 1)
    pop[[i]] = list (
      learner = learner,
      hyper.pars = sampleHyperPars(par.sets[[learner]]),
      bits = rbinom(bits, 1, control@prob.init)
    )
  }
  return(pop)
}


varselMCO2 = function(bag, task, resampling, measures, bit.names, bits.to.features, control, measure.max.vals=NULL, par.sets, learner.ns) {
  n = length(bit.names)
  m = length(measures)
  
  hps.ns = as.character(do.call(c, lapply(par.sets, function(ps) names(ps$pars))))
  if ("learner" %in% bit.names) 
    stop("'learner' can currently not be a bit/feature name!")
  if ("learner" %in% hps.ns) 
    stop("'learner' can currently not be a hyperparameter name in your ROI!")
  if (length(intersect(hps.ns, bit.names)) > 0) 
    stop("Hyperparameter names and bit/feature names overlap. Currently that is not supported!")
  if (any(duplicated(hps.ns)))
    stop("Hyperparameter names of learners overlap. Currently that is not supported!")
  
  opt.path = makeOptPathDFFromMeasures(c("learner", hps.ns, bit.names), measures)
  
  mu = control@mu
  gen = 0L
  prob.init = control@prob.init
  prob.mut.learner = control@prob.mut.learner
  prob.mut.bit = control@prob.mut.bit
  mut.hp.eta = control@mut.hp.eta
  mut.hp.prob = control@mut.hp.prob
  prob.cx = control@prob.cx
  pop = initPopulation(learner.ns, par.sets, n, control)
  my.eval.states(bag, task, resampling, measures, par.sets, bits.to.features, control, opt.path, pop, dob=gen, hps.ns=hps.ns)
  # Indices of individuals that are in the current pop.
  active = 1:mu    
  while(length(opt.path) < control@maxit) {
    print(gen)
    gen = gen + 1L
    ## Variation:
    parents = sample(active, 2)
    p1a = getOptPathEl(opt.path, parents[1])$x
    p2a = getOptPathEl(opt.path, parents[2])$x
    p1 = list()
    p2 = list()
    p1$learner = p1a$learner    
    p2$learner = p2a$learner
    p1$hyper.pars = p1a[names(par.sets[[p1$learner]]$pars)]
    p2$hyper.pars = p2a[names(par.sets[[p2$learner]]$pars)]
    mode(p1$hyper.pars) = "numeric"
    mode(p2$hyper.pars) = "numeric"
    p1$bits = unlist(p1a[bit.names])
    p2$bits = unlist(p2a[bit.names])
    child = crossover(p1, p2, par.sets, control)
    child = mutate(child, learner.ns, par.sets, control)
    states = list(child)
    my.eval.states(bag, task, resampling, measures, par.sets, bits.to.features, control, opt.path, states, dob=gen, hps.ns=hps.ns)
    
    active = c(active, length(opt.path))
    Y = as.data.frame(opt.path)[, opt.path@y.names]
    Y = t(as.matrix(Y[active,]))
    if (!is.null(measure.max.vals))
      Y[names(measure.max.vals),] = Y[names(measure.max.vals),] / measure.max.vals 
    ## Selection:
    i = nds_hv_selection(Y)
    
    ## Remove the i-th active individual:
    opt.path@env$eol[active[i]] = gen  
    active = active[-i]
  }
  return(opt.path)
}

#todo maybe add this to mlr?
my.eval.states = function(bag, task, resampling, measures, par.sets, bits.to.features, control, opt.path, pars, 
  eol=as.integer(NA), dob=as.integer(NA), hps.ns) {
  fun = function(bag, task, resampling, measures, par.set, bits.to.features, control, val) {
    hps = trafoVal(par.sets[[val$learner]], as.list(val$hyper.pars))
    # trafo hyper pars, integer conversion is done before
    bag@learners[[val$learner]] = setHyperPars(bag@learners[[val$learner]], par.vals=hps)
    # select learner and set hyper pars
    bag = setHyperPars(bag, sel.learner=val$learner)
    # select features 
    task = subsetData(task, vars=bits.to.features(val$bits, task))
    r = resample(bag, task, resampling, measures=measures)
    return(r$aggr)
  }
  
  
  y = mylapply(xs=pars, from="opt", f=fun, bag=bag, task=task, resampling=resampling, 
    measures=measures, par.set=par.set, bits.to.features=bits.to.features, control=control)
  n = length(pars)
  if (length(dob) == 1)
    dob = rep(dob, n)
  if (length(eol) == 1)
    eol = rep(eol, n)
  for (i in 1:n) {
    hps = rep(as.numeric(NA), length(hps.ns))
    names(hps) = hps.ns
    x = pars[[i]]
    hps[names(x$hyper.pars)] = x$hyper.pars
    x = c(x$learner, as.list(hps), as.list(x$bits))
    addOptPathEl(opt.path, x=x, y=y[[i]], dob=dob[i], eol=eol[i])
  }
  return(y)
}


