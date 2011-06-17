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

varselMCO = function(learners, task, resampling, measures, bit.names, bits.to.features, control, measure.max.vals=NULL, multi.starts=1, par.sets) {
  bag = makeLearnerBag(learners)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  if (length(measures) < 2)
    stop("Please pass ate least 2 measures for MCO!")
  sapply(measures, function(m) if (length(m@aggr) != 1) 
        stop("Please set only one aggr. function for: ", m@id))
  if (missing(bit.names))
    bit.names = getFeatureNames(task)
  if (missing(bits.to.features))
    bits.to.features = function(x, task) binary.to.vars(x, getFeatureNames(task)) 
  
  ops = replicate(multi.starts,  simplify=FALSE, varselMCO2(bag, task, resampling, measures, 
      bit.names, bits.to.features, control, measure.max.vals, par.sets))
}

varselMCO2 = function(bag, task, resampling, measures, bit.names, bits.to.features, control, measure.max.vals=NULL, par.sets) {
  learner.ns = names(bag@learners)
  
  
  initPopulation = function(learner.ns, pars.sets, bits, control) {
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
  
  crossover = function(x, y, control) {
    print(x)
    z = list()
    z$learner = sample(c(x$learner, y$learner), 1)
    p = c(control@prob.cx, 1-control@prob.cx)
    z$bits = mapply(function(a,b) sample(c(a,b), 1, prob=p), x$bits, y$bits)
    return(z)
  }
  
  mutate = function(x, learner.ns, par.sets, control) {
    if (runif(1) < control@prob.mut.learner) {
      x$learner = sample(learner.ns, 1)
      x$hyper.pars = sampleHyperPars(par.sets[[x$learner]])
    } else {
      x = mutate.hp(x, control)
    }
    mut = sample(0:1, n, replace=TRUE, prob=c(1-control@prob.mut.bit, control@prob.mut.bit))
    x$bits = x$bits + mut %% 2
    return(x)
  }  
  
  mutate.hp = function(x) {
    ps = par.sets[[x$learner]]
    print(ps)
    low = lower(ps)
    upp = upper(ps)
    ns = names(x$hyper.pars)
    for (i in seq_len(x$hyper.pars)) {
      pm = pm_operator(mut.hp.eta, mut.hp.prob, lower=low[ns[i]], upper=upp[ns[i]])
      x$hyper.pars[i] = pm(x$hyper.pars[i])
    }
    return(x)
  }
  
  
  n = length(bit.names)
  m = length(measures)
  
  print(c(n, m))
  # td: scale all measures to 0,1 and always use ref(1,...,1)  
  # td: put this in varselMOCControl
  #todo: check that "learner" is not in bit.names
  opt.path = makeOptPathDFFromMeasures(c("learner", bit.names), measures)
  
  mu = control@mu
  gen = 0L
  prob.init = control@prob.init
  prob.mut.learner = control@prob.mut.learner
  prob.mut.bit = control@prob.mut.bit
  mut.hp.eta = control@mut.hp.eta
  mut.hp.prob = control@mut.hp.prob
  prob.cx = control@prob.cx
  pop = initPopulation(learner.ns, par.sets, n, control)
  my.eval.states(bag, task, resampling, measures, NULL, bits.to.features, control, opt.path, pop, dob=gen)
  # Indices of individuals that are in the current pop.
  active = 1:mu    
  print(pop)
  while(getLength(opt.path) < control@maxit) {
    gen = gen + 1L
    ## Variation:
    parents = sample(active, 2)
    p1a = getPathElement(opt.path, parents[1])$x
    p2a = getPathElement(opt.path, parents[2])$x
    p1 = list()
    p2 = list()
    p1$learner = p1a$learner    
    p2$learner = p2a$learner
    p1$bits = unlist(p1a[-1])
    p2$bits = unlist(p2a[-1])
    child = crossover(p1, p2, control)
    print(child)
    child = mutate(child)
    states = list(child)
    my.eval.states(bag, task, resampling, measures, NULL, bits.to.features, control, opt.path, states, dob=gen)
    
    active = c(active, getLength(opt.path))
    Y = as.data.frame(opt.path)[, opt.path@y.names]
    Y = t(as.matrix(Y[active,]))
    # todo check if names are correct
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

my.eval.states = function(bag, task, resampling, measures, par.set, bits.to.features, control, opt.path, pars, 
  eol=as.integer(NA), dob=as.integer(NA)) {
  
  fun = function(bag, task, resampling, measures, par.set, bits.to.features, control, val) {
    bag = setHyperPars(bag, sel.learner=val$learner)
    task = subset(task, vars=bits.to.features(val$bits, task))
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
  for (i in 1:n) 
    addPathElement(opt.path, x=as.list(pars[[i]]), y=y[[i]], dob=dob[i], eol=eol[i])
  return(y)
}


sampleHyperPars = function(par.set) {
  n = length(par.set@pars)
  z = numeric(n)
  for (i in seq_len(n)) {
    pd = par.set@pars[[i]]
    z[i] = runif(1, pd@constraints$lower, pd@constraints$upper)
  }
  names(z) = names(par.set@pars)
  return(z)
}

