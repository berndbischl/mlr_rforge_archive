library("emoa")
source("http://git.datensplitter.net/cgit/emoa/plain/examples/sms_emoa.r")

tunemco = function(learner, task, resampling, measures, control) {
  n = length(measures)
  if (n <= 1)
    stop("Need at least two measures for MCO!")
  sapply(measures, function(m) if (length(m["aggr"]) != 1) stop("Please set only one aggr. function for: ", m["id"]))
  ms.names = sapply(measures, function(m) m["id"])
  # multiply max. measures with -1
  sig = sapply(measures, function(m) ifelse(m["minimize"],1,-1))
  ns = names(control["lower"])
  
  f = function(x) {
    if (any(is.na(x)))
      return(rep(NA, n))
    names(x) = ns
    learner = set.hyper.pars(learner, par.vals=as.list(x))
    r = resample(learner, task, resampling, measures=measures)
    r$aggr * sig
  }

  or = sms_emoa(f, lower=control["lower"], upper=control["upper"], control=control@extra.args)

  # repair sign of max. measures in result
  or$Y = diag(sig) %*% or$Y 
  or$value = diag(sig) %*% or$value
  
  rownames(or$value) = rownames(or$Y) = ms.names
  return(or)
}

library(mlbench)
data(Sonar)
ct = make.task(target="Class", data=Sonar)
res = make.res.desc("holdout")
m1 = tpr
m1@aggr = list(test.mean)
m2 = fpr
m2@aggr = list(test.mean)
wl = make.learner("classif.ksvm", predict.type="prob")
pds = list()
ctrl = cmaes.control(start=c(C=1, sigma=1), lower=c(C=0, sigma=0), upper=c(C=1000, sigma=1000),
  mu=5, maxeval=2000)
tr = tunemco(wl, ct, res, measures=list(m1, m2), control=ctrl)
myplot(tr)


