library("emoa")
source("D:\\sync\\projekte\\mlr\\src\\base\\optimize\\smsga.r")

#nr of measures should select criteria
varselmco = function(learner, task, resampling, measures, control) {
  n = length(measures)
  if (n <= 1)
    stop("Need at least two measures for MCO!")
  sapply(measures, function(m) if (length(m["aggr"]) != 1) stop("Please set only one aggr. function for: ", m["id"]))
  ms.names = sapply(measures, function(m) m["id"])
  all.vars = task["input.names"]
  # multiply max. measures with -1
  sig = sapply(measures, function(m) ifelse(m["minimize"],1,-1))
  
  f = function(x) {
    if (any(is.na(x)))
      return(rep(NA, n))
    vars = all.vars[as.logical(x)]
    r = resample(learner, task, resampling, measures=measures, vars=vars)
    r$aggr * sig
  }
  
  or = sms_ga(f, length(all.vars), control=list(maxeval=control$maxeval,
      mu=control$mu,
      crossover=single_point_binary_crossover,
      mutate=ubm_operator(1/10)
  ))
  # repair sign of max. measures in result
  or$Y = diag(sig) %*% or$Y 
  or$value = diag(sig) %*% or$value
  
  rownames(or$value) = rownames(or$Y) = ms.names
  p = or$par
  or$vars = lapply(1:ncol(p), function(i) all.vars[as.logical(p[,i])])
  return(or)
}

active_population <- function(res, fe) {
  sel <- res$dob <= fe & (res$eol > fe | res$eol < 0)
  list(par=res$X[, sel],
    value=res$Y[, sel],
    hypeval=res$hypeval[fe]
  )
}

myplot = function(vr, lim) {
  n = nrow(vr$Y)
  y = t(vr$value)
  if (n == 2)
    plot(y)
  else {
    par(mfrow=c(n,1))
    for (i in 1:n) {
      plot(y[,i], type="l", ylab=colnames(y)[i])
      points(y[,i])
      abline(h=lim[i])
    }
  }
}

library(mlbench)
data(Sonar)
ct = make.task(target="Class", data=Sonar)
res = make.res.desc("holdout")
m1 = mmce
m1@aggr = list(test.mean)
m2 = nvars
m2@aggr = list(test.mean)
m3 = auc
m3@aggr = list(test.mean)
ctrl = list(mu=10, maxeval=500)
wl = make.learner("classif.lda", predict.type="prob")
#vr1 = varselmco(wl, ct, res, measures=list(m1, m2), control=ctrl)
#vr2 = varselmco(wl, ct, res, measures=list(m1, m2, m3), control=ctrl)
myplot(vr2, lim=c(0.2, 20, -0.9))



